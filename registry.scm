(define-module (registry)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 string-fun)
  #:export (load-ui-dir load-all-uis get-watched-files))

;; Estado interno: tabela hash de filepath -> (mtime . builder-proc)
;; Usamos hash-table em vez de lista para lookup O(1) por filepath.
(define *ui-cache* (make-hash-table))

;; Ordem de inserção preservada para garantir ordem estável de rebuild.
(define *ui-order* '())

(define (filepath->module-name filepath)
  (let* ((no-ext   (string-drop-right filepath 4))
         (parts    (string-split no-ext #\/))
         (filtered (filter (lambda (s) (not (string=? s "."))) parts)))
    (map string->symbol filtered)))

;; Retorna o mtime do arquivo em segundos, ou -1 se não acessível.
(define (file-mtime filepath)
  (catch #t
    (lambda ()
      (let ((st (stat filepath)))
        (stat:mtime st)))
    (lambda (key . args) -1)))

;; Invalida o cache interno do Guile para o módulo, se ele já existir.
;; Necessário para que um `load` subsequente realmente reexecute o arquivo
;; em vez de reutilizar a versão compilada em memória.
;;
;; Em Guile 3.0, `reload-module` recarrega e reavalia o módulo a partir
;; do source. Aqui usamos apenas para *invalidar* — o `load` abaixo
;; é quem faz a leitura do disco, garantindo que o filepath correto
;; (e não o caminho inferido pelo sistema de módulos) seja usado.
(define (invalidate-module-cache! mod-name)
  (let ((existing (resolve-module mod-name #f #:ensure #f)))
    (when existing
      (catch #t
        (lambda () (reload-module existing))
        (lambda (key . args)
          (format (current-error-port)
                  "AVISO: reload-module falhou para ~a: ~a ~s\n"
                  mod-name key args))))))

;; Carrega (ou recarrega) um arquivo .scm e retorna o builder-proc,
;; ou #f se o módulo não exportar build-ui.
;;
;; Fluxo:
;;   1. Invalida o módulo no cache do Guile (se já existia).
;;   2. `load` reavalia o arquivo do disco.
;;   3. `resolve-module` agora devolve a versão recém-avaliada.
(define (load-component! filepath)
  (let ((mod-name (filepath->module-name filepath)))
    (invalidate-module-cache! mod-name)
    (load filepath)
    (let ((module (resolve-module mod-name #f #:ensure #f)))
      (if module
          (module-ref module 'build-ui #f)
          (begin
            (format (current-error-port)
                    "AVISO: Módulo ~a não encontrado após load de ~a\n"
                    mod-name filepath)
            #f)))))

;; Varre o diretório procurando arquivos .scm.
;; - Arquivos novos: carrega e adiciona ao cache.
;; - Arquivos existentes com mtime diferente: recarrega.
;; - Arquivos existentes com mtime igual: mantém builder em cache, sem load.
;; - Arquivos removidos: descarta do cache.
(define (load-ui-dir dir)
  (let* ((entries  (scandir dir (lambda (f) (string-suffix? ".scm" f))))
         (files    (if entries
                       (map (lambda (f) (string-append dir "/" f)) entries)
                       '()))
         (file-set (make-hash-table)))

    ;; Marca quais arquivos ainda existem no diretório
    (for-each (lambda (fp) (hash-set! file-set fp #t)) files)

    ;; Coleta primeiro os arquivos que sumiram do diretório,
    ;; sem mutar o hash durante a iteração (comportamento indefinido).
    (let ((to-remove
           (hash-fold (lambda (fp _ acc)
                        (if (hash-ref file-set fp #f)
                            acc
                            (cons fp acc)))
                      '()
                      *ui-cache*)))
      (for-each (lambda (fp)
                  (hash-remove! *ui-cache* fp)
                  (set! *ui-order*
                        (filter (lambda (x) (not (string=? x fp))) *ui-order*)))
                to-remove))

    ;; Para cada arquivo presente, decide se precisa recarregar
    (for-each
      (lambda (filepath)
        (let* ((mtime   (file-mtime filepath))
               (cached  (hash-ref *ui-cache* filepath #f))
               (changed? (or (not cached)
                             (not (= mtime (car cached))))))
          (when changed?
            (let ((builder (load-component! filepath)))
              (when builder
                ;; Atualiza cache com novo mtime e builder
                (hash-set! *ui-cache* filepath (cons mtime builder))
                ;; Adiciona à ordem só se for arquivo novo
                (unless cached
                  (set! *ui-order* (append *ui-order* (list filepath)))))
              (unless builder
                ;; Load falhou — remove do cache para tentar novamente no próximo ciclo
                (hash-remove! *ui-cache* filepath)
                (set! *ui-order* (filter (lambda (x) (not (string=? x filepath)))
                                         *ui-order*)))))))
      files)))

;; Chama todos os builders na ordem de inserção.
(define (load-all-uis win)
  (for-each
    (lambda (filepath)
      (let ((entry (hash-ref *ui-cache* filepath #f)))
        (when entry
          ((cdr entry) win))))
    *ui-order*))

;; Retorna apenas os arquivos com builder válido no cache.
(define (get-watched-files)
  (filter (lambda (fp) (hash-ref *ui-cache* fp #f))
          *ui-order*))