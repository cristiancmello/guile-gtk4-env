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

;; Carrega (ou recarrega) um arquivo .scm e retorna o builder-proc,
;; ou #f se o módulo não exportar build-ui.
(define (load-component! filepath)
  (let* ((mod-name (filepath->module-name filepath))
         (module   (begin
                     (load filepath)
                     (resolve-module mod-name #f #:ensure #f))))
    (if module
        (module-ref module 'build-ui #f)
        (begin
          (format (current-error-port)
                  "AVISO: Módulo ~a não encontrado após load de ~a\n"
                  mod-name filepath)
          #f))))

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

    ;; Remove do cache arquivos que sumiram do diretório
    (hash-for-each
      (lambda (fp _)
        (unless (hash-ref file-set fp #f)
          (hash-remove! *ui-cache* fp)
          (set! *ui-order* (filter (lambda (x) (not (string=? x fp))) *ui-order*))))
      *ui-cache*)

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