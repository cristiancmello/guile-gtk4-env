(define-module (registry)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:export (load-ui-dir load-all-uis get-watched-files))

;; Lista de (filepath . builder-proc) — atualizada atomicamente
(define *entries* '())

;; Converte caminho de arquivo para nome de módulo simbólico.
;; Ex: "components/ui.scm" -> (components ui)
(define (filepath->module-name filepath)
  (let* ((no-ext   (string-drop-right filepath 4))
         (parts    (string-split no-ext #\/))
         (filtered (filter (lambda (s) (not (string=? s "."))) parts)))
    (map string->symbol filtered)))

;; Tenta carregar filepath como módulo e retorna o proc build-ui,
;; ou #f em caso de falha — sem lançar exceção.
(define (load-component filepath)
  (catch #t
    (lambda ()
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
    (lambda (key . args)
      (format (current-error-port)
              "ERRO ao carregar ~a: ~a ~s\n" filepath key args)
      #f)))

;; Varre dir por arquivos .scm, monta nova lista localmente
;; e só substitui *entries* ao final — atualização atômica.
(define (load-ui-dir dir)
  (let* ((files   (or (scandir dir (lambda (f) (string-suffix? ".scm" f))) '()))
         (new-entries
           (filter-map
             (lambda (filename)
               (let* ((filepath (string-append dir "/" filename))
                      (builder  (load-component filepath)))
                 (if builder
                     (cons filepath builder)
                     #f)))
             files)))
    (if (null? new-entries)
        (format (current-error-port)
                "AVISO: Nenhum componente carregado de ~a\n" dir)
        (set! *entries* new-entries))))

;; Chama todos os builders registrados passando a janela.
(define (load-all-uis win)
  (for-each (lambda (entry) ((cdr entry) win))
            *entries*))

;; Retorna lista de caminhos monitorados.
(define (get-watched-files)
  (map car *entries*))