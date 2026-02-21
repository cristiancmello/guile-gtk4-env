(define-module (registry)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 string-fun)
  #:export (load-ui-dir load-all-uis get-watched-files))

;; Estado interno: lista de (filepath . builder-proc)
(define *ui-entries* '())

;; Converte um caminho de arquivo para um nome de módulo simbólico.
;; Ex: "components/ui.scm" -> (components ui)
(define (filepath->module-name filepath)
  (let* ((no-ext   (string-drop-right filepath 4))          ;; remove ".scm"
         (parts    (string-split no-ext #\/))               ;; split por "/"
         (filtered (filter (lambda (s) (not (string=? s ".")))
                           parts)))
    (map string->symbol filtered)))

;; Carrega um arquivo .scm como módulo e retorna o proc build-ui exportado,
;; ou #f se o módulo não exportar build-ui.
(define (load-component! filepath)
  (let* ((mod-name (filepath->module-name filepath))
         ;; Re-carrega o arquivo forçando reavaliação
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

;; Varre o diretório dir procurando arquivos .scm, carrega cada um como
;; módulo e coleta os builders e caminhos assistidos.
(define (load-ui-dir dir)
  (set! *ui-entries* '())
  (let ((entries (scandir dir (lambda (f) (string-suffix? ".scm" f)))))
    (when entries
      (for-each
        (lambda (filename)
          (let* ((filepath (string-append dir "/" filename))
                 (builder  (load-component! filepath)))
            (when builder
              (set! *ui-entries*
                    (append *ui-entries* (list (cons filepath builder)))))))
        entries))))

;; Chama todos os builders registrados passando a janela.
(define (load-all-uis win)
  (for-each (lambda (entry) ((cdr entry) win))
            *ui-entries*))

;; Retorna a lista de caminhos de arquivo monitorados.
(define (get-watched-files)
  (map car *ui-entries*))