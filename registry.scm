(define-module (registry)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:export (load-ui-dir load-all-uis get-watched-files))

;; Lista de (filepath . builder-proc) — atualizada atomicamente
(define *components* '())

;; Converte caminho de arquivo para nome de módulo simbólico.
;; Ex: "components/ui.scm" -> (components ui)
(define (filepath->module-name filepath)
  (let* ((no-ext   (string-drop-right filepath 4))
         (parts    (string-split no-ext #\/))
         (filtered (filter (lambda (s) (not (string=? s "."))) parts)))
    (map string->symbol filtered)))

;; Tenta carregar/recarregar o módulo via sistema de módulos do Guile
;; e retorna o proc build-ui, ou #f em caso de falha.
(define (load-component filepath)
  (catch #t
    (lambda ()
      (let* ((mod-name (filepath->module-name filepath))
             (mod      (resolve-module mod-name #f #:ensure #f)))
        ;; Se o módulo já existe, força recarga; caso contrário carrega pela primeira vez
        (if mod
            (reload-module mod)
            (load filepath))
        ;; Após carregado, resolve a interface pública e extrai build-ui
        (let ((iface (resolve-interface mod-name)))
          (or (module-ref iface 'build-ui #f)
              (begin
                (format (current-error-port)
                        "AVISO: ~a não exporta build-ui\n" mod-name)
                #f)))))
    (lambda (key . args)
      (format (current-error-port)
              "ERRO ao carregar ~a: ~a ~s\n" filepath key args)
      #f)))

;; Varre dir por arquivos .scm, monta nova lista localmente
;; e só substitui *components* ao final — atualização atômica.
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
        (set! *components* new-entries))))

;; Chama todos os builders registrados passando a janela.
(define (load-all-uis win)
  (for-each (lambda (entry) ((cdr entry) win))
            *components*))

;; Retorna lista de caminhos monitorados.
(define (get-watched-files)
  (map car *components*))