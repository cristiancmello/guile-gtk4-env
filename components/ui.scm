(define-module (components ui)
  #:use-module (g-golf)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (build-ui))

(define *css-provider* #f)

(define (build-ui win css-path)
  (apply-styles! css-path)
  (let* ((main-box    (make <gtk-box> #:orientation 'vertical))
         (label-title (make <gtk-label> #:label "MONITOR DE AMBIENTE GRÁFICO"))
         (scrolled    (make <gtk-scrolled-window>
                            #:min-content-height 400
                            #:vexpand #t))
         (text-view   (make <gtk-text-view> #:editable #f #:cursor-visible #f))
         (buffer      (slot-ref text-view 'buffer)))

    (gtk-widget-add-css-class label-title "header-label")
    (gtk-widget-add-css-class text-view   "log-text")
    (gtk-widget-add-css-class scrolled    "scrolled-frame")

    (set-child win main-box)
    (append main-box label-title)
    (set-child scrolled text-view)
    (append main-box scrolled)

    (populate-log! buffer text-view)
    (present win)))

(define (apply-styles! css-path)
  (let ((display (gdk-display-get-default)))
    (when *css-provider*
      (catch #t
        (lambda ()
          (gtk-style-context-remove-provider-for-display display *css-provider*))
        (lambda (key . args)
          (format (current-error-port)
                  "AVISO: Falha ao remover CSS provider anterior: ~a ~s\n"
                  key args))))

    (let ((provider (make <gtk-css-provider>)))
      (when css-path
        (catch #t
          (lambda ()
            (gtk-css-provider-load-from-path provider css-path))
          (lambda (key . args)
            (format (current-error-port)
                    "AVISO: Falha ao carregar ~a: ~a ~s\n"
                    css-path key args))))
      (gtk-style-context-add-provider-for-display display provider 600)
      (set! *css-provider* provider))))

(define (make-logger buffer text-view)
  (lambda (msg)
    (gtk-text-buffer-insert-at-cursor buffer (string-append "> " msg "\n") -1)
    (gtk-text-view-scroll-mark-onscreen
      text-view
      (gtk-text-buffer-get-insert buffer))))

(define (exec cmd)
  (catch #t
    (lambda ()
      (let* ((port (open-input-pipe
                     (string-append "/bin/sh -c \"" cmd " 2>/dev/null\"")))
             (line (read-line port)))
        (close-pipe port)
        (if (or (eof-object? line) (string=? line "")) "N/A" line)))
    (lambda (key . args) "N/A")))

(define (populate-log! buffer text-view)
  (let ((log! (make-logger buffer text-view)))

    (log! "=== VERIFICAÇÃO DE DISPLAY SERVER ===")

    (let* ((session (or (getenv "XDG_SESSION_TYPE") "Não detectado"))
           (backend (cond
                      ((string-contains (string-downcase session) "wayland")
                       "Wayland Backend")
                      ((string-contains (string-downcase session) "x11")
                       "X11/Xorg Backend")
                      (else "Desconhecido"))))
      (log! (string-append "Sessão atual: " (string-upcase session)))
      (log! (string-append "Backend GTK: "  backend)))

    (log! "--------------------------------")
    (log! (format #f "Kernel: ~a"   (exec "uname -r")))
    (log! (format #f "Hostname: ~a" (exec "hostname")))
    (log! "--------------------------------")

    (let ((gpu (exec "/usr/bin/lspci | grep -i vga | cut -d ':' -f3")))
      (log! (string-append "GPU: "
                           (if (string=? gpu "N/A")
                               "Hardware info via lspci indisponível"
                               (string-trim-both gpu)))))

    (log! "--------------------------------")
    (log! "Fim do diagnóstico.")))