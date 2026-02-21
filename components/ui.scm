(define-module (components ui)
  #:use-module (g-golf)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (build-ui))

;; --- CSS Provider singleton -------------------------------------------

(define *css-provider*    #f)
(define *styles-applied?* #f)

(define %css
  ".log-text {
      font-family: 'Monospace', monospace;
      color: #1901f2;
      padding: 15px;
   }
   .scrolled-frame {
      border: 1px solid #333;
      border-radius: 8px;
      margin: 15px;
   }
   .header-label {
      font-weight: bold;
      margin: 15px 15px 5px 20px;
   }")

(define (apply-styles!)
  (unless *styles-applied?*
    (set! *css-provider* (make <gtk-css-provider>))
    (gtk-css-provider-load-from-string *css-provider* %css)
    (gtk-style-context-add-provider-for-display
      (gdk-display-get-default)
      *css-provider*
      600)
    (set! *styles-applied?* #t)))

;; --- Coleta de dados do sistema ---------------------------------------

;; Executa cmd em shell e retorna a primeira linha da saída,
;; ou "N/A" em caso de falha. ATENÇÃO: cmd deve ser string literal
;; confiável — não interpolar entrada do usuário aqui.
(define (exec cmd)
  (catch #t
    (lambda ()
      (let* ((port (open-input-pipe (string-append "/bin/sh -c \"" cmd " 2>/dev/null\"")))
             (line (read-line port)))
        (close-pipe port)
        (if (or (eof-object? line) (string=? line "")) "N/A" line)))
    (lambda (key . args) "N/A")))

(define (collect-system-info)
  (let* ((session (or (getenv "XDG_SESSION_TYPE") "Não detectado"))
         (backend (cond
                    ((string-contains (string-downcase session) "wayland") "Wayland Backend")
                    ((string-contains (string-downcase session) "x11")     "X11/Xorg Backend")
                    (else                                                   "Desconhecido")))
         (gpu     (exec "/usr/bin/lspci | grep -i vga | cut -d ':' -f3")))
    (list
      "=== VERIFICAÇÃO DE DISPLAY SERVER ==="
      (string-append "Sessão atual: " (string-upcase session))
      (string-append "Backend GTK: "  backend)
      "--------------------------------"
      (string-append "Kernel: "   (exec "uname -r"))
      (string-append "Hostname: " (exec "hostname"))
      "--------------------------------"
      (string-append "GPU: " (if (string=? gpu "N/A")
                                 "Hardware info via lspci indisponível"
                                 (string-trim-both gpu)))
      "--------------------------------"
      "Fim diagnóstico.")))

;; --- Renderização -----------------------------------------------------

(define (make-logger buffer text-view)
  (lambda (msg)
    (gtk-text-buffer-insert-at-cursor buffer (string-append "> " msg "\n") -1)
    (gtk-text-view-scroll-mark-onscreen
      text-view
      (gtk-text-buffer-get-insert buffer))))

(define (render-log! buffer text-view entries)
  (let ((log! (make-logger buffer text-view)))
    (for-each log! entries)))

;; --- Ponto de entrada ------------------------------------------------

(define (build-ui win)
  (apply-styles!)
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

    (render-log! buffer text-view (collect-system-info))
    (present win)))