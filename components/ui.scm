(use-modules 
  (ice-9 popen)
  (ice-9 rdelim))
             
(define (build-ui win)
  (let ((provider (make <gtk-css-provider>))
        (display (gdk-display-get-default)))
    (gtk-css-provider-load-from-string provider 
      "window { background-color: #121212; }
       .log-text { 
          font-family: 'Monospace', monospace; 
          color: #2ecc71; 
          background: #000; 
          padding: 15px; 
       }
       .scrolled-frame {
          border: 1px solid #333;
          border-radius: 8px;
          margin: 15px;
       }
       .header-label {
          color: #7f8c8d;
          font-weight: bold;
          margin: 15px 15px 5px 20px;
       }")
    (gtk-style-context-add-provider-for-display display provider 600))

  (let* ((main-box    (make <gtk-box> #:orientation 'vertical))
         (label-title (make <gtk-label> #:label "MONITOR DE AMBIENTE GRÁFICO"))
         (scrolled    (make <gtk-scrolled-window> #:min-content-height 400 #:vexpand #t))
         (text-view   (make <gtk-text-view> #:editable #f #:cursor-visible #f))
         (buffer      (slot-ref text-view 'buffer)))

    (gtk-widget-add-css-class label-title "header-label")
    (gtk-widget-add-css-class text-view   "log-text")
    (gtk-widget-add-css-class scrolled    "scrolled-frame")

    (set-child win main-box)
    (append main-box label-title)
    (set-child scrolled text-view)
    (append main-box scrolled)

    (define (log-info msg)
      (gtk-text-buffer-insert-at-cursor buffer (string-append "> " msg "\n") -1)
      (let ((mark (gtk-text-buffer-get-insert buffer)))
        (gtk-text-view-scroll-mark-onscreen text-view mark)))

    (define (exec cmd)
      (catch #t
        (lambda ()
          (let* ((port (open-input-pipe
                         (string-append "/bin/sh -c \"" cmd " 2>/dev/null\"")))
                 (line (read-line port)))
            (close-pipe port)
            (if (or (eof-object? line) (string=? line "")) "N/A" line)))
        (lambda (key . args) "N/A")))

    (log-info "=== VERIFICAÇÃO DE DISPLAY SERVER ===")

    (let* ((session (or (getenv "XDG_SESSION_TYPE") "Não detectado"))
           (backend (cond
                      ((string-contains (string-downcase session) "wayland") "Wayland Backend")
                      ((string-contains (string-downcase session) "x11")     "X11/Xorg Backend")
                      (else "Desconhecido"))))
      (for-each log-info (list
        (string-append "Sessão atual: " (string-upcase session))
        (string-append "Backend GTK: "  backend))
      ))

    (for-each log-info (list 
        "--------------------------------"
        (format #f "Kernel: ~a"   (exec "uname -r"))
        (format #f "Hostname: ~a" (exec "hostname"))
        "--------------------------------"))

    (let ((gpu (exec "/usr/bin/lspci | grep -i vga | cut -d ':' -f3")))
      (log-info (string-append "GPU: "
                               (if (string=? gpu "N/A")
                                   "Hardware info via lspci indisponível"
                                   (string-trim-both gpu)))))

    (for-each log-info '(
      "--------------------------------"
      "Fim do diagnóstico"))
    (present win)))
