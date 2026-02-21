(use-modules (g-golf)
             (oop goops)
             (ice-9 popen)
             (ice-9 rdelim)
             (watcher))

;; 1. Configuração do GTK4
(g-irepository-require "Gtk" #:version "4.0")

;; Importação das classes essenciais
(for-each (lambda (name) (gi-import-by-name "Gtk" name))
          '("Application" 
            "ApplicationWindow" 
            "Box" 
            "Label" 
            "ScrolledWindow" 
            "TextView" 
            "TextBuffer" 
            "TextIter" 
            "ProgressBar"
            "CssProvider" 
            "StyleContext"))

;; 2. Estado Global
(define *win* #f)

;; Carrega o arquivo UI (que agora não deve ter referências a gi-struct-new)
(primitive-load "ui.scm")

(define (reload-ui-action)
  (g-idle-add (lambda ()
                (if (and *win* (is-a? *win* <gtk-application-window>))
                    (catch #t
                      (lambda ()
                        (set-child *win* #f)
                        (primitive-load "ui.scm")
                        (build-ui *win*)
                        (present *win*))
                      (lambda (key . args)
                        (format #t "Erro no reload: ~a ~s\n" key args)))
                    #f)
                #f)))

(define (activate app)
  (set! *win* (make <gtk-application-window> #:application app #:default-width 600))
  (build-ui *win*)
  (start-file-watcher "ui.scm" reload-ui-action)
  (present *win*))

(let ((app (make <gtk-application> #:application-id "org.guile.gtk.logs")))
  (connect app 'activate activate)
  (run app '()))
