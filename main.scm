(add-to-load-path (dirname (current-filename)))

(use-modules (g-golf)
             (watcher)
             (registry))

(g-irepository-require "Gtk" #:version "4.0")

(for-each gi-import '("Gtk" "Gdk" "Gio"))

(define *win* #f)

(load-ui-dir "components")

(define (reload-ui-action)
  (g-idle-add (lambda ()
                (if (and *win* (is-a? *win* <gtk-application-window>))
                    (catch #t
                      (lambda ()
                        (set-child *win* #f)
                        (load-ui-dir "components")
                        (load-all-uis *win*)
                        (present *win*))
                      (lambda (key . args)
                        (format #t "Erro no reload: ~a ~s\n" key args)))
                    #f)
                #f)))

(define (activate app)
  (set! *win* (make <gtk-application-window> #:application app #:default-width 600))
  (load-all-uis *win*)
  (for-each (lambda (f) (start-file-watcher f reload-ui-action))
            (get-watched-files))
  (present *win*))

(let ((app (make <gtk-application> #:application-id "org.guile.gtk.logs")))
  (connect app 'activate activate)
  (run app '()))
