(add-to-load-path (dirname (current-filename)))

(use-modules (g-golf)
             (registry)
             (app state)
             (app watchers))

(g-irepository-require "Gtk" #:version "4.0")

(for-each gi-import '("Gtk" "Gdk" "Gio"))

(define (rebuild-ui!)
  (load-ui-dir "components")
  (when (win-ready?)
    (set-child (get-win) #f)
    (load-all-uis (get-win))
    (present (get-win))))

(define (reload-ui-action)
  (g-idle-add
    (lambda ()
      (catch #t
        (lambda ()
          (rebuild-ui!))
        (lambda (key . args)
          (format (current-error-port)
                  "Erro no reload: ~a ~s\n" key args)))
      #f)))

(define (activate app)
  (set-win!
    (make <gtk-application-window>
          #:application   app
          #:default-width 600))
  (rebuild-ui!)
  (start-watchers! reload-ui-action)
  (present (get-win)))

(let ((app (make <gtk-application>
                 #:application-id "org.guile.gtk.logs")))
  (connect app 'activate activate)
  (run app '()))