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

(define *rebuild-pending?* #f)

(define (reload-ui-action)
  (unless *rebuild-pending?*
    (set! *rebuild-pending?* #t)
    (g-idle-add
      (lambda ()
        (catch #t
          (lambda ()
            (rebuild-ui!)
            (start-watchers! (get-watched-files) reload-ui-action))
          (lambda (key . args)
            (format (current-error-port)
                    "ERRO no reload: ~a ~s\n" key args)))
        (set! *rebuild-pending?* #f)
        #f))))

(define (activate app)
  (set-win! (make <gtk-application-window>
                  #:application   app
                  #:default-width 600))
  (rebuild-ui!)
  (start-watchers! (get-watched-files) reload-ui-action)
  (present (get-win)))

(let ((app (make <gtk-application>
                 #:application-id "org.guile.gtk.logs")))
  (connect app 'activate activate)
  (run app '()))