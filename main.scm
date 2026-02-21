(add-to-load-path (dirname (current-filename)))

(use-modules (g-golf)
             (watcher)
             (registry))

(g-irepository-require "Gtk" #:version "4.0")

(for-each gi-import '("Gtk" "Gdk" "Gio"))

(define *win*             #f)
(define *active-watchers* '())

(define (stop-all-watchers!)
  (for-each (lambda (stop!) (stop!)) *active-watchers*)
  (set! *active-watchers* '()))

(define (rebuild-ui!)
  (load-ui-dir "components")
  (when (and *win* (is-a? *win* <gtk-application-window>))
    (set-child *win* #f)
    (load-all-uis *win*)
    (present *win*)))

(define (start-watchers!)
  (stop-all-watchers!)
  (set! *active-watchers*
        (map (lambda (filepath)
               (start-file-watcher filepath reload-ui-action))
             (get-watched-files))))

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
  (set! *win*
        (make <gtk-application-window>
              #:application   app
              #:default-width 600))
  (rebuild-ui!)
  (start-watchers!)
  (present *win*))

(let ((app (make <gtk-application>
                 #:application-id "org.guile.gtk.logs")))
  (connect app 'activate activate)
  (run app '()))