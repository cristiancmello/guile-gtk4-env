(add-to-load-path (dirname (current-filename)))

(use-modules (g-golf)
             (watcher)
             (registry))

(g-irepository-require "Gtk" #:version "4.0")

(for-each gi-import '("Gtk" "Gdk" "Gio"))

(define *win*             #f)
(define *active-watchers* '())

;; Para cada watcher, chama stop! e aguarda sem busy-wait.
;; Como stop! fecha o fd, a thread de leitura bloqueada em read-c retorna
;; imediatamente com n <= 0 e encerra o loop sozinha.
(define (stop-all-watchers!)
  (for-each (lambda (stop!) (stop!)) *active-watchers*)
  (set! *active-watchers* '()))

(define (rebuild-ui!)
  (load-ui-dir "components")
  (when (and *win* (is-a? *win* <gtk-application-window>))
    (set-child *win* #f)
    (load-all-uis *win*)
    (present *win*)))

;; Para os watchers existentes antes de criar os novos, evitando
;; que duas threads monitorem o mesmo arquivo simultaneamente.
(define (start-watchers!)
  (stop-all-watchers!)
  (set! *active-watchers*
        (map (lambda (filepath)
               (start-file-watcher filepath reload-ui-action))
             (get-watched-files))))

;; Agenda o rebuild na main loop do GTK (thread-safe).
;; Retorna #f para que g-idle-add não reagende o callback.
(define (reload-ui-action)
  (g-idle-add
    (lambda ()
      (catch #t
        (lambda ()
          (rebuild-ui!)
          ;; Após rebuild, recria os watchers para refletir
          ;; eventuais novos arquivos em components/
          (start-watchers!))
        (lambda (key . args)
          (format (current-error-port)
                  "ERRO no reload: ~a ~s\n" key args)))
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