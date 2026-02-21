(use-modules (g-golf)
             (oop goops)
             (ice-9 threads)
             (system foreign)
             (rnrs bytevectors))

;; Globais protegidas
(define *win* #f)
(define *app* #f)

;; Inicialização do GTK
(g-irepository-require "Gtk" #:version "4.0")
(for-each (lambda (name) (gi-import-by-name "Gtk" name))
          '("Application" "ApplicationWindow" "Button" "Box" "Label"))

;; Carrega a UI inicial (certifique-se que o arquivo ui.scm existe)
(load "ui.scm")

(define (reload-ui)
  (g-idle-add (lambda ()
                (if (and *win* (is-a? *win* <gtk-application-window>))
                    (catch #t
                      (lambda ()
                        (set-child *win* #f)
                        (load "ui.scm")
                        (build-ui *win*)
                        (display "=> UI Atualizada!\n"))
                      (lambda (k . a) 
                        (format #t "Erro no ui.scm: ~a ~s\n" k a)))
                    (display "Aguardando janela...\n"))
                #f)))

(define (start-inotify)
  (let* ((libc (dynamic-link))
         (init (pointer->procedure int (dynamic-func "inotify_init" libc) '()))
         (add (pointer->procedure int (dynamic-func "inotify_add_watch" libc) (list int '* uint32)))
         (read-c (pointer->procedure ssize_t (dynamic-func "read" libc) (list int '* size_t)))
         (fd (init))
         (buffer (make-bytevector 1024)))
    (if (< fd 0)
        (display "Falha ao iniciar inotify\n")
        (begin
          (add fd (string->pointer "ui.scm") 8) ;; 8 = IN_CLOSE_WRITE
          (make-thread (lambda ()
                         (let loop ()
                           (read-c fd (bytevector->pointer buffer) 1024)
                           (usleep 100000) ;; 0.1s de folga para o disco
                           (reload-ui)
                           (loop))))))))

(define (activate app)
  (set! *win* (make <gtk-application-window> #:application app))
  (set-default-size *win* 400 300)
  (build-ui *win*))

(display "=== Motor de Live Reload Ativo ===\n")
(set! *app* (make <gtk-application> #:application-id "org.guile.watcher"))
(connect *app* 'activate activate)
(start-inotify)
(run *app* '())
