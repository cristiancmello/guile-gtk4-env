(define-module (watcher)
  #:use-module (system foreign)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 threads)
  #:export (start-file-watcher))

(define IN_CLOSE_WRITE #x00000008)

(define (start-file-watcher filename on-change)
  (let* ((libc         (dynamic-link))
         (inotify-init (pointer->procedure int
                         (dynamic-func "inotify_init" libc) '()))
         (inotify-add  (pointer->procedure int
                         (dynamic-func "inotify_add_watch" libc)
                         (list int '* uint32)))
         (close-fd     (pointer->procedure int
                         (dynamic-func "close" libc)
                         (list int)))
         (read-c       (pointer->procedure ssize_t
                         (dynamic-func "read" libc)
                         (list int '* size_t)))
         (fd           (inotify-init))
         (buffer       (make-bytevector 1024))
         (pending?     #f)
         (mu           (make-mutex)))

    (if (< fd 0)
        (begin
          (format (current-error-port)
                  "ERRO: Falha ao iniciar inotify para ~a\n" filename)
          (lambda () #f))

        (begin
          (inotify-add fd (string->pointer filename) IN_CLOSE_WRITE)

          (make-thread
            (lambda ()
              (let loop ()
                (let ((n (read-c fd (bytevector->pointer buffer) 1024)))
                  (cond
                    ((<= n 0)
                     (format (current-error-port)
                             "INFO: Watcher encerrado para ~a\n" filename))
                    (else
                     (with-mutex mu
                       (when (not pending?)
                         (set! pending? #t)
                         (usleep 200000)
                         (on-change)
                         (usleep 800000)
                         (set! pending? #f)))
                     (loop)))))))
          (lambda ()
            (close-fd fd))))))