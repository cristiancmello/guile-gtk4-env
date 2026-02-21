(define-module (watcher)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 atomic)
  #:export (start-file-watcher))

(define IN_CLOSE_WRITE #x00000008)
(define DEBOUNCE-THRESHOLD 0.2)

(define (current-time-seconds)
  (let ((tv (gettimeofday)))
    (+ (car tv) (/ (cdr tv) 1e6))))

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
         (fd           (inotify-init)))

    (if (< fd 0)
        (begin
          (format (current-error-port)
                  "ERRO: Falha ao iniciar inotify para ~a\n" filename)
          (lambda () #f))

        (begin
          (inotify-add fd (string->pointer filename) IN_CLOSE_WRITE)

          (let* ((cancelled? (make-atomic-box #f))
                 (buffer     (make-bytevector 1024))
                 (debounce-mu (make-mutex))
                 (last-fired  0.0))

            (define (should-fire?)
              (with-mutex debounce-mu
                (let* ((now   (current-time-seconds))
                       (delta (- now last-fired)))
                  (if (> delta DEBOUNCE-THRESHOLD)
                      (begin (set! last-fired now) #t)
                      #f))))

            (make-thread
              (lambda ()
                (let loop ()
                  (unless (atomic-box-ref cancelled?)
                    (let ((n (read-c fd (bytevector->pointer buffer) 1024)))
                      (cond
                        ((<= n 0)
                         (unless (atomic-box-ref cancelled?)
                           (format (current-error-port)
                                   "AVISO: Watcher encerrado inesperadamente para ~a\n"
                                   filename)))
                        (else
                         (when (should-fire?)
                           (on-change))
                         (loop))))))))

            (lambda ()
              (atomic-box-set! cancelled? #t)
              (close-fd fd)))))))