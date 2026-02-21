(define-module (watcher)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 threads)
  #:use-module (g-golf)
  #:export (start-file-watcher))

;; --- Constantes ------------------------------------------------------

(define IN_CLOSE_WRITE #x00000008)

;; Tamanho mínimo garantido para um evento inotify:
;; sizeof(inotify_event) = 16 bytes + NAME_MAX (255) + 1 = 272 bytes
(define %inotify-buf-size 272)

;; Debounce: aguarda 300ms após o último evento antes de disparar on-change
(define %debounce-ms 300)

;; --- FFI: resolvidos uma vez no carregamento do módulo ---------------

(define %libc          (dynamic-link))

(define %inotify-init
  (pointer->procedure int
    (dynamic-func "inotify_init" %libc) '()))

(define %inotify-add-watch
  (pointer->procedure int
    (dynamic-func "inotify_add_watch" %libc)
    (list int '* uint32)))

(define %close
  (pointer->procedure int
    (dynamic-func "close" %libc)
    (list int)))

(define %read
  (pointer->procedure ssize_t
    (dynamic-func "read" %libc)
    (list int '* size_t)))

;; --- Implementação ---------------------------------------------------

(define (start-file-watcher filepath on-change)
  (let ((fd (%inotify-init)))
    (if (< fd 0)
        (begin
          (format (current-error-port)
                  "ERRO: Falha ao iniciar inotify para ~a\n" filepath)
          (lambda () #f))
        (begin
          (%inotify-add-watch fd (string->pointer filepath) IN_CLOSE_WRITE)

          (let ((buf       (make-bytevector %inotify-buf-size))
                (timer-id  #f)
                (mu        (make-mutex)))

            (make-thread
              (lambda ()
                (let loop ()
                  (let ((n (%read fd (bytevector->pointer buf) %inotify-buf-size)))
                    (cond
                      ((<= n 0)
                       (format (current-error-port)
                               "INFO: Watcher encerrado para ~a\n" filepath))
                      (else
                       (with-mutex mu
                         ;; Cancela timer pendente e agenda novo — debounce
                         (when timer-id
                           (g-source-remove timer-id)
                           (set! timer-id #f))
                         (set! timer-id
                               (g-timeout-add %debounce-ms
                                 (lambda ()
                                   (with-mutex mu (set! timer-id #f))
                                   (on-change)
                                   #f))))  ;; #f = não repetir
                       (loop)))))))

            (lambda ()
              (%close fd)))))))