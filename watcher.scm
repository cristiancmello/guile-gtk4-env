(define-module (watcher)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 atomic)
  #:export (start-file-watcher))

;; inotify: só nos interessa o evento de escrita concluída
(define IN_CLOSE_WRITE #x00000008)

;; Threshold de debounce em segundos (0.2s)
(define DEBOUNCE-THRESHOLD 0.2)

;; Retorna o tempo atual como número real em segundos (POSIX monotônico via gettimeofday)
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
          ;; Retorna um stop! inerte
          (lambda () #f))

        (begin
          (inotify-add fd (string->pointer filename) IN_CLOSE_WRITE)

          ;; Flag atômica: a thread de leitura checa antes de cada iteração.
          ;; stop! seta #t e fecha o fd, fazendo read-c retornar <= 0.
          (let* ((cancelled? (make-atomic-box #f))
                 (buffer     (make-bytevector 1024))
                 ;; Último instante em que on-change foi disparado
                 (last-fired (make-atomic-box 0.0)))

            (make-thread
              (lambda ()
                (let loop ()
                  ;; Sai imediatamente se já foi cancelado antes do próximo read
                  (unless (atomic-box-ref cancelled?)
                    (let ((n (read-c fd (bytevector->pointer buffer) 1024)))
                      (cond
                        ;; fd fechado ou erro — encerra silenciosamente
                        ((<= n 0)
                         (unless (atomic-box-ref cancelled?)
                           (format (current-error-port)
                                   "AVISO: Watcher encerrado inesperadamente para ~a\n"
                                   filename)))
                        (else
                         ;; Debounce por timestamp: só dispara se passou o threshold
                         ;; desde o último disparo. Sem nenhum sleep aqui.
                         (let* ((now   (current-time-seconds))
                                (last  (atomic-box-ref last-fired))
                                (delta (- now last)))
                           (when (> delta DEBOUNCE-THRESHOLD)
                             (atomic-box-set! last-fired now)
                             (on-change)))
                         (loop))))))))

            ;; stop! — seguro chamar múltiplas vezes
            (lambda ()
              (atomic-box-set! cancelled? #t)
              (close-fd fd)))))))