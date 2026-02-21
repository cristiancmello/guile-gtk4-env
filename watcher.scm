(define-module (watcher)
  #:use-module (system foreign)  ;; <--- Adicione isso para habilitar pointer->procedure
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 threads)
  #:export (start-file-watcher))

(define (start-file-watcher filename on-change)
  (let* ((libc (dynamic-link))
         ;; Importando funções da libc para usar o inotify
         (init (pointer->procedure int (dynamic-func "inotify_init" libc) '()))
         (add (pointer->procedure int (dynamic-func "inotify_add_watch" libc) (list int '* uint32)))
         (read-c (pointer->procedure ssize_t (dynamic-func "read" libc) (list int '* size_t)))
         (fd (init))
         (buffer (make-bytevector 1024)))
    
    ;; Função interna para re-adicionar o watch caso o editor substitua o arquivo
    (define (add-watch)
      (add fd (string->pointer filename) 8)) ;; 8 = IN_CLOSE_WRITE

    (if (< fd 0)
        (display "Erro: Falha ao iniciar inotify no kernel.\n")
        (begin
          (add-watch) ;; Watch inicial
          (make-thread (lambda ()
                         (let loop ()
                           ;; Bloqueia a thread até que o arquivo seja escrito e fechado
                           (read-c fd (bytevector->pointer buffer) 1024)
                           
                           ;; Re-adiciona o watch (essencial para editores que salvam via rename)
                           (add-watch)
                           
                           ;; Pequena pausa para o sistema de arquivos estabilizar
                           (usleep 100000)
                           
                           ;; Chama o callback de reload (reload-ui-action no main.scm)
                           (on-change)
                           (loop))))))))
