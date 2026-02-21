;; app/watchers.scm — só sabe gerenciar o ciclo de vida
(define-module (app watchers)
  #:use-module (watcher)
  #:export (start-watchers!
            stop-watchers!))

(define *active-watchers* '())

(define (stop-watchers!)
  (for-each (lambda (stop!) (stop!)) *active-watchers*)
  (set! *active-watchers* '()))

;; Recebe a lista de arquivos de fora — não busca ela mesmo
(define (start-watchers! filepaths on-change)
  (stop-watchers!)
  (set! *active-watchers*
        (map (lambda (filepath)
               (start-file-watcher filepath on-change))
             filepaths)))