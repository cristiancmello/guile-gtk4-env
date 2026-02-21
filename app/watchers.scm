(define-module (app watchers)
  #:use-module (watcher)
  #:use-module (registry)
  #:export (start-watchers!
            stop-watchers!))

(define *active-watchers* '())

(define (stop-watchers!)
  (for-each (lambda (stop!) (stop!)) *active-watchers*)
  (set! *active-watchers* '()))

(define (start-watchers! on-change)
  (stop-watchers!)
  (set! *active-watchers*
        (map (lambda (filepath)
               (start-file-watcher filepath on-change))
             (get-watched-files))))