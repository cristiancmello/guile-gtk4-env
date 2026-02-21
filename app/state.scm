(define-module (app state)
  #:use-module (g-golf)
  #:export (get-win
            set-win!
            win-ready?))

(define *win* #f)

(define (get-win)
  *win*)

(define (set-win! w)
  (set! *win* w))

(define (win-ready?)
  (and *win* (is-a? *win* <gtk-application-window>)))