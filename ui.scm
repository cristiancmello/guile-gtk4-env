(define (build-ui win)
  (let ((btn (make <gtk-button> #:label "Funciona agora!")))
    (set-child win btn)
    (present win)))
