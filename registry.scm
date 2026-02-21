(define-module (registry)
  #:use-module (ice-9 ftw)
  #:export (load-ui-dir load-all-uis get-watched-files register-ui!))

(define *builders* '())
(define *watched-files* '())

(define (register-ui! filename builder)
  (set! *watched-files* (cons filename *watched-files*))
  (set! *builders* (cons builder *builders*)))

(define (load-ui-dir dir)
  (set! *builders* '())
  (set! *watched-files* '())
  (for-each
    (lambda (f)
      (when (string-suffix? ".scm" f)
        (primitive-load (string-append dir "/" f))))
    (scandir dir (lambda (f) (string-suffix? ".scm" f)))))

(define (load-all-uis win)
  (for-each (lambda (b) (b win))
            (reverse *builders*)))

(define (get-watched-files)
  (reverse *watched-files*))
