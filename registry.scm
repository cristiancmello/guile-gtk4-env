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
  (let ((entries (scandir dir (lambda (f) (string-suffix? ".scm" f)))))
    (when entries
      (for-each
        (lambda (f)
          (let ((filepath (string-append dir "/" f)))
            (primitive-load filepath)
            (let ((builder (module-ref (current-module) 'build-ui #f)))
              (when builder
                (set! *watched-files* (cons filepath *watched-files*))
                (set! *builders* (cons builder *builders*))))))
        entries))))

(define (load-all-uis win)
  (for-each (lambda (b) (b win))
            (reverse *builders*)))

(define (get-watched-files)
  (reverse *watched-files*))
