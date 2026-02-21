(define-module (registry)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:export (load-ui-dir load-all-uis get-watched-files))

(define *ui-cache* (make-hash-table))
(define *ui-order* '())

(define (filepath->module-name filepath)
  (let* ((no-ext   (string-drop-right filepath 4))
         (parts    (string-split no-ext #\/))
         (filtered (filter (lambda (s) (not (string=? s "."))) parts)))
    (map string->symbol filtered)))

(define (file-mtime filepath)
  (catch #t
    (lambda ()
      (stat:mtime (stat filepath)))
    (lambda (key . args) -1)))

(define (scm->css-path filepath)
  (let ((css (string-append (string-drop-right filepath 4) ".css")))
    (if (file-exists? css) css #f)))

(define (invalidate-module-cache! mod-name)
  (let ((existing (resolve-module mod-name #f #:ensure #f)))
    (when existing
      (catch #t
        (lambda () (reload-module existing))
        (lambda (key . args)
          (format (current-error-port)
                  "AVISO: reload-module falhou para ~a: ~a ~s\n"
                  mod-name key args))))))

(define (load-component! filepath)
  (let ((mod-name (filepath->module-name filepath)))
    (invalidate-module-cache! mod-name)
    (load filepath)
    (let ((module (resolve-module mod-name #f #:ensure #f)))
      (if module
          (module-ref module 'build-ui #f)
          (begin
            (format (current-error-port)
                    "AVISO: Módulo ~a não encontrado após load de ~a\n"
                    mod-name filepath)
            #f)))))

(define (load-ui-dir dir)
  (let* ((entries  (scandir dir (lambda (f) (string-suffix? ".scm" f))))
         (files    (if entries
                       (map (lambda (f) (string-append dir "/" f)) entries)
                       '()))
         (file-set (make-hash-table)))

    (for-each (lambda (fp) (hash-set! file-set fp #t)) files)

    (let ((to-remove
           (hash-fold (lambda (fp _ acc)
                        (if (hash-ref file-set fp #f)
                            acc
                            (cons fp acc)))
                      '()
                      *ui-cache*)))
      (for-each (lambda (fp)
                  (hash-remove! *ui-cache* fp)
                  (set! *ui-order*
                        (filter (lambda (x) (not (string=? x fp))) *ui-order*)))
                to-remove))

    (for-each
      (lambda (filepath)
        (let* ((mtime    (file-mtime filepath))
               (cached   (hash-ref *ui-cache* filepath #f))
               (changed? (or (not cached)
                             (not (= mtime (car cached))))))
          (when changed?
            (let ((builder (load-component! filepath)))
              (if builder
                  (begin
                    (hash-set! *ui-cache* filepath (cons mtime builder))
                    (unless cached
                      (set! *ui-order* (append *ui-order* (list filepath)))))
                  (begin
                    (hash-remove! *ui-cache* filepath)
                    (set! *ui-order*
                          (filter (lambda (x) (not (string=? x filepath)))
                                  *ui-order*))))))))
      files)))

(define (load-all-uis win)
  (for-each
    (lambda (filepath)
      (let ((entry (hash-ref *ui-cache* filepath #f)))
        (when entry
          ((cdr entry) win (scm->css-path filepath)))))
    *ui-order*))

(define (get-watched-files)
  (let ((scm-files (filter (lambda (fp) (hash-ref *ui-cache* fp #f))
                           *ui-order*)))
    (append scm-files
            (filter-map scm->css-path scm-files))))