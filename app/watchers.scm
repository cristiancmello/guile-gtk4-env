;; app/watchers.scm — só sabe gerenciar o ciclo de vida
(define-module (app watchers)
  #:use-module (watcher)
  #:export (start-watchers!
            stop-watchers!))

(define *active-watchers* '())

;; Conjunto de arquivos para os quais já emitimos aviso de "não encontrado".
;; Evita repetição do aviso a cada ciclo de reload.
(define *warned-missing* (make-hash-table))

(define (stop-watchers!)
  (for-each (lambda (stop!) (stop!)) *active-watchers*)
  (set! *active-watchers* '()))

;; Recebe a lista de arquivos de fora — não busca ela mesmo.
;; Arquivos que não existem no disco são ignorados silenciosamente,
;; evitando que recursos opcionais (ex: styles/main.css ainda não criado)
;; gerem watchers inertes ou erros encobertos.
;; O aviso de "não encontrado" é emitido apenas uma vez por arquivo.
(define (start-watchers! filepaths on-change)
  (stop-watchers!)
  (let ((existing (filter file-exists? filepaths))
        (missing  (filter (lambda (fp) (not (file-exists? fp))) filepaths)))
    ;; Avisa só para arquivos ainda não reportados
    (for-each
      (lambda (fp)
        (unless (hash-ref *warned-missing* fp #f)
          (hash-set! *warned-missing* fp #t)
          (format (current-error-port)
                  "AVISO: arquivo não encontrado, watcher ignorado: ~a\n" fp)))
      missing)
    ;; Se um arquivo que estava faltando agora existe, limpa o aviso
    ;; para que ele volte a ser reportado caso suma novamente.
    (for-each
      (lambda (fp) (hash-remove! *warned-missing* fp))
      existing)
    (set! *active-watchers*
          (map (lambda (fp) (start-file-watcher fp on-change))
               existing))))