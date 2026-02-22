#!/usr/bin/guile -s
!#

;; Adiciona a raiz do projeto ao "load path" para que o Guile encontre o diretório "src"
(add-to-load-path (getcwd))

(use-modules (g-golf)
             (src app))

;; Importamos Gio porque a função g-application-run pertence a esta biblioteca
(gi-import "Gio" #:version "2.0")

(define (main)
  (let ((app (create-app)))
    ;; g-application-run bloqueia a execução do terminal e inicia a interface gráfica.
    ;; (command-line) repassa quaisquer argumentos passados no terminal para o GTK.
    (g-application-run app (command-line))))

(main)