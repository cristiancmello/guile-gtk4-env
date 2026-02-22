(define-module (app)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:duplicates (merge-generics replace warn-override-core warn last))

;; Carregamento do GTK4
(g-irepository-require "Gtk" #:version "4.0")
(gi-import "Gtk")

;; Handler do sinal 'activate'
(define (on-activate app)
  (display "Sinal 'activate' recebido!\n")
  
  (let* ((window-class (module-ref (current-module) '<gtk-application-window>))
         (window (make window-class #:application app))
         ;; Resolução dinâmica do método quit
         (quit-proc (module-ref (current-module) 'g-application-quit)))
    (display "Janela instanciada.\n")
    (quit-proc app)))

(define (main)
  (let* ((app-class (module-ref (current-module) '<gtk-application>))
         (app (make app-class 
                #:application-id "org.guile.exemplo"
                #:flags '()))
         ;; Resolução dinâmica do método run
         (run-proc (module-ref (current-module) 'g-application-run)))
    
    (connect app 'activate on-activate)
    (display "Iniciando loop da aplicação...\n")
    (run-proc app '())))

(main)
