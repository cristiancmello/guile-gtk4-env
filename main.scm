(define-module (main)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:duplicates (merge-generics replace warn-override-core warn last))

(g-irepository-require "Gtk" #:version "4.0")
(gi-import "Gtk")

(define (on-activate app)
  (let* ((window-class (module-ref (current-module) '<gtk-application-window>))
         (window (make window-class #:application app))
         (present (module-ref (current-module) 'present)))
    (present window)
    (display "Janela carregada.\n")
    
    ;; SOLID: Se estivermos em teste, fechamos imediatamente após carregar
    (when (getenv "GUILE_TEST_MODE")
      (display "Modo de teste detectado, encerrando...\n")
      ((module-ref (current-module) 'g-application-quit) app))))

(define (main-proc)
  (let* ((app-class (module-ref (current-module) '<gtk-application>))
         (app (make app-class 
                #:application-id "org.guile.standalone"
                #:flags '()))
         (run-proc (module-ref (current-module) 'g-application-run)))
    
    (connect app 'activate on-activate)
    (run-proc app '())))

;; Executa a lógica
(main-proc)