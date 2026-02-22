(define-module (src app)
  #:use-module (g-golf)
  #:export (create-app))

;; Ignoramos warnings de compilação pois G-Golf injeta tipos em tempo de execução
(gi-import "Gtk" #:version "4.0")
(gi-import "Gio" #:version "2.0")

(define (activate app)
  (let ((window (make <gtk-application-window> 
                      #:application app
                      #:title "Guile GTK4 Prototyper")))
    (gtk-window-present window)))

(define (create-app)
  "Cria e retorna a instância da aplicação GTK, pronta para ser ativada ou testada."
  (let ((app (make <gtk-application> 
                   #:application-id "org.guile.prototyper"
                   #:flags '())))
    (connect app 'activate activate)
    app))