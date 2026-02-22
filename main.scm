;; No topo do seu main.scm, logo após os use-modules
(setenv "GTK_A11Y" "none") ; Desativa acessibilidade (ganha tempo)
(setenv "G_MESSAGES_DEBUG" "none")

;; main.scm
(define-module (main)
  #:use-module (g-golf)
  #:export (main))

;; Voltamos ao básico funcional
(gi-import "Gtk" #:version "4.0")

(define (on-button-clicked button)
  (display "Botão clicado! Funciona sem subset.\n"))

(define (activate app)
  (let* ((window (make <gtk-application-window> 
                       #:application app
                       #:title "Guile GTK4 Standalone"
                       #:default-width 400
                       #:default-height 300))
         (box (make <gtk-box> 
                    #:orientation 'vertical
                    #:spacing 10
                    #:margin-top 20
                    #:margin-bottom 20
                    #:margin-start 20
                    #:margin-end 20))
         (label (make <gtk-label> 
                      #:label "Lentidão inicial = G-Golf mapeando o GTK4"))
         (button (make <gtk-button> 
                       #:label "Clique Aqui")))
    
    (connect button 'clicked on-button-clicked)
    (set-child window box)
    (append box label)
    (append box button)
    (present window)))

(define (main args)
  (let ((app (make <gtk-application> 
                   #:application-id "io.github.cristiancmello.app")))
    (connect app 'activate activate)
    (run app args)))