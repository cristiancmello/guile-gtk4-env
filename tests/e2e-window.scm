#!/usr/bin/guile -s
!#

;; Adiciona o diretório atual ao path para achar o módulo src/app
(add-to-load-path (getcwd))

(use-modules (g-golf)
             (src app))

(gi-import "Gtk" #:version "4.0")
(gi-import "Gio" #:version "2.0")

(define (run-ui-test)
  (format #t "Iniciando teste de UI: Verificando instanciação da Janela...\n")
  
  ;; 1. Cria a aplicação (Arrange)
  (define app (create-app))
  
  ;; 2. Registra e Ativa a aplicação programaticamente (Act)
  ;; O #f indica que não temos um objeto GCancellable
  (g-application-register app #f) 
  (g-application-activate app)
  
  ;; 3. Obtém a janela que o GTK criou (Assert)
  (let ((window (gtk-application-get-active-window app)))
    (if window
        (let ((title (gtk-window-get-title window)))
          (if (string=? title "Guile GTK4 Prototyper")
              (begin
                (format #t "Sucesso: Janela encontrada com o título '~a'!\n" title)
                (primitive-exit 0))
              (begin
                (format #t "Falha: Título incorreto. Esperado 'Guile GTK4 Prototyper', recebido '~a'\n" title)
                (primitive-exit 1))))
        (begin
          (format #t "Falha: Nenhuma janela ativa foi registrada na aplicação.\n")
          (primitive-exit 1)))))

(run-ui-test)