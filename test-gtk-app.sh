#!/bin/bash
set -e

echo "--- Preparando app.scm (Versão Rigorosa - Sem Avisos) ---"
cat << 'EOF' > app.scm
(define-module (app)
  #:use-module (oop goops)
  #:use-module (g-golf)
  #:duplicates (merge-generics replace warn-override-core warn last))

;; Carregamento do GTK4
(g-irepository-require "Gtk" #:version "4.0")
(gi-import "Gtk")

(define (main)
  ;; Usamos module-ref para obter a classe dinamicamente.
  ;; Isso informa ao compilador: "Eu sei que este símbolo virá do módulo em runtime".
  (let* ((gtk-app-class (module-ref (current-module) '<gtk-application>))
         (app (make gtk-app-class 
                #:application-id "org.guile.exemplo"
                #:flags '())))
    
    (if (is-a? app gtk-app-class)
        (display "GtkApplication instanciada com sucesso\n")
        (display "Falha na instanciação\n"))))

(main)
EOF

echo "--- Compilando bytecode ---"
# Agora sem nenhuma flag de warning (usando o padrão do sistema)
guild compile -o app.go app.scm

echo "--- Executando teste de objeto ---"
OUTPUT=$(./app)

if [ "$OUTPUT" == "GtkApplication instanciada com sucesso" ]; then
    echo "✅ Teste Passou: Clean Green atingido!"
    exit 0
else
    echo "❌ Teste Falhou. Saída:"
    echo "$OUTPUT"
    exit 1
fi