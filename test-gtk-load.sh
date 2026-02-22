#!/bin/bash
set -e

echo "--- Preparando app.scm com GTK4 ---"
cat << 'EOF' > app.scm
(use-modules (g-golf))
(g-irepository-require "Gtk" #:version "4.0")
(display "GTK4 Carregado com Sucesso\n")
EOF

echo "--- Compilando bytecode ---"
guild compile -o app.go app.scm

echo "--- Compilando executável ---"
# O main.c permanece o mesmo com setlocale
gcc main.c -o app $(pkg-config --cflags --libs guile-3.0)

echo "--- Executando teste de carregamento ---"
OUTPUT=$(./app)

if [ "$OUTPUT" == "GTK4 Carregado com Sucesso" ]; then
    echo "✅ Teste Passou: G-Golf e GTK4 carregados no binário."
    exit 0
else
    echo "❌ Teste Falhou: Saída inesperada -> $OUTPUT"
    exit 1
fi