#!/bin/bash
set -e # Para parar no primeiro erro

echo "--- Preparando ambiente ---"
echo '(display "Olá do Guile Standalone!\n")' > app.scm

echo "--- Compilando bytecode ---"
guild compile -o app.go app.scm

echo "--- Compilando executável ---"
gcc main.c -o app $(pkg-config --cflags --libs guile-3.0)

echo "--- Executando teste de saída ---"
OUTPUT=$(./app)

if [ "$OUTPUT" == "Olá do Guile Standalone!" ]; then
    echo "✅ Teste Passou: Binário executado e saída correta."
    # Limpeza opcional para manter o repo limpo após sucesso
    rm app.scm app.go app
    exit 0
else
    echo "❌ Teste Falhou: Saída inesperada -> $OUTPUT"
    exit 1
fi