#!/bin/bash
set -e

echo "--- [1/2] Compilando Scheme para Bytecode (main.go) ---"
guild compile -o main.go main.scm

echo "--- [2/2] Compilando Wrapper C para Binário (meu-app) ---"
gcc main.c -o meu-app $(pkg-config --cflags --libs guile-3.0)

echo "✅ Build concluído: Executável 'meu-app' e bytecode 'main.go' gerados."
echo "Para rodar: ./meu-app"