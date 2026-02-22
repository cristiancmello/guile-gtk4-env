#!/usr/bin/env bash
# build-binary.sh
# Compila o nosso wrapper em C gerando um binário ELF nativo

set -e

# Nome do nosso executável final
BIN_NAME="guile-gtk4-prototyper-bin"

echo "==> Compilando o binário nativo com o Guile embutido..."

# Usamos o pkg-config para pegar as flags exatas de compilação da libguile
# O gcc compila o src/main.c e linka com o runtime do Guile
gcc -o "$BIN_NAME" src/main.c $(pkg-config --cflags --libs guile-3.0)

echo "==> Sucesso! Binário gerado: $BIN_NAME"
echo "==> Para executar: ./$BIN_NAME"