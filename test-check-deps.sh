#!/usr/bin/env bash
# tests/check-deps.sh
set -e

BIN="dist/meu-app"

echo "--- [TESTE] Verificando dependências de $BIN ---"

if [ ! -f "$BIN" ]; then
    echo "FALHA: Binário $BIN não encontrado. Execute o build antes."
    exit 1
fi

# Procura por bibliotecas que o ldd não consegue resolver (not found)
# Forçamos o ldd a olhar apenas para a pasta dist/ (onde o rpath aponta)
MISSING=$(ldd "$BIN" | grep "not found" || true)

if [ -n "$MISSING" ]; then
    echo "FALHA: Bibliotecas ausentes detectadas:"
    echo "$MISSING"
    exit 1
else
    echo "SUCESSO: Todas as dependências foram resolvidas."
    exit 0
fi