#!/usr/bin/env bash
# tests/check-modules.sh
set -e

BIN="dist/meu-app"

echo "--- [TESTE] Verificando carregamento de módulos Guile em $BIN ---"

if [ ! -f "$BIN" ]; then
    echo "FALHA: Binário $BIN não encontrado."
    exit 1
fi

# Tentamos carregar o módulo srfi-11. 
# Se o binário falhar ao iniciar o módulo, capturamos o erro.
if ! ./$BIN -c '(use-modules (srfi srfi-11)) (display "Módulo carregado\n")'; then
    echo "FALHA: Não foi possível carregar o módulo (srfi srfi-11)."
    exit 1
else
    echo "SUCESSO: Módulos básicos carregados corretamente."
    exit 0
fi