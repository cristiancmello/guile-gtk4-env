#!/bin/bash
set -e

echo "--- Testando Standalone em pasta isolada ---"
cd dist

# Tentamos rodar. Se ele abrir e fechar (devido ao quit no script), passou.
# O segredo do teste TDD aqui é que o app.scm deve ter um timeout ou um quit imediato.
OUTPUT=$(LD_LIBRARY_PATH=. ./meu-app)

echo "✅ Teste Finalizado."