#!/bin/bash
set -e

echo "--- Iniciando Teste de Isolamento Real ---"

# 1. Verifica se a libguile está sendo buscada na pasta local ($ORIGIN)
# O grep busca se o caminho da lib contém a nossa pasta atual
CHECK_LIB=$(ldd ./dist_test/meu-app | grep libguile)

if [[ $CHECK_LIB == *"./libguile"* ]] || [[ $CHECK_LIB == *"/dist_test/"* ]]; then
    echo "✅ Sucesso: O binário está linkado localmente (RPATH $ORIGIN)."
else
    echo "❌ Falha: O binário ainda está usando a lib do sistema: $CHECK_LIB"
    exit 1
fi

# 2. Executa o binário em modo de teste (sem travar)
echo "--- Executando binário em modo isolado ---"
cd dist_test
GUILE_TEST_MODE=1 LD_LIBRARY_PATH=. ./meu-app

echo "✅ Teste de execução isolada passou!"