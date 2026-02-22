#!/bin/bash
set -e

echo "--- [1/4] Compilando Bytecode e Binário ---"
guild compile -o main.go main.scm
# Compilação com RPATH e flags de origem
gcc main.c -o meu-app $(pkg-config --cflags --libs guile-3.0) \
    -Wl,-rpath,'$ORIGIN' -Wl,-z,origin

echo "--- [2/4] Preparando Pasta dist/ ---"
mkdir -p dist
cp meu-app main.go dist/

echo "--- [3/4] Copiando Bibliotecas ---"
# Pega o caminho da libguile via pkg-config para ser infalível
LIB_FILE=$(pkg-config --variable=libdir guile-3.0)/libguile-3.0.so.1
cp "$LIB_FILE" dist/

echo "--- [4/4] Copiando Kernel e Bytecode do Guile ---"
# 1. Localiza fontes (.scm) - usando search-path que é built-in
ICE9_SOURCE=$(guile -c '(display (dirname (search-path %load-path "ice-9/boot-9.scm")))')
cp -r "$ICE9_SOURCE" dist/

# 2. Localiza bytecode compilado (.go) do sistema
# Em vez de %config, usamos pkg-config para pegar o libdir (ex: /usr/lib64)
# O bytecode costuma ficar em libdir/guile/3.0/ccache/ice-9
LIBDIR=$(pkg-config --variable=libdir guile-3.0)
ICE9_COMPILED="$LIBDIR/guile/3.0/ccache/ice-9"

if [ -d "$ICE9_COMPILED" ]; then
    echo "Localizado bytecode em: $ICE9_COMPILED"
    # Criamos a estrutura e copiamos os .go para dentro da pasta ice-9 local
    mkdir -p dist/ice-9
    cp -r "$ICE9_COMPILED"/* dist/ice-9/
else
    echo "❌ Erro: Diretório de bytecode não encontrado em $ICE9_COMPILED"
    exit 1
fi

echo "--- [VERIFICAÇÃO] ---"
ls -F dist/ice-9/ | head -n 5 # Mostra os primeiros arquivos para confirmar .go
echo "✅ Build concluído com sucesso!"