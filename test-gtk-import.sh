#!/usr/bin/env bash
set -e
BIN="dist/meu-app"

echo "--- [TESTE] Verificando Importação do GTK4 via G-Golf ---"

# Tentamos carregar o módulo g-golf e importar o namespace Gtk 4.0
# ... dentro do tests/test-gtk-import.sh ...
if ! ./$BIN -c '(use-modules (g-golf)) (gi-import "Gtk" #:version "4.0") (display "GTK4 Importado!\n")'; then
    echo "FALHA: Não foi possível encontrar o G-Golf ou as Typelibs do GTK4."
    exit 1
else
    echo "SUCESSO: G-Golf e GTK4 estão acessíveis."
    exit 0
fi