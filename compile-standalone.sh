#!/bin/bash
set -e
APP_NAME="meu-app"

echo "--- [1/5] Compilando ---"
mkdir -p .build dist/ccache
guild compile -o .build/main.go main.scm
gcc main.c -o "dist/$APP_NAME.bin" $(pkg-config --cflags --libs guile-3.0)

mv .build/main.go dist/ccache/

echo "--- [2/5] Coletando Bibliotecas (.so) ---"
bundle_lib() {
    local soname=$1
    local linkname=$2
    local src=$(find /usr/lib64 /usr/lib -name "$soname" | head -n 1)
    if [ -n "$src" ]; then
        cp -v "$src" dist/
        (cd dist && ln -sf "$soname" "$linkname")
    fi
}

bundle_lib "libguile-3.0.so.1" "libguile-3.0.so"
bundle_lib "libgirepository-1.0.so.1" "libgirepository-1.0.so"
bundle_lib "libg-golf.so.0" "libg-golf.so"
bundle_lib "libgtk-4.so.1" "libgtk-4.so"
bundle_lib "libglib-2.0.so.0" "libglib-2.0.so"

# Pega o resto das dependências
libs=$(ldd "dist/$APP_NAME.bin" | grep "=> /" | awk '{print $3}' | sort -u | grep -vE "libc.so|libm.so|libdl.so|libpthread.so")
for lib in $libs; do cp -vn "$lib" dist/; done

echo "--- [3/5] Typelibs ---"
# Copiamos para a RAIZ da dist para facilitar o GI_TYPELIB_PATH
INTRO_DIR="/usr/lib64/girepository-1.0"
for repo in Gtk-4.0 Gdk-4.0 Gsk-4.0 Graphene-1.0 Pango-1.0 Gio-2.0 GObject-2.0 GLib-2.0; do
    [ -f "$INTRO_DIR/$repo.typelib" ] && cp -p "$INTRO_DIR/$repo.typelib" dist/
done

echo "--- [4/5] Módulos do Guile (O Coração) ---"
# Tenta localizar a pasta de fontes do Guile de forma mais robusta
SCM_ROOT=$(guile -c '(display (dirname (search-path %load-path "ice-9/boot-9.scm"))) ' | sed 's/\/ice-9//')
GO_ROOT=$(pkg-config --variable=libdir guile-3.0)/guile/3.0/ccache

if [ ! -d "$SCM_ROOT/ice-9" ]; then
    echo "ERRO CRÍTICO: Não foi possível encontrar a pasta ice-9 em $SCM_ROOT"
    exit 1
fi

for mod in ice-9 srfi oop system language rnrs sxml; do
    cp -rp "$SCM_ROOT/$mod" dist/
    [ -d "$GO_ROOT/$mod" ] && mkdir -p "dist/ccache/$mod" && cp -rp "$GO_ROOT/$mod"/* "dist/ccache/$mod/"
done

# G-Golf
GGOLF_ROOT=$(guile -c '(use-modules (g-golf)) (display (dirname (search-path %load-path "g-golf.scm")))')
cp -rp "$GGOLF_ROOT/g-golf" dist/
cp -p "$GGOLF_ROOT/g-golf.scm" dist/
[ -d "$GO_ROOT/g-golf" ] && mkdir -p dist/ccache/g-golf && cp -rp "$GO_ROOT/g-golf"/* dist/ccache/g-golf/

echo "--- [5/5] Criando Launcher ---"
cat << 'EOF' > "$APP_NAME"
#!/bin/bash
HERE=$(dirname "$(readlink -f "$0")")
# Forçamos o uso estrito da nossa pasta dist/
export LD_LIBRARY_PATH="$HERE/dist"
export GI_TYPELIB_PATH="$HERE/dist"
export GUILE_LOAD_PATH="$HERE/dist"
export GUILE_LOAD_COMPILED_PATH="$HERE/dist/ccache"
export GUILE_AUTO_COMPILE=0

exec "$HERE/dist/meu-app.bin" "$@"
EOF
chmod +x "$APP_NAME"
echo "✅ Pronto! Execute: ./$APP_NAME"