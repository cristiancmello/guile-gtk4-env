#!/usr/bin/env bash
# scripts/build-standalone.sh
# Empacotamento Dinâmico Otimizado para Performance (Boot Rápido)

set -e

PROJECT_ROOT=$(pwd)
DIST_DIR="$PROJECT_ROOT/dist"
APP_NAME="GuileGTK4Prototyper"
APP_DIR="$DIST_DIR/AppDir"
BIN_NAME="guile-gtk4-prototyper-bin"
BIN_PATH="$DIST_DIR/$BIN_NAME"

echo "==> 0. Criando diretório de distribuição e checando dependências..."
mkdir -p "$DIST_DIR"

for cmd in gcc pkg-config ldd wget awk xargs file guild find; do
    if ! command -v $cmd &> /dev/null; then
        echo "ERRO: Comando '$cmd' não encontrado."
        exit 1
    fi
done

echo "==> 1. Compilando o entrypoint nativo (C)..."
gcc -O2 -o "$BIN_PATH" src/main.c $(pkg-config --cflags --libs guile-3.0)

echo "==> 2. Preparando a estrutura do AppDir em $DIST_DIR..."
rm -rf "$APP_DIR"
mkdir -p "$APP_DIR/usr/bin"
mkdir -p "$APP_DIR/usr/lib"
mkdir -p "$APP_DIR/usr/lib/girepository-1.0"
mkdir -p "$APP_DIR/usr/share/$APP_NAME/src"
mkdir -p "$APP_DIR/usr/share/applications"

echo "==> 3. Copiando código e PRÉ-COMPILANDO nossa aplicação..."
cp "$BIN_PATH" "$APP_DIR/usr/bin/$APP_NAME"
chmod +x "$APP_DIR/usr/bin/$APP_NAME"
cp src/app.scm "$APP_DIR/usr/share/$APP_NAME/src/"
# Compilação AOT (Ahead-of-Time) para garantir que nossa lógica seja instantânea
guild compile src/app.scm -o "$APP_DIR/usr/share/$APP_NAME/src/app.go"

echo "==> 4. Coletando bibliotecas dinâmicas (.so)..."
ldd "$BIN_PATH" | grep "=> /" | awk '{print $3}' | xargs -I '{}' cp -L -n '{}' "$APP_DIR/usr/lib/" || true

echo "==> 4.1. Caçando bibliotecas de runtime (GTK4, G-Golf, GI)..."
LIBS_TO_HUNT=("libg-golf" "libgirepository-1.0" "libgtk-4" "libgobject-2.0" "libglib-2.0" "libgio-2.0" "libadwaita-1" "libepoxy")

SEARCH_PATHS=""
for p in /usr/lib /usr/local/lib /usr/lib64 /usr/lib/x86_64-linux-gnu; do
    [ -d "$p" ] && SEARCH_PATHS="$SEARCH_PATHS $p"
done

for lib in "${LIBS_TO_HUNT[@]}"; do
    LIB_PATH=$(find $SEARCH_PATHS -name "${lib}.so*" 2>/dev/null | head -n 1)
    if [ -n "$LIB_PATH" ]; then
        REAL_FILE=$(basename "$LIB_PATH")
        echo "Encapsulando $lib de $LIB_PATH"
        cp -L "$LIB_PATH" "$APP_DIR/usr/lib/"
        TARGET_LINK="$lib.so"
        if [ "$REAL_FILE" != "$TARGET_LINK" ]; then
            (cd "$APP_DIR/usr/lib" && ln -sf "$REAL_FILE" "$TARGET_LINK")
        fi
    fi
done

echo "==> 4.2. Coletando Typelibs..."
find $SEARCH_PATHS -name "girepository-1.0" -type d 2>/dev/null | while read dir; do
    cp -L -r "$dir"/*.typelib "$APP_DIR/usr/lib/girepository-1.0/" 2>/dev/null || true
done

echo "==> 4.3. Coletando Módulos Scheme e Bytecode (Preservando Estrutura)..."
mkdir -p "$APP_DIR/usr/share/guile/site"
mkdir -p "$APP_DIR/usr/lib/guile/ccache"

# Copia os arquivos .scm (módulos)
guile -c "(for-each (lambda (p) (display p) (newline)) %load-path)" | while read path; do
    if [ -d "$path" ] && [ "$path" != "." ]; then
        cp -r -L -n "$path"/. "$APP_DIR/usr/share/guile/site/" 2>/dev/null || true
    fi
done

# Copia os arquivos .go (bytecode compilado)
guile -c "(for-each (lambda (p) (display p) (newline)) %load-compiled-path)" | while read path; do
    if [ -d "$path" ] && [ "$path" != "." ]; then
        cp -r -L -n "$path"/. "$APP_DIR/usr/lib/guile/ccache/" 2>/dev/null || true
    fi
done

echo "==> 5. Metadados e AppRun..."
cat << EOF > "$APP_DIR/org.guile.prototyper.desktop"
[Desktop Entry]
Version=1.0
Type=Application
Name=Guile GTK4 Prototyper
Exec=$APP_NAME
Icon=org.guile.prototyper
Terminal=false
Categories=Development;
EOF
cp "$APP_DIR/org.guile.prototyper.desktop" "$APP_DIR/usr/share/applications/"
touch "$APP_DIR/org.guile.prototyper.png"

cat << 'EOF' > "$APP_DIR/AppRun"
#!/bin/sh
HERE="$(dirname "$(readlink -f "${0}")")"
export GUILE_AUTO_COMPILE=0
export LD_LIBRARY_PATH="$HERE/usr/lib:$LD_LIBRARY_PATH"
export GI_TYPELIB_PATH="$HERE/usr/lib/girepository-1.0"
export GUILE_LOAD_PATH="$HERE/usr/share/$APP_NAME:$HERE/usr/share/guile/site:$GUILE_LOAD_PATH"
export GUILE_LOAD_COMPILED_PATH="$HERE/usr/share/$APP_NAME:$HERE/usr/lib/guile/ccache:$GUILE_LOAD_COMPILED_PATH"
exec "$HERE/usr/bin/GuileGTK4Prototyper" "$@"
EOF

chmod +x "$APP_DIR/AppRun"

echo "==> 6. Empacotando..."
APPIMAGETOOL="$DIST_DIR/appimagetool-x86_64.AppImage"
if [ ! -f "$APPIMAGETOOL" ]; then
    wget -q https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage -O "$APPIMAGETOOL"
    chmod +x "$APPIMAGETOOL"
fi

ARCH=x86_64 "$APPIMAGETOOL" --appimage-extract-and-run "$APP_DIR"

# Move o AppImage final para a pasta dist
mv Guile_GTK4_Prototyper-x86_64.AppImage "$DIST_DIR/" 2>/dev/null || true

echo "==> SUCESSO! Artefatos disponíveis em: $DIST_DIR"