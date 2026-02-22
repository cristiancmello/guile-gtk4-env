FROM fedora:43

ARG GGOLF_REPO=https://git.savannah.gnu.org/git/g-golf.git
ARG GGOLF_REF=v0.8.3

ARG GUILE_VER=3.0.9-5.fc43
ARG GLIB2_VER=2.86.4-1.fc43
ARG GOBJECT_VER=1.84.0-3.fc43
ARG GTK3_VER=3.24.51-2.fc43
ARG GTK4_VER=4.20.2-1.fc43
ARG ADWAITA_VER=1.8.4-1.fc43

RUN dnf update -y && \
    dnf install -y glibc-langpack-en glibc-locale-source && \
    localedef -i en_US -f UTF-8 en_US.UTF-8 && \
    dnf install -y --setopt=install_weak_deps=False --setopt=tsflags=nodocs \
        pciutils wget git file \
        gcc gcc-c++ make automake autoconf libtool pkgconfig texinfo \
        guile30-${GUILE_VER} guile30-devel-${GUILE_VER} \
        glib2-devel-${GLIB2_VER} gettext-devel \
        gobject-introspection-${GOBJECT_VER} \
        gobject-introspection-devel-${GOBJECT_VER} \
        gtk3-devel-${GTK3_VER} \
        gtk4-devel-${GTK4_VER} \
        libadwaita-${ADWAITA_VER} libadwaita-devel-${ADWAITA_VER} \
        vulkan-loader vulkan-tools \
        libwayland-client libwayland-cursor libwayland-egl \
        libxkbcommon-devel \
    && dnf clean all \
    && rm -rf /var/cache/dnf

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

RUN ln -sf /usr/bin/guile3.0 /usr/bin/guile && \
    ln -sf /usr/bin/guild3.0  /usr/bin/guild

RUN set -e && \
    git clone --depth 1 --branch "${GGOLF_REF}" "${GGOLF_REPO}" /tmp/g-golf && \
    cd /tmp/g-golf && \
    ./autogen.sh && \
    ./configure \
        --prefix=/usr \
        --libdir=/usr/lib64 \
        --with-guile-site=yes \
        GUILE=/usr/bin/guile3.0 \
        GUILD=/usr/bin/guild3.0 && \
    make -j"$(nproc)" && \
    make install && \
    cd / && rm -rf /tmp/g-golf && \
    ldconfig

ENV GUILE_LOAD_PATH="/usr/share/guile/site/3.0"
ENV GUILE_LOAD_COMPILED_PATH="/usr/lib64/guile/3.0/ccache"
ENV GUILE_EXTENSIONS_PATH="/usr/lib64/guile/3.0/extensions"

RUN set -e && \
    \
    echo "=== Guile ===" && \
    guile --version && \
    guile -c "(display \"Guile OK\n\")" && \
    guile -c "(use-modules (g-golf)) (display \"G-Golf OK\n\")" && \
    \
    echo "=== GLib2 ===" && \
    pkg-config --modversion glib-2.0 && \
    pkg-config --atleast-version=2.73.0 glib-2.0 && \
    echo "GLib2 version OK (>= 2.73.0)" && \
    echo "#include <glib.h>" | gcc -x c - -c -o /dev/null $(pkg-config --cflags glib-2.0) && \
    echo "GLib2 headers OK" && \
    guile -c "(use-modules (g-golf glib)) (display \"GLib2 via G-Golf OK\n\")" && \
    \
    echo "=== Gettext / i18n ===" && \
    guile -c " \
      (use-modules (ice-9 i18n)) \
      (display \"ice-9 i18n OK\n\") \
      (setlocale LC_ALL \"en_US.UTF-8\") \
      (display (string-append \"locale: \" (setlocale LC_ALL) \"\n\")) \
      (bindtextdomain \"test\" \"/tmp\") \
      (textdomain \"test\") \
      (display \"bindtextdomain/textdomain OK\n\") \
    " && \
    \
    echo "=== GObject Introspection ===" && \
    guile -c " \
      (use-modules (g-golf)) \
      (display \"g-golf OK\n\") \
      (g-irepository-require \"GObject\" #:version \"2.0\") \
      (display \"GObject-2.0 typelib loaded OK\n\") \
      (let* ((n   (g-irepository-get-n-infos \"GObject\")) \
             (ver (g-irepository-get-version \"GObject\"))) \
        (display (string-append \"GObject version: \" ver \"\n\")) \
        (display (string-append \"GObject entries: \" (number->string n) \"\n\"))) \
    " && \
    \
    echo "=== GTK4 ===" && \
    pkg-config --modversion gtk4 && \
    pkg-config --atleast-version=4.8.0 gtk4 && \
    echo "GTK4 version OK (>= 4.8.0)" && \
    guile -c " \
      (use-modules (oop goops)) \
      (default-duplicate-binding-handler \
        '(merge-generics replace warn-override-core warn last)) \
      (use-modules (g-golf)) \
      (g-irepository-require \"Gtk\" #:version \"4.0\") \
      (display \"Gtk-4.0 typelib loaded OK\n\") \
      (for-each (lambda (name) (gi-import-by-name \"Gtk\" name)) \
                '(\"Application\" \"ApplicationWindow\" \"Box\" \"Label\" \"Button\")) \
      (display \"GTK4 classes introspected OK\n\") \
    " && \
    \
    echo "=== Adwaita ===" && \
    pkg-config --modversion libadwaita-1 && \
    pkg-config --atleast-version=1.8.0 libadwaita-1 && \
    echo "Adwaita version OK (>= 1.8.0)" && \
    guile -c " \
      (use-modules (oop goops)) \
      (default-duplicate-binding-handler \
        '(merge-generics replace warn-override-core warn last)) \
      (use-modules (g-golf)) \
      (g-irepository-require \"Adw\" #:version \"1\") \
      (display \"Adw-1 typelib loaded OK\n\") \
      (for-each (lambda (name) (gi-import-by-name \"Adw\" name)) \
                '(\"Application\" \"ApplicationWindow\" \"HeaderBar\" \"ToolbarView\")) \
      (display \"Adwaita classes introspected OK\n\") \
    " && \
    \
    echo "=== Vulkan ===" && \
    echo "Skipping vulkaninfo hardware check during build (drivers will be injected by distrobox at runtime)" && \
    pkg-config --modversion vulkan && \
    pkg-config --atleast-version=1.3.0 vulkan && \
    echo "Vulkan headers OK (>= 1.3.0)" && \
    \
    echo "=== Todos os sanity checks passaram ==="

ENV GDK_BACKEND=wayland
ENV GSK_RENDERER=vulkan
ENV QT_QPA_PLATFORM=wayland
ENV XDG_SESSION_TYPE=wayland

CMD ["/bin/bash"]