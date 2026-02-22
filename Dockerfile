FROM fedora:43

ARG GGOLF_REPO=https://git.savannah.gnu.org/git/g-golf.git
ARG GGOLF_REF=v0.8.3

RUN dnf update -y && \
    dnf install -y glibc-langpack-en glibc-locale-source && \
    localedef -i en_US -f UTF-8 en_US.UTF-8 && \
    dnf install -y --setopt=install_weak_deps=False --setopt=tsflags=nodocs \
        pciutils wget git \
        gcc gcc-c++ make automake autoconf libtool pkgconfig texinfo \
        guile30-3.0.9-5.fc43 guile30-devel-3.0.9-5.fc43 \
        glib2-devel-2.86.4-1.fc43 gettext-devel \
        gobject-introspection-1.84.0-3.fc43 gobject-introspection-devel-1.84.0-3.fc43 \
        gtk3-devel-3.24.51-2.fc43 \
        gtk4-devel-4.20.2-1.fc43 \
        libadwaita-1.8.4-1.fc43 libadwaita-devel-1.8.4-1.fc43 \
        vulkan-loader mesa-vulkan-drivers vulkan-tools \
        mesa-libGLES libwayland-client libwayland-cursor libwayland-egl libxkbcommon-devel \
    && dnf clean all \
    && rm -rf /var/cache/dnf

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

RUN ln -sf /usr/bin/guile3.0 /usr/bin/guile && \
    ln -sf /usr/bin/guild3.0  /usr/bin/guild

RUN git clone --branch "${GGOLF_REF}" "${GGOLF_REPO}" /tmp/g-golf && \
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
ENV GUILE_EXTENSIONS_PATH="/usr/lib64/guile/3.0/extensions"

# Sanity check: verify Guile binary, basic execution, and G-Golf import
RUN echo "=== Guile sanity check ===" && \
    guile --version && \
    guile -c "(display \"Guile OK\n\")" && \
    guile -c "(use-modules (g-golf)) (display \"G-Golf OK\n\")" && \
    echo "=== Sanity check passed ==="

# Sanity check: verify GLib2 headers, pkg-config metadata, and runtime version
RUN echo "=== GLib2 sanity check ===" && \
    pkg-config --modversion glib-2.0 && \
    pkg-config --atleast-version=2.73.0 glib-2.0 && \
    echo "GLib2 version OK (>= 2.73.0)" && \
    echo "#include <glib.h>" | gcc -x c - -c -o /dev/null $(pkg-config --cflags glib-2.0) && \
    echo "GLib2 headers OK" && \
    guile -c "(use-modules (g-golf glib)) (display \"GLib2 via G-Golf OK\n\")" && \
    echo "=== Sanity check passed ==="

# Sanity check: verify gettext via Guile's i18n module
RUN echo "=== Gettext sanity check ===" && \
    guile -c " \
      (use-modules (ice-9 i18n)) \
      (display \"ice-9 i18n OK\n\") \
      (setlocale LC_ALL \"en_US.UTF-8\") \
      (display (string-append \"locale set to: \" (setlocale LC_ALL) \"\n\")) \
      (bindtextdomain \"test\" \"/tmp\") \
      (textdomain \"test\") \
      (display \"bindtextdomain/textdomain OK\n\") \
    " && \
    echo "=== Sanity check passed ==="

# Sanity check: verify GObject Introspection via G-Golf's irepository bindings
RUN echo "=== GObject Introspection sanity check ===" && \
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
    echo "=== Sanity check passed ==="

# Sanity check: verify GTK4 typelib loads and core classes introspect correctly
# Note: widgets cannot be instantiated without a display; this check is limited
# to typelib loading and class introspection only.
RUN echo "=== GTK4 sanity check ===" && \
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
      (display \"GtkApplication introspected OK\n\") \
      (display \"GtkApplicationWindow introspected OK\n\") \
      (display \"GtkBox introspected OK\n\") \
      (display \"GtkLabel introspected OK\n\") \
      (display \"GtkButton introspected OK\n\") \
    " && \
    echo "=== Sanity check passed ==="

# Sanity check: verify Adwaita typelib loads and core classes introspect correctly
RUN echo "=== Adwaita sanity check ===" && \
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
      (display \"AdwApplication introspected OK\n\") \
      (display \"AdwApplicationWindow introspected OK\n\") \
      (display \"AdwHeaderBar introspected OK\n\") \
      (display \"AdwToolbarView introspected OK\n\") \
    " && \
    echo "=== Sanity check passed ==="

# Sanity check: verify Vulkan loader, ICD, and headers are present
# Note: GPU enumeration requires a real device; this check validates the
# loader and software ICD (lavapipe) which is available in mesa-vulkan-drivers.
RUN echo "=== Vulkan sanity check ===" && \
    vulkaninfo --summary 2>&1 | grep -E "Vulkan Instance|apiVersion|driverVersion|deviceName|deviceType" && \
    echo "Vulkan loader OK" && \
    pkg-config --modversion vulkan && \
    pkg-config --atleast-version=1.3.0 vulkan && \
    echo "Vulkan headers OK (>= 1.3.0)" && \
    echo "=== Sanity check passed ==="

ENV GDK_BACKEND=wayland
ENV GSK_RENDERER=vulkan
ENV GDK_DEBUG=vulkan
ENV QT_QPA_PLATFORM=wayland
ENV XDG_SESSION_TYPE=wayland

RUN useradd -m -s /bin/bash dev && \
    echo "dev ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers.d/dev

USER dev
WORKDIR /home/dev

CMD ["/bin/bash"]