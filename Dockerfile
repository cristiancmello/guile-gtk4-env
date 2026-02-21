FROM fedora:latest

# 1. Instalação de dependências e Drivers Gráficos (Sem pacotes X11 desnecessários)
RUN dnf update -y && dnf install -y \
    glibc-langpack-en \
    sudo curl wget git \
    gcc gcc-c++ make automake autoconf libtool pkgconfig texinfo \
    gettext-devel \
    guile30-devel \
    glib2-devel gobject-introspection-devel \
    gtk4-devel \
    vulkan-loader mesa-vulkan-drivers vulkan-tools \
    mesa-libGLES \
    libwayland-client libwayland-cursor libwayland-egl \
    libxkbcommon-devel \
    && dnf clean all

# 2. Configuração de Locale
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# 3. FIX: Links simbólicos para o Guile 3.0
RUN ln -s /usr/bin/guile3.0 /usr/bin/guile && \
    ln -s /usr/bin/guild3.0 /usr/bin/guild

# 4. Build do G-Golf (Savannah)
WORKDIR /tmp
RUN git clone git://git.savannah.gnu.org/g-golf.git && \
    cd g-golf && \
    ./autogen.sh && \
    ./configure --prefix=/usr --libdir=/usr/lib64 --with-guile-site=yes \
                GUILE=/usr/bin/guile3.0 GUILD=/usr/bin/guild3.0 && \
    make -j$(nproc) && \
    make install

# 5. Variáveis de Caminho do Guile
ENV GUILE_LOAD_PATH="/usr/share/guile/site/3.0"
ENV GUILE_EXTENSIONS_PATH="/usr/lib64/guile/3.0/extensions"
ENV LD_LIBRARY_PATH="/usr/lib64"

# 6. MODO WAYLAND + VULKAN (Sem X11)
ENV GDK_BACKEND=wayland
ENV GSK_RENDERER=vulkan
ENV GDK_DEBUG=vulkan
ENV QT_QPA_PLATFORM=wayland
ENV XDG_SESSION_TYPE=wayland

WORKDIR /root
CMD ["/bin/bash"]
