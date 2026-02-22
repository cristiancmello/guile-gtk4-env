# Notas de Manutenção

## Visão Geral

Este Dockerfile constrói um ambiente **Fedora + Guile 3.0 + GTK4 + G-Golf**.  
O Fedora tem ciclo de vida de ~13 meses por versão. O processo de upgrade é
simples mas requer atenção aos pins de versão das dependências do G-Golf.

---

## Upgrade de Versão do Fedora

### 1. Atualizar a imagem base

No `Dockerfile`, altere a linha:

```dockerfile
FROM fedora:43
```

Para a nova versão (ex: `FROM fedora:44`).

---

### 2. Descobrir as novas versões dos pacotes pinados

Para cada pacote pinado, consulte a versão disponível na nova release.  
O jeito mais direto é rodar um container temporário:

```bash
podman run --rm fedora:44 dnf info \
    guile30 \
    glib2-devel \
    gobject-introspection \
    gtk3-devel \
    gtk4-devel \
    libadwaita \
    glibc-langpack-en \
    | grep -E "^Name|^Version|^Release"
```

---

### 3. Verificar conformidade com os mínimos do G-Golf

Compare as versões encontradas com os mínimos exigidos pelo G-Golf.  
A tabela de referência está no cabeçalho do `Dockerfile` e também aqui:

| Dependência           | Mínimo G-Golf 0.8.3 |
|-----------------------|---------------------|
| Guile                 | >= 3.0.7            |
| GLib-2.0 / GObject    | >= 2.73.0           |
| GObject-Introspection | >= 1.72.0           |
| GTK3 (test-suite)     | >= 3.10.0           |
| GTK4 (exemplos)       | >= 4.8.0            |
| Adw-1 (ex. Adwaita)   | >= 1.8.0            |

Se alguma versão do novo Fedora ficar **abaixo** do mínimo exigido
(improvável, mas possível em casos de regressão de empacotamento),
a solução é compilar essa dependência do fonte no próprio Dockerfile,
assim como já se faz com o G-Golf.

---

### 4. Atualizar os pins no Dockerfile

Substitua as versões antigas pelas novas em cada pacote pinado.  
Exemplo para o Guile:

```dockerfile
# antes
guile30-3.0.9-5.fc43 \
guile30-devel-3.0.9-5.fc43 \

# depois
guile30-3.0.9-6.fc44 \
guile30-devel-3.0.9-6.fc44 \
```

Pacotes atualmente pinados no Dockerfile:

| Pacote                       | Versão pinada (fc43)   |
|------------------------------|------------------------|
| `guile30`                    | 3.0.9-5.fc43           |
| `guile30-devel`              | 3.0.9-5.fc43           |
| `glib2-devel`                | 2.86.4-1.fc43          |
| `gobject-introspection`      | 1.84.0-3.fc43          |
| `gobject-introspection-devel`| 1.84.0-3.fc43          |
| `gtk3-devel`                 | 3.24.51-2.fc43         |
| `gtk4-devel`                 | 4.20.2-1.fc43          |
| `libadwaita`                 | 1.8.4-1.fc43           |
| `libadwaita-devel`           | 1.8.4-1.fc43           |

Pacotes **sem pin** (acompanham a versão do Fedora automaticamente):
`pciutils`, `wget`, `git`, `gcc`, `gcc-c++`, `make`, `automake`,
`autoconf`, `libtool`, `pkgconfig`, `texinfo`, `gettext-devel`,
`vulkan-loader`, `mesa-vulkan-drivers`, `vulkan-tools`, `mesa-libGLES`,
`libwayland-client`, `libwayland-cursor`, `libwayland-egl`,
`libxkbcommon-devel`, `glibc-langpack-en`, `glibc-locale-source`.

---

### 5. Atualizar a versão do G-Golf (quando necessário)

O G-Golf é fixado via `ARG` no `Dockerfile`:

```dockerfile
ARG GGOLF_REF=v0.8.3
```

Ao atualizar para uma nova versão do G-Golf, verifique o `NEWS` ou
`install.html` do release para checar se os mínimos de dependências mudaram
e atualize a tabela acima de acordo.

O anúncio de cada release é publicado em:
- https://www.mail-archive.com/info-gnu@gnu.org/
- https://www.gnu.org/software/g-golf/

---

### 6. Testar o build

```bash
podman build -t g-golf:fc44 .
```

Em caso de erro de dependência não encontrada, o DNF reportará claramente
qual pacote ou versão está ausente.

---

## Dependência Ausente: Guile-Lib

O pacote `guile-lib` (>= 0.2.5, exigido pela test-suite do G-Golf) foi
removido do Fedora desde o F29 e não foi reintroduzido.

Para rodar `make check`, é necessário compilá-lo do fonte antes do G-Golf:

```bash
wget https://download.savannah.nongnu.org/releases/guile-lib/guile-lib-0.2.7.tar.gz
tar xzf guile-lib-0.2.7.tar.gz
cd guile-lib-0.2.7
./configure --prefix=/usr
make && make install
```

---

## Calendário de Releases do Fedora

| Versão | EOL aproximado |
|--------|----------------|
| F42    | Mai 2026       |
| F43    | Nov 2026       |
| F44    | Mai 2027       |

Fonte: https://fedoraproject.org/wiki/Fedora_Release_Life_Cycle