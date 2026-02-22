#include <libguile.h>
#include <locale.h>
#include <stdlib.h>
#include <unistd.h>
#include <linux/limits.h>
#include <libgen.h>

static void inner_main (void *closure, int argc, char **argv) {
    scm_c_eval_string("(load-compiled \"main.go\")");
}

int main (int argc, char **argv) {
    char path[PATH_MAX];
    readlink("/proc/self/exe", path, PATH_MAX);
    char *dir = dirname(path);

    setlocale(LC_ALL, "");

    // BLOQUEIA AUTO-COMPILAÇÃO: Essencial para standalone
    setenv("GUILE_AUTO_COMPILE", "0", 1);

    // Aponta para a pasta local para fontes e objetos
    setenv("GUILE_LOAD_PATH", dir, 1);
    setenv("GUILE_LOAD_COMPILED_PATH", dir, 1);

    scm_boot_guile(argc, argv, inner_main, 0);
    return 0;
}