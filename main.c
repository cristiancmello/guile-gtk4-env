#include <libguile.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <linux/limits.h>
#include <libgen.h>

static void inner_main (void *data, int argc, char **argv) {
    char *base_dir = (char *)data;

    // Se houver argumento -c (para os seus scripts de teste)
    if (argc >= 3 && strcmp(argv[1], "-c") == 0) {
        scm_c_eval_string(argv[2]);
        return;
    }

    // Carrega o app principal
    char cmd[PATH_MAX + 128];
    snprintf(cmd, sizeof(cmd), "(load-compiled \"%s/ccache/main.go\")", base_dir);
    scm_c_eval_string(cmd);

    // Executa a função main do Scheme
    scm_c_eval_string("(let ((m (resolve-interface '(main)))) "
                      "  ((module-ref m 'main) '(\"meu-app\")))");
}

int main (int argc, char **argv) {
    char path[PATH_MAX];
    ssize_t len = readlink("/proc/self/exe", path, sizeof(path) - 1);
    if (len == -1) return 1;
    path[len] = '\0';
    
    // base_dir será a pasta 'dist/'
    char *base_dir = strdup(dirname(path));

    setlocale(LC_ALL, "");

    // --- CONFIGURAÇÃO DE AMBIENTE ---
    // Forçamos o Guile a olhar APENAS para dentro da nossa pasta dist/
    setenv("GUILE_AUTO_COMPILE", "0", 1);
    setenv("GUILE_LOAD_PATH", base_dir, 1);
    
    char ccache_path[PATH_MAX];
    snprintf(ccache_path, sizeof(ccache_path), "%s/ccache", base_dir);
    setenv("GUILE_LOAD_COMPILED_PATH", ccache_path, 1);
    
    // Importante para o Guile 3.0 não tentar buscar no /usr do host
    setenv("GUILE_SYSTEM_PATH", base_dir, 1);
    
    // Bibliotecas e Typelibs
    setenv("LD_LIBRARY_PATH", base_dir, 1);
    setenv("GI_TYPELIB_PATH", base_dir, 1);

    scm_boot_guile(argc, argv, inner_main, base_dir);
    
    free(base_dir);
    return 0;
}