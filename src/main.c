/*
 * src/main.c
 * Binário nativo que embute a VM do Guile.
 * Baseado no ambiente configurado pelas variáveis GUILE_LOAD_PATH.
 */

#include <libguile.h>
#include <stdlib.h>

static void inner_main(void *data, int argc, char **argv) {
    /* * Não usamos mais (add-to-load-path (getcwd)) aqui.
     * O AppRun já configura o GUILE_LOAD_PATH corretamente para o ambiente isolado.
     */
    scm_c_eval_string("(use-modules (g-golf) (src app))");
    scm_c_eval_string("(gi-import \"Gio\" #:version \"2.0\")");
    
    /* Inicializa a aplicação */
    scm_c_eval_string("(let ((app (create-app))) (g-application-run app (command-line)))");
}

int main(int argc, char **argv) {
    scm_boot_guile(argc, argv, inner_main, NULL);
    return 0;
}