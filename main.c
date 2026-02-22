#include <libguile.h>
#include <locale.h>

static void inner_main (void *closure, int argc, char **argv) {
    /* Carrega o arquivo compilado principal */
    scm_c_eval_string("(load-compiled \"main.go\")");
}

int main (int argc, char **argv) {
    setlocale(LC_ALL, ""); 
    scm_boot_guile(argc, argv, inner_main, 0);
    return 0;
}