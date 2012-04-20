/*
 * hash_set.c
 */

#include "hash_set.h"

/*
 * The following function is a dummy one; replace it for
 * your C function definitions.
 */

ScmObj test_hash_set(void)
{
    return SCM_MAKE_STR("hash_set is working");
}

/*
 * Module initialization function.
 */
extern void Scm_Init_hash_setlib(ScmModule*);

void Scm_Init_hash_set(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(hash_set);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("hash-set", TRUE));

    /* Register stub-generated procedures */
    Scm_Init_hash_setlib(mod);
}
