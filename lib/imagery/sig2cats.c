#include <grass/imagery.h>

int I_signature_to_cats( struct Signature *S, struct Categories *cats)
{
    int n;
    CELL c;

    G_init_cats (c = 0, S->title, cats);
    for (n = 0; n < S->nsigs; n++)
	G_set_cat (++c, S->sig[n].desc, cats);

    return 0;
}
