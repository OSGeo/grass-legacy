
#include "V_.h"


Vect__get_window (Map, n, s, e, w)
    struct Map_info *Map;
    double *n, *s, *e, *w;
{
    struct dig_head *dhead;

    dhead = &(Map->head);

    *n = dhead->N;
    *s = dhead->S;
    *e = dhead->E;
    *w = dhead->W;

    return (0);
}
