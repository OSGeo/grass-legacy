#include "mat.h"

/*
 * transpose: returns arg2 as the transpose of arg1
 */

transpose (a, b)
    MATRIX *a, *b;
{
    register int i, j, nr, nc;
    static MATRIX m;

    if (a->nrows == 0)
        return error ("\': arg1 not defined\n");

    nr = a->nrows;
    nc = a->ncols;

    for (i = 0; i < nr; i++)
        for (j = 0; j < nc; j++)
            m.x[j][i] = a->x[i][j];

    m.nrows = nc;
    m.ncols = nr;
    m_copy (b, &m);
    return 1;
}
