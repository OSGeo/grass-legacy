/**************************************************************************
*                                                                        
*                          matrix_ops.c                                  
*                                                                        
*   m_add (MATRIX *a, MATRIX *b, MATRIX *c)
*       matrix addition (returns c = a + b)
*
*   m_copy (MATRIX *a, MATRIX *b)
*       matrix equivalency (return a = b).
*
*   m_inverse (MATRIX *a, MATRIX *b)
*       invert a square martix (puts pivot elements on main diagonal).
*       returns arg2 as the inverse of arg1.
*       This routine is based on a routine found in Andrei Rogers, "Matrix
*       Methods in Urban and Regional Analysis", (1971), pp. 143-153.
*
*   m_isnull (MATRIX *a)
*       returns 1 if matrix is null, else 0.
*
*   m_mult (MATRIX *a, MATRIX *b, MATRIX *c)
*       matrix multiplication (return c = a * b)
*
*   m_transpose (MATRIX *a, MATRIX *b)
*       returns arg2 as the transpose of arg1
*
*   m_zero (MATRIX *a)
*       returns arg2 zero filled 
*
**************************************************************************/

#include "matrix_ops.h"
#include <stdio.h>
#include <math.h>
#include "ortho_image.h"

#define EPSILON 1.0e-8
#define ZERO 1.0e-8


/*------------------------------------------------------------------------*/
/*
 * m_add: matrix addition (returns c = a + b)
 */
int m_add (MATRIX *a,MATRIX *b,MATRIX *c)
{
    register int nr, nc;
    char message[256];
    register double *ap, *bp, *mp;
    static MATRIX m;

    if (a->nrows == 0)
        return m_error ("+: arg1 not defined\n");
    else if (b->nrows == 0)
        return m_error ("+: arg2 not defined\n");

    /* check for conformity */
    if ((a->nrows != b->nrows) || (a->ncols != b->ncols))
    {
        sprintf (message, "+: matrices not conformable, %d x %d + %d x %d\n",
            a->nrows, a->ncols, b->nrows, b->ncols);
        return m_error (message);
    }

    nr = a->nrows;
    while (nr--)
    {
        nc = a->ncols;
        ap = &(a->x[nr][0]);
        bp = &(b->x[nr][0]);
        mp = &(m.x[nr][0]);
        while (nc--)
            *mp++ = *ap++ + *bp++;
    }
    m.nrows = a->nrows;
    m.ncols = a->ncols;
    m_copy (c, &m);
    return 1;
}

/*------------------------------------------------------------------------*/
/*
 * m_copy: matrix equivalency (return a = b).
 */
int m_copy (MATRIX *a,MATRIX *b)
{
    register int r, c;
    register double *ap, *bp;

    if (b->nrows == 0)
        return m_error ("=: arg2 not defined\n");

    r = b->nrows;
    a->nrows = b->nrows;
    a->ncols = b->ncols;
    while (r--)
    {
        c = b->ncols;
        ap = &(a->x[r][0]);
        bp = &(b->x[r][0]);
        while (c--)
            *ap++ = *bp++;
    }

    return 1;
}


/*------------------------------------------------------------------------*/
/*
 * inverse: invert a square martix (puts pivot elements on main diagonal).
 *          returns arg2 as the inverse of arg1.
 *
 *  This routine is based on a routine found in Andrei Rogers, "Matrix
 *  Methods in Urban and Regional Analysis", (1971), pp. 143-153.
 */
int m_inverse (MATRIX *a,MATRIX *b)
{
    int i, j, k, l, ir=0, ic=0, nr, nc;
    int ipivot[MAXROWS], itemp[MAXROWS][2];
    double pivot[MAXROWS], t;
    double fabs();
    static MATRIX m;

    if (a->nrows == 0)
        return matrix_error ("inv: arg1 not defined\n");

    if (a->nrows != a->ncols)
        return matrix_error ("inv: matrix not square\n");

    if (m_isnull (a))
    {
        /* fprintf (stderr, " inv: matrix is singular\n"); */
        return matrix_error ("inv: matrix is singular\n");
    } 
    m_copy (&m, a);
    nr = a->nrows;
    nc = a->ncols;

    /* initialization */
    for (i = 0; i < nr; i++)
        ipivot[i] = 0;

    for (i = 0; i < nr; i++)
    {
        t = 0.0;  /* search for pivot element */
        for (j = 0; j < nr; j++)
        {
            if (ipivot[j] == 1) /* found pivot */
                continue;
            for (k = 0; k < nc; k++)
                switch (ipivot[k]-1)
                {
                    case  0:
                        break;
                    case -1:
                        if (fabs (t) < fabs (m.x[j][k]))
                        {
                            ir = j;
                            ic = k;
                            t = m.x[j][k];
                        }
                        break;
                    case  1:
                        return matrix_error ("inv: matrix is singular\n");
                        break;
                    default: /* shouldn't get here */
                        return matrix_error ("inv: matrix is singular\n");
                        break;
                }
        }
        ipivot[ic] += 1;
        if (ipivot[ic] > 1) /* check for dependency */
            return matrix_error ("inv: matrix is singular\n");
        /* interchange rows to put pivot element on diagonal */
        if (ir != ic)
            for (l = 0; l < nc; l++)
            {
                t = m.x[ir][l];
                m.x[ir][l] = m.x[ic][l];
                m.x[ic][l] = t;
            }

        itemp[i][0] = ir;
        itemp[i][1] = ic;
        pivot[i] = m.x[ic][ic];

        /* check for zero pivot */
        if (fabs (pivot[i]) < EPSILON)
            return matrix_error ("inv: matrix is singular\n");

        /* divide pivot row by pivot element */
        m.x[ic][ic] = 1.0;
        for (j = 0; j < nc; j++)
            m.x[ic][j] /= pivot[i];

        /* reduce nonpivot rows */
        for (k = 0; k < nr; k++)
            if (k != ic)
            {
                t = m.x[k][ic];
                m.x[k][ic] = 0.0;
                for (l = 0; l < nc; l++)
                    m.x[k][l] -= (m.x[ic][l] * t);
            }
    }

    /* interchange columns */
    for (i = 0; i < nc; i++)
    {
        l = nc - i - 1;
        if (itemp[l][0] == itemp[l][1])
            continue;
        ir = itemp[l][0];
        ic = itemp[l][1];
        for (k = 0; k < nr; k++)
        {
            t = m.x[k][ir];
            m.x[k][ir] = m.x[k][ic];
            m.x[k][ic] = t;
        }
    }
    
    b->nrows = nr;
    b->ncols = nc;
    m_copy (b, &m);
    return 1;
}


/*------------------------------------------------------------------------*/
/*
 * m_isnull: returns 1 if matrix is null, else 0.
 */
int m_isnull (MATRIX *a)
{
    register int i, j, rows, cols;

    if (a->nrows == 0)
        return m_error ("m_isnull: argument not defined.\n");

    rows = a->nrows;
    cols = a->ncols;

    for (i = 0; i < rows; i++)
        for (j = 0; j < cols; j++)
            if ((fabs (a->x[i][j]) - ZERO) > ZERO)
                return 0;

    return 1;
}


/*------------------------------------------------------------------------*/
/*
 * m_mult: matrix multiplication (return c = a * b)
 */
int m_mult (MATRIX *a,MATRIX *b,MATRIX *c)
{
    register int i, j, k, nr, nc, ncols;
    char message[256];
    static MATRIX m;

    if (a->nrows == 0)
        return m_error ("*: arg1 not defined\n");
    else if (b->nrows == 0)
        return m_error ("*: arg2 not defined\n");

    /* check for conformity */
    if (a->ncols != b->nrows)
    {
        sprintf (message, "*: matrices not conformable, %d x %d * %d x %d\n",
            a->nrows, a->ncols, b->nrows, b->ncols);
        /* fprintf (stderr, message); */
        return m_error (message);
    }

    ncols = a->ncols;
    nr = a->nrows;
    nc = b->ncols;
    for (i = 0; i < nr; i++)
        for (j = 0; j < nc; j++)
        {
            m.x[i][j] = 0.0;
            for (k = 0; k < ncols; k++)
                m.x[i][j] += (a->x[i][k] * b->x[k][j]);
        }

    m.nrows = nr;
    m.ncols = nc;
    m_copy (c, &m);
    return 1;
}


/*------------------------------------------------------------------------*/
/*
 * transpose: returns arg2 as the transpose of arg1
 */
int m_transpose (MATRIX *a,MATRIX *b)
{
    register int i, j, nr, nc;
    static MATRIX m;

    if (a->nrows == 0)
        return m_error ("\': arg1 not defined\n");

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


/*------------------------------------------------------------------------*/
/*
 * zero: returns arg2 zero filled 
 */
int m_zero (MATRIX *a)
{
    register int i, j;
    char message[256];

    for (i = 0 ; i < a->nrows ; i++)
      for (j = 0; j < a->ncols; j++)
          a->x[i][j] = 0.0;

    return 1;
}

/*------------------------------------------------------------------------*/
