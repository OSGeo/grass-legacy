/**************************************************************************/
/*                                                                        */
/*                          matrix_ops.h                                  */
/*                                                                        */
/**************************************************************************/

#ifndef _matrix_defs.h
#define _matrix_defs.h


#define MAXROWS 25          /* max number of rows in a matrix */
#define MAXCOLS MAXROWS     /* max number of columns in a matrix */

        /* The MATRIX typedef */

typedef struct matrix {
    int nrows; /* row index */
    int ncols; /* col index */
    double x[MAXROWS][MAXCOLS];
} MATRIX;



       /* Matrix operations defs */
#ifdef _NO_PROTO
   m_add       ();
   m_copy      ();
   m_inverse   ();
   m_isnull    ();
   m_mult      ();
   m_transpose ();
   m_zero      ();
#else
   m_add       (MATRIX *a, MATRIX *b, MATRIX *c);
   m_copy      (MATRIX *a, MATRIX *b);
   m_inverse   (MATRIX *a, MATRIX *b);
   m_isnull    (MATRIX *a);
   m_mult      (MATRIX *a, MATRIX *b, MATRIX *c);
   m_transpose (MATRIX *a, MATRIX *b);
   m_zero      (MATRIX *a);
#endif  /* _NO_PROTO */

#endif  /* _matrix_defs.h */
