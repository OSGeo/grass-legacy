/**************************************************************************/
/*                                                                        */
/*                          matrix_ops.h                                  */
/*                                                                        */
/**************************************************************************/

#ifndef _matrix_defs_h
#define _matrix_defs_h


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
   int m_add       ();
   int m_copy      ();
   int m_inverse   ();
   int m_isnull    ();
   int m_mult      ();
   int m_transpose ();
   int m_zero      ();
#else
   int m_add       (MATRIX *a, MATRIX *b, MATRIX *c);
   int m_copy      (MATRIX *a, MATRIX *b);
   int m_inverse   (MATRIX *a, MATRIX *b);
   int m_isnull    (MATRIX *a);
   int m_mult      (MATRIX *a, MATRIX *b, MATRIX *c);
   int m_transpose (MATRIX *a, MATRIX *b);
   int m_zero      (MATRIX *a);
   int m_error     (char *);
#endif  /* _NO_PROTO */

#endif  /* _matrix_defs.h */
