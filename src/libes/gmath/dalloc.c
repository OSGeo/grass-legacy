#include "gis.h"
#include <stdlib.h>


/*!
 * \brief memory allocation
 *
 * Allocate a 
 * vector (array) of <b>n</b> doubles initialized to zero.
 *
 *  \param n
 *  \return double * 
 */

 double *G_alloc_vector(int n)
{
    return (double *) G_calloc (n, sizeof(double));
}


/*!
 * \brief memory allocation
 *
 * Allocate a matrix of <b>rows</b> by <b>cols</b> doubles initialized
 * to zero.
 *
 *  \param rows
 *  \param cols
 *  \return double ** 
 */

 double **G_alloc_matrix( int rows,int cols)
{
    double **m;
    int i;

    m = (double **) G_calloc (rows, sizeof(double *));
    m[0] = (double *) G_calloc (rows*cols, sizeof(double));
    for (i = 1; i < rows; i++)
	m[i] = m[i-1] + cols;
    return m;
}


/*!
 * \brief memory allocation
 *
 * Allocate a
 * vector (array) of <b>n</b> floats initialized to zero.
 *
 *  \param n
 *  \return float * 
 */

 float *G_alloc_fvector(int n)
{
    return (float *) G_calloc (n, sizeof(float));
}


/*!
 * \brief memory allocation
 *
 * Allocate a matrix of <b>rows</b> by <b>cols</b> floats initialized
 * to zero.
 *
 *  \param rows
 *  \param cols
 *  \return float ** 
 */

 float **G_alloc_fmatrix( int rows,int cols)
{
    float **m;
    int i;

    m = (float **) G_calloc (rows, sizeof(float *));
    m[0] = (float *) G_calloc (rows*cols, sizeof(float));
    for (i = 1; i < rows; i++)
	m[i] = m[i-1] + cols;
    return m;
}


/*!
 * \brief memory deallocation
 *
 * Deallocate a
 * vector (array) of doubles or floats.
 *
 *  \param v
 *  \return int
 */

 int G_free_vector(double *v)
{
    free (v);
    return 0;
}


/*!
 * \brief memory deallocation
 *
 * Deallocate 
 * a matrix of doubles.
 *
 *  \param m
 *  \return int
 */

 int G_free_matrix( double **m)
{
    free (m[0]);
    free (m);
    return 0;
}


/*!
 * \brief memory deallocation
 *
 * Deallocate
 * a matrix of floats.
 *
 *  \param m
 *  \return int
 */

 int G_free_fmatrix(float **m)
{
    free (m[0]);
    free (m);
    return 0;
}
