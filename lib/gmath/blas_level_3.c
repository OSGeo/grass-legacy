
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2007
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:      linear equation system solvers
* 		part of the gpde library
*               
* COPYRIGHT:    (C) 2007 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

#include <math.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "grass/gmath.h"
#include <grass/gis.h>


/*!
 * \brief Add two matrices and scale matrix A with the scalar a
 *
 * \f[ {\bf C} = a {\bf A} + {\bf B} \f]
 *
 * In case B == NULL, matrix A will be scaled by scalar a. \n
 * In case a == 1.0, a simple matrix addition is performed. \n
 * In case a == -1.0 matrix A is substracted from matrix B. \n
 * The result is written into matrix C. 
 *
 *
 * This function is multi-threaded with OpenMP and can be called within a parallel OpenMP region.
 *
 * \param A (double **)
 * \param B (double **) if NULL, matrix A is scaled by scalar a only
 * \param a (double)
 * \param C (double **)
 * \param rows (int)
 * \param cols (int)
 * \return (void) 
 *
 * */
void G_math_d_aA_B(double **A, double **B, double a, double **C, int rows,
		   int cols)
{
    int i, j;


    /*If B is null, scale the matrix A with th scalar a */
    if (B == NULL) {
#pragma omp for schedule (static) private(i, j)
	for (i = rows - 1; i >= 0; i--)
	    for (j = cols - 1; j >= 0; j--)
		C[i][j] = a * A[i][j];

	return;
    }

    /*select special cases */
    if (a == 1.0) {
#pragma omp for schedule (static) private(i, j)
	for (i = rows - 1; i >= 0; i--)
	    for (j = cols - 1; j >= 0; j--)
		C[i][j] = A[i][j] + B[i][j];
    }
    else if (a == -1.0) {
#pragma omp for schedule (static) private(i, j)
	for (i = rows - 1; i >= 0; i--)
	    for (j = cols - 1; j >= 0; j--)
		C[i][j] = B[i][j] - A[i][j];
    }
    else {
#pragma omp for schedule (static) private(i, j)
	for (i = rows - 1; i >= 0; i--)
	    for (j = cols - 1; j >= 0; j--)
		C[i][j] = a * A[i][j] + B[i][j];
    }

    return;
}

/*!
 * \brief Add two matrices and scale matrix A with the scalar a
 *
 * \f[ {\bf C} = a {\bf A} + {\bf B} \f]
 *
 * In case B == NULL, matrix A will be scaled by scalar a. \n
 * In case a == 1.0, a simple matrix addition is performed. \n
 * In case a == -1.0 matrix A is substracted from matrix B. \n
 * The result is written into matrix C. 
 *
 *
 *
 * This function is multi-threaded with OpenMP and can be called within a parallel OpenMP region.
 *
 * \param A (float **)
 * \param B (float **) if NULL, matrix A is scaled by scalar a only
 * \param a (float)
 * \param C (float **) 
 * \param rows (int)
 * \param cols (int)

 * \return  (void) 
 *
 * */
void G_math_f_aA_B(float **A, float **B, float a, float **C, int rows,
		   int cols)
{
    int i, j;

    /*If B is null, scale the matrix A with th scalar a */
    if (B == NULL) {
#pragma omp for schedule (static) private(i, j)
	for (i = rows - 1; i >= 0; i--)
	    for (j = cols - 1; j >= 0; j--)
		C[i][j] = a * A[i][j];
	return;
    }

    /*select special cases */
    if (a == 1.0) {
#pragma omp for schedule (static) private(i, j)
	for (i = rows - 1; i >= 0; i--)
	    for (j = cols - 1; j >= 0; j--)
		C[i][j] = A[i][j] + B[i][j];
    }
    else if (a == -1.0) {
#pragma omp for schedule (static) private(i, j)
	for (i = rows - 1; i >= 0; i--)
	    for (j = cols - 1; j >= 0; j--)
		C[i][j] = B[i][j] - A[i][j];
    }
    else {
#pragma omp for schedule (static) private(i, j)
	for (i = rows - 1; i >= 0; i--)
	    for (j = cols - 1; j >= 0; j--)
		C[i][j] = a * A[i][j] + B[i][j];
    }

    return;
}


/*!
 * \brief Matrix multiplication
 *
 * \f[ {\bf C} = {\bf A}{\bf B} \f]
 *
 * The result is written into matrix C. 
 *
 * A must be of size rows_A * cols_A
 * B must be of size rows_B * cols_B with rows_B == cols_A
 * C must be of size rows_A * rows_B
 *
 *
 * This function is multi-threaded with OpenMP and can be called within a parallel OpenMP region.
 *
 * \param A (double **)
 * \param B (double **)
 * \param C (double **)
 * \param rows_A (int)
 * \param cols_A (int)
 * \param rows_B (int)
 * \return (void)
 *
 * */
void G_math_d_AB(double **A, double **B, double **C, int rows_A,
		 int cols_A, int rows_B)
{
    int i, j, k;

#pragma omp for schedule (static) private(i, j, k)
    for (i = 0; i < rows_A; i++) {
	for (j = 0; j < rows_B; j++) {
	    C[i][j] = 0.0;
	    for (k = cols_A - 1; k >= 0; k--) {
		C[i][j] += A[i][k] * B[k][j];
	    }
	}
    }

    return;
}

/*!
 * \brief Matrix multiplication
 *
 * \f[ {\bf C} = {\bf A}{\bf B} \f]
 *
 * The result is written into matrix C. 
 *
 * A must be of size rows_A * cols_A
 * B must be of size rows_B * cols_B with rows_B == cols_A
 * C must be of size rows_A * rows_B
 *
 *
 * This function is multi-threaded with OpenMP and can be called within a parallel OpenMP region.
 *
 * \param A (float **)
 * \param B (float **) 
 * \param D (float **) 
 * \param rows_A (int)
 * \param cols_A (int)
 * \param rows_B (int)
 * \return (void)
 *
 * */
void G_math_f_AB(float **A, float **B, float **C, int rows_A,
		 int cols_A, int rows_B)
{
    int i, j, k;

#pragma omp for schedule (static) private(i, j, k)
    for (i = 0; i < rows_A; i++) {
	for (j = 0; j < rows_B; j++) {
	    C[i][j] = 0.0;
	    for (k = cols_A - 1; k >= 0; k--) {
		C[i][j] += A[i][k] * B[k][j];
	    }
	}
    }

    return;
}
