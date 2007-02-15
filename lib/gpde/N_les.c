
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:      functions to manage linear equation systems
* 		part of the gpde library
*               
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

#include "grass/N_pde.h"
#include <stdlib.h>

/* *************************************************************** */
/* *************************************************************** */
/* *************************************************************** */
/*!
 * \brief Allocate memory for a sparse vector
 *
 * \param cols int
 * \return N_spvector *
 *
 * */
N_spvector *N_alloc_spvector(int cols)
{
    N_spvector *spvector;

    G_debug(4, "Allocate memory for a sparse vector with %i cols\n", cols);

    spvector = (N_spvector *) G_calloc(1, sizeof(N_spvector));

    spvector->cols = cols;
    spvector->index = (int *)G_calloc(cols, sizeof(int));
    spvector->values = (double *)G_calloc(cols, sizeof(double));

    return spvector;
}


/* *************************************************************** */
/* *************************************************************** */
/* *************************************************************** */
/*!
 * \brief Alloacte memory for a linear equation system
 *
 * The type of the linear equation system must be N_NORMAL_LES for
 * a regular quadratic matrix or N_SPARSE_LES for a sparse matrix
 *
 * <p>
 * In case of N_NORMAL_LES
 * 
 * A quadratic matrix of size rows*rows*sizeof(double) will allocated
 *
 * <p>
 * In case of N_SPARSE_LES
 *
 * a vector of size row will be allocated, ready to hold additional allocated sparse vectors.
 * each sparse vector may have a different size.
 *
 * \param rows int
 * \param type int
 * \return N_les *
 *
 * */
N_les *N_alloc_les(int rows, int type)
{
    N_les *les;
    int i;

    if (type == N_SPARSE_LES)
	G_debug(2,
		"Allocate memory for a sparse linear equation system with %i rows\n",
		rows);
    else
	G_debug(2,
		"Allocate memory for a regular linear equation system with %i rows\n",
		rows);

    les = (N_les *) G_calloc(1, sizeof(N_les));

    les->x = (double *)G_calloc(rows, sizeof(double));
    les->b = (double *)G_calloc(rows, sizeof(double));
    les->A = NULL;
    les->Asp = NULL;
    les->rows = rows;

    if (type == N_SPARSE_LES) {
	les->Asp = (N_spvector **) G_calloc(rows, sizeof(N_spvector *));
	les->type = N_SPARSE_LES;
    }
    else {
	les->A = (double **)G_calloc(rows, sizeof(double *));

	for (i = 0; i < rows; i++)
	    les->A[i] = (double *)G_calloc(rows, sizeof(double));

	les->type = N_NORMAL_LES;
    }

    return les;
}


/* *************************************************************** */
/* *************************************************************** */
/* *************************************************************** */
/*!
 * \brief Adds a sparse vector to a sparse linear equation system at position row
 *
 * Return 1 for success and -1 for failure
 *
 * \param les N_les *
 * \param spvector N_spvector * 
 * \param row int
 * \return int
 *
 * */
int N_add_spvector_to_les(N_les * les, N_spvector * spvector, int row)
{


    if (les != NULL) {
	if (les->type != N_SPARSE_LES)
	    return -1;

	if (les->rows > row) {
	    G_debug(5,
		    "Add sparse vector %p to the sparse linear equation system at row %i\n",
		    spvector, row);
	    les->Asp[row] = spvector;
	}
	else
	    return -1;

    }
    else {
	return -1;
    }


    return 1;
}

/* *************************************************************** */
/* *************************************************************** */
/* *************************************************************** */
/*!
 *
 * \brief prints the linear equation system to stdout
 *
 * <p>
 * Format:
 * A*x = b
 *
 * <p>
 * Example
 \verbatim
 
 2 1 1 1 * 2 = 0.1
 1 2 0 0 * 3 = 0.2
 1 0 2 0 * 3 = 0.2
 1 0 0 2 * 2 = 0.1
 
 \endverbatim
 *
 * \param les N_les * 
 * \return void
 *  
 * */
void N_print_les(N_les * les)
{
    int i, j, k, out;


    if (les->type == N_SPARSE_LES) {
	for (i = 0; i < les->rows; i++) {
	    for (j = 0; j < les->rows; j++) {
		out = 0;
		for (k = 0; k < les->Asp[i]->cols; k++) {
		    if (les->Asp[i]->index[k] == j) {
			fprintf(stdout, "%4.5f ", les->Asp[i]->values[k]);
			out = 1;
		    }
		}
		if (!out)
		    fprintf(stdout, "%4.5f ", 0.0);
	    }
	    fprintf(stdout, "  *  %4.5f  =  %4.5f \n", les->x[i], les->b[i]);
	}
    }
    else {

	for (i = 0; i < les->rows; i++) {
	    for (j = 0; j < les->rows; j++) {
		fprintf(stdout, "%4.5f ", les->A[i][j]);
	    }
	    fprintf(stdout, "  *  %4.5f  =  %4.5f \n", les->x[i], les->b[i]);
	}

    }
    return;
}

/* *************************************************************** */
/* *************************************************************** */
/* *************************************************************** */
/*!
 * \brief Release the memory of the sparse vector
 *
 * \param spvector N_spvector *
 * \return void
 *
 * */
void N_free_spvector(N_spvector * spvector)
{
    if (spvector) {
	if (spvector->values)
	    G_free(spvector->values);
	if (spvector->index)
	    G_free(spvector->index);
	G_free(spvector);

	spvector = NULL;
    }

    return;
}


/* *************************************************************** */
/* *************************************************************** */
/* *************************************************************** */
/*!
 * \brief Release the memory of the linear equation system
 *
 * \param les N_les *            
 * \return void
 *
 * */

void N_free_les(N_les * les)
{
    int i;

    if (les->type == N_SPARSE_LES)
	G_debug(2, "Releasing memory of a sparse linear equation system\n");
    else
	G_debug(2, "Releasing memory of a regular linear equation system\n");

    if (les) {

	if (les->x)
	    G_free(les->x);
	if (les->b)
	    G_free(les->b);

	if (les->type == N_SPARSE_LES) {

	    if (les->Asp) {
		for (i = 0; i < les->rows; i++)
		    if (les->Asp[i])
			N_free_spvector(les->Asp[i]);

		G_free(les->Asp);
	    }
	}
	else {

	    if (les->A) {
		for (i = 0; i < les->rows; i++)
		    if (les->A[i])
			G_free(les->A[i]);

		G_free(les->A);
	    }
	}

	free(les);
    }

    return;
}
