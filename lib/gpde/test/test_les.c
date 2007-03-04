
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:      Unit tests for les creation
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

#include <grass/gis.h>
#include <grass/glocale.h>
#include <grass/N_pde.h>
#include "test_gpde_lib.h"


/* prototypes */
int test_les();

/* *************************************************************** */
/* Perfrome the les creation tests ******************************* */
/* *************************************************************** */
int unit_test_les_creation()
{
    int sum = 0;

    G_message(_("\n++ Running les creation unit tests ++"));

    sum += test_les();

    if (sum > 0)
	G_warning(_("\n-- les creation unit tests failure --"));
    else
	G_message(_("\n-- les creation unit tests finished successfully --"));

    return sum;
}


/* *************************************************************** */
/* test the les creation of normal and sparse matirces *********** */
/* *************************************************************** */
int test_les()
{
    N_spvector *spvector;
    N_les *les;
    N_les *sples;
    int i, j;

    G_message(_("\t * testing les creation in parallel\n"));

    les = N_alloc_les(TEST_N_NUM_ROWS, N_NORMAL_LES);
    sples = N_alloc_les(TEST_N_NUM_ROWS, N_SPARSE_LES);

#pragma omp parallel for private(i, j) shared(les)
    for (i = 0; i < TEST_N_NUM_ROWS; i++) {
	for (j = 0; j < TEST_N_NUM_ROWS; j++) {
	    if (i != j)
		les->A[i][j] = 2e-2;
	    les->A[i][i] = -1e2 - i;
	}
	les->x[i] = 273.15 + i;
	les->b[i] = 1e2 - i;
    }

#pragma omp parallel for private(i, j) shared(sples, spvector)
    for (i = 0; i < TEST_N_NUM_ROWS; i++) {
	spvector = N_alloc_spvector(TEST_N_NUM_ROWS);

	for (j = 0; j < TEST_N_NUM_ROWS; j++)
	    if (i != j)
		spvector->index[j] = 2e-2;

	spvector->index[0] = i;
	spvector->values[0] = -1e2 - i;

	N_add_spvector_to_les(sples, spvector, i);
	sples->x[i] = 273.15 + i;
	sples->b[i] = 1e2 - i;
    }

    N_free_les(les);
    N_free_les(sples);

    G_message(_("\t * testing les creation in serial\n"));

    les = N_alloc_les(TEST_N_NUM_ROWS, N_NORMAL_LES);
    sples = N_alloc_les(TEST_N_NUM_ROWS, N_SPARSE_LES);

    for (i = 0; i < TEST_N_NUM_ROWS; i++) {
	for (j = 0; j < TEST_N_NUM_ROWS; j++) {
	    if (i != j)
		les->A[i][j] = 2e-2;
	    les->A[i][i] = -1e2 - i;
	}
	les->x[i] = 273.15 + i;
	les->b[i] = 1e2 - i;
    }

    for (i = 0; i < TEST_N_NUM_ROWS; i++) {
	spvector = N_alloc_spvector(TEST_N_NUM_ROWS);

	for (j = 0; j < TEST_N_NUM_ROWS; j++)
	    if (i != j)
		spvector->index[j] = 2e-2;

	spvector->index[0] = i;
	spvector->values[0] = -1e2 - i;

	N_add_spvector_to_les(sples, spvector, i);
	sples->x[i] = 273.15 + i;
	sples->b[i] = 1e2 - i;
    }

    N_free_les(les);
    N_free_les(sples);

    return 0;
}
