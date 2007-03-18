
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:      Unit tests for les solving
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <grass/glocale.h>
#include <grass/N_pde.h>
#include "test_gpde_lib.h"

/* prototypes */
int test_solvers();
N_les *create_normal_les(int rows);
N_les *create_sparse_les(int rows);

/* ************************************************************************* */
/* Performe the solver unit tests ****************************************** */
/* ************************************************************************* */
int unit_test_solvers()
{
    int sum = 0;

    G_message(_("\n++ Running solver unit tests ++"));

    sum += test_solvers();

    if (sum > 0)
	G_warning(_("\n-- Solver unit tests failure --"));
    else
	G_message(_("\n-- Solver unit tests finished successfully --"));

    return sum;
}

/* *************************************************************** */
/* create a normal matrix with values **************************** */
/* *************************************************************** */
N_les *create_normal_les(int rows)
{
    N_les *les;
    int i, j;

    les = N_alloc_les(rows, N_NORMAL_LES);

    for (i = 0; i < rows; i++) {
	for (j = 0; j < rows; j++) {
	    if (i > 1)
		les->A[i][j] = 2e-2;

	    les->A[i][i] = -1e2 - i;
	}
	les->x[i] = 273.15 + i;
	les->b[i] = 0;
    }

    return les;
}

/* *************************************************************** */
/* create a sparse matrix with values **************************** */
/* *************************************************************** */
N_les *create_sparse_les(int rows)
{
    N_les *les;
    N_spvector *spvector;
    int i, j;

    les = N_alloc_les(rows, N_SPARSE_LES);

    for (i = 0; i < rows; i++) {
	spvector = N_alloc_spvector(rows);

	for (j = 0; j < rows; j++)
	    if (j > 1)
		spvector->index[j] = 2e-2;

	spvector->index[0] = i;
	spvector->values[0] = -1e2 - i;

	N_add_spvector_to_les(les, spvector, i);
	les->x[i] = 273.15 + i;
	les->b[i] = 0;
    }


    return les;
}


/* *************************************************************** */
/* Test all implemented solvers for sparse and normal matrices *** */
/* *************************************************************** */
int test_solvers()
{
    N_les *les;
    N_les *sples;

    G_message("\t * testing jacobi solver\n");

    les = create_normal_les(TEST_N_NUM_ROWS);
    sples = create_sparse_les(TEST_N_NUM_ROWS);

    N_solver_jacobi(les, 100, 1, 0.1e-4);
    /*N_print_les(les); */
    N_solver_jacobi(sples, 100, 1, 0.1e-4);
    /*N_print_les(sples); */

    N_free_les(les);
    N_free_les(sples);


    G_message("\t * testing SOR solver\n");

    les = create_normal_les(TEST_N_NUM_ROWS);
    sples = create_sparse_les(TEST_N_NUM_ROWS);

    N_solver_SOR(les, 100, 1, 0.1e-4);
    /*N_print_les(les); */
    N_solver_SOR(sples, 100, 1, 0.1e-4);
    /*N_print_les(sples); */

    N_free_les(les);
    N_free_les(sples);


    G_message("\t * testing cg solver\n");

    les = create_normal_les(TEST_N_NUM_ROWS);
    sples = create_sparse_les(TEST_N_NUM_ROWS);

    N_solver_cg(les, 100, 0.1e-8);
    /*N_print_les(les); */
    N_solver_cg(sples, 100, 0.1e-8);
    /*N_print_les(sples); */

    N_free_les(les);
    N_free_les(sples);

    G_message("\t * testing bicgstab solver\n");

    les = create_normal_les(TEST_N_NUM_ROWS);
    sples = create_sparse_les(TEST_N_NUM_ROWS);


    N_solver_bicgstab(les, 100, 0.1e-8);
    /*N_print_les(les); */
    N_solver_bicgstab(sples, 100, 0.1e-8);
    /*N_print_les(sples); */

    N_free_les(les);
    N_free_les(sples);

    G_message("\t * testing gauss elimination solver\n");

    les = create_normal_les(TEST_N_NUM_ROWS);

     /*GAUSS*/ N_solver_gauss(les);
    /*N_print_les(les); */

    N_free_les(les);

    G_message("\t * testing lu decomposition solver\n");

    les = create_normal_les(TEST_N_NUM_ROWS);

     /*LU*/ N_solver_lu(les);
    /*N_print_les(les); */

    N_free_les(les);


    return 0;
}
