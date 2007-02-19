
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:      Unit tests for matrix assembling
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
int test_matrix_assemble_2d();
int test_matrix_assemble_3d();
N_array_2d *create_status_array_2d();
N_array_3d *create_status_array_3d();
N_array_2d *create_value_array_2d();
N_array_3d *create_value_array_3d();

/* *************************************************************** */
/* Performe the les assmbling tests ****************************** */
/* *************************************************************** */
int unit_test_assemble()
{
    int sum = 0;

    G_message(_("++ Running assembling unit tests ++"));

    G_message(_("\t 1. testing 2d assembling"));
    sum += test_matrix_assemble_2d();

    G_message(_("\t 2. testing 3d assembling"));
    sum += test_matrix_assemble_3d();

    if (sum > 0)
	G_warning(_("-- Assembling unit tests failure --"));
    else
	G_message(_("-- Assembling unit tests finished successfully --"));

    return sum;
}

/* *************************************************************** */
/* Create the status array with values of 1 and 2 **************** */
/* *************************************************************** */
N_array_2d *create_status_array_2d()
{
    N_array_2d *data;
    int i, j;

    data = N_alloc_array_2d(TEST_N_NUM_COLS, TEST_N_NUM_ROWS, 1, CELL_TYPE);

#pragma omp parallel for private (i, j) shared (data)
    for (j = 0; j < TEST_N_NUM_ROWS; j++) {
	for (i = 0; i < TEST_N_NUM_COLS; i++) {

	    if (j == 0) {
		N_put_array_2d_c_value(data, i, j, 2);
	    }
	    else {
		N_put_array_2d_c_value(data, i, j, 1);
	    }
	}
    }
    return data;
}

/* *************************************************************** */
/* Create a value array ****************************************** */
/* *************************************************************** */
N_array_2d *create_value_array_2d()
{
    N_array_2d *data;
    int i, j;

    data = N_alloc_array_2d(TEST_N_NUM_COLS, TEST_N_NUM_ROWS, 1, DCELL_TYPE);

#pragma omp parallel for private (i, j) shared (data)
    for (j = 0; j < TEST_N_NUM_ROWS; j++) {
	for (i = 0; i < TEST_N_NUM_COLS; i++) {

	    if (j == 0) {
		N_put_array_2d_d_value(data, i, j, 50);
	    }
	    else {
		N_put_array_2d_d_value(data, i, j, 40);
	    }
	}
    }
    return data;
}

/* *************************************************************** */
/* Create the status array with values of 1 and 2 **************** */
/* *************************************************************** */
N_array_3d *create_status_array_3d()
{
    N_array_3d *data;
    int i, j, k;

    data =
	N_alloc_array_3d(TEST_N_NUM_COLS, TEST_N_NUM_ROWS, TEST_N_NUM_DEPTHS, 1,
			 FCELL_TYPE);

#pragma omp parallel for private (i, j, k) shared (data)
    for (k = 0; k < TEST_N_NUM_DEPTHS; k++)
	for (j = 0; j < TEST_N_NUM_ROWS; j++) {
	    for (i = 0; i < TEST_N_NUM_COLS; i++) {


		if (i == 0) {
		    N_put_array_3d_f_value(data, i, j, k, 2.0);
		}
		else {

		    N_put_array_3d_f_value(data, i, j, k, 1.0);
		}
	    }
	}

    return data;

}

/* *************************************************************** */
/* Create a value array ****************************************** */
/* *************************************************************** */
N_array_3d *create_value_array_3d()
{
    N_array_3d *data;
    int i, j, k;

    data =
	N_alloc_array_3d(TEST_N_NUM_COLS, TEST_N_NUM_ROWS, TEST_N_NUM_DEPTHS, 1,
			 DCELL_TYPE);

#pragma omp parallel for private (i, j, k) shared (data)
    for (k = 0; k < TEST_N_NUM_DEPTHS; k++)
	for (j = 0; j < TEST_N_NUM_ROWS; j++) {
	    for (i = 0; i < TEST_N_NUM_COLS; i++) {


		if (j == 0) {
		    N_put_array_3d_f_value(data, i, j, k, 50);
		}
		else {

		    N_put_array_3d_f_value(data, i, j, k, 50);
		}
	    }
	}

    return data;

}

/* *************************************************************** */
/* Test the matrix assembling with 3d array data ***************** */
/* *************************************************************** */
int test_matrix_assemble_3d()
{
    N_geom_data *geom;
    N_les *les;
    N_les_callback_3d *call;
    N_array_3d *status;
    N_array_3d *start_val;


    /*set the callback to default */
    call = N_alloc_les_callback_3d();

    status = create_status_array_3d();
    start_val = create_value_array_3d();

    geom = N_alloc_geom_data();

    geom->dx = 1;
    geom->dy = 1;
    geom->dz = 1;

    geom->Ax = 1;
    geom->Ay = 1;
    geom->Az = 1;

    geom->depths = TEST_N_NUM_DEPTHS;
    geom->rows = TEST_N_NUM_ROWS;
    geom->cols = TEST_N_NUM_COLS;

    /*Assemble the matrix */
    les = N_assemble_les_3d(N_SPARSE_LES, geom, status, start_val, NULL, call);
    N_free_les(les);

    les = N_assemble_les_3d(N_NORMAL_LES, geom, status, start_val, NULL, call);
    N_free_les(les);

    G_free(geom);
    G_free(call);

    return 0;
}


/* *************************************************************** */
/* Test the matrix assembling with 2d array data ***************** */
/* *************************************************************** */
int test_matrix_assemble_2d()
{
    N_geom_data *geom;
    N_les *les;
    N_les_callback_2d *call;
    N_array_2d *status;
    N_array_2d *start_val;

    /*set the callback to default */
    call = N_alloc_les_callback_2d();

    status = create_status_array_2d();
    start_val = create_value_array_2d();

    geom = N_alloc_geom_data();

    geom->dx = 1;
    geom->dy = 1;

    geom->Ax = 1;
    geom->Ay = 1;
    geom->Az = 1;

    geom->rows = TEST_N_NUM_ROWS;
    geom->cols = TEST_N_NUM_COLS;

    /*Assemble the matrix */
    les = N_assemble_les_2d(N_SPARSE_LES, geom, status, start_val, NULL, call);
    N_free_les(les);

    les = N_assemble_les_2d(N_NORMAL_LES, geom, status, start_val, NULL, call);
    N_free_les(les);

    G_free(geom);
    G_free(call);

    return 0;
}
