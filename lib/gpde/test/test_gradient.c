
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:      Unit tests for gradient calculation
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
int test_gradient_2d();
int test_gradient_3d();
N_array_2d *create_relax_array_2d();
N_array_3d *create_relax_array_3d();
N_array_2d *create_potential_array_2d();
N_array_3d *create_potential_array_3d();

/* *************************************************************** */
/* Performe the les assmbling tests ****************************** */
/* *************************************************************** */
int unit_test_gradient()
{
    int sum = 0;

    G_message(_("++ Running gradient unit tests ++"));

    G_message(_("\t 1. testing 2d gradient"));
    sum += test_gradient_2d();

    G_message(_("\t 2. testing 3d gradient"));
    sum += test_gradient_3d();

    if (sum > 0)
	G_warning(_("-- Gradient unit tests failure --"));
    else
	G_message(_("-- Gradient unit tests finished successfully --"));

    return sum;
}

/* *************************************************************** */
/* Create the status array with values of 1 and 2 **************** */
/* *************************************************************** */
N_array_2d *create_relax_array_2d()
{
    N_array_2d *data;
    int i, j;

    data = N_alloc_array_2d(TEST_N_NUM_COLS, TEST_N_NUM_ROWS, 1, CELL_TYPE);

#pragma omp parallel for private (i, j) shared (data)
    for (j = 0; j < TEST_N_NUM_ROWS; j++) {
	for (i = 0; i < TEST_N_NUM_COLS; i++) {

	    if (j == 0) {
		N_put_array_2d_value_cell(data, i, j, 1);
	    }
	    else {
		N_put_array_2d_value_cell(data, i, j, 1);
	    }
	}
    }
    return data;
}

/* *************************************************************** */
/* Create a value array ****************************************** */
/* *************************************************************** */
N_array_2d *create_potential_array_2d()
{
    N_array_2d *data;
    int i, j;

    data = N_alloc_array_2d(TEST_N_NUM_COLS, TEST_N_NUM_ROWS, 1, DCELL_TYPE);

#pragma omp parallel for private (i, j) shared (data)
    for (j = 0; j < TEST_N_NUM_ROWS; j++) {
	for (i = 0; i < TEST_N_NUM_COLS; i++) {

	    if (j == 0) {
		N_put_array_2d_value_dcell(data, i, j, 40);
	    }
	    else {
		N_put_array_2d_value_dcell(data, i, j, 40);
	    }
	}
    }
    return data;
}

/* *************************************************************** */
/* Create the status array with values of 1 and 2 **************** */
/* *************************************************************** */
N_array_3d *create_relax_array_3d()
{
    N_array_3d *data;
    int i, j, k;

    data =
	N_alloc_array_3d(TEST_N_NUM_COLS, TEST_N_NUM_ROWS, TEST_N_NUM_DEPTHS, 1,
			 G3D_FLOAT);

#pragma omp parallel for private (i, j, k) shared (data)
    for (k = 0; k < TEST_N_NUM_DEPTHS; k++)
	for (j = 0; j < TEST_N_NUM_ROWS; j++) {
	    for (i = 0; i < TEST_N_NUM_COLS; i++) {


		if (i == 0) {
		    N_put_array_3d_value_float(data, i, j, k, 1.0);
		}
		else {

		    N_put_array_3d_value_float(data, i, j, k, 1.0);
		}
	    }
	}

    return data;

}

/* *************************************************************** */
/* Create a value array ****************************************** */
/* *************************************************************** */
N_array_3d *create_potential_array_3d()
{
    N_array_3d *data;
    int i, j, k;

    data =
	N_alloc_array_3d(TEST_N_NUM_COLS, TEST_N_NUM_ROWS, TEST_N_NUM_DEPTHS, 1,
			 G3D_DOUBLE);

#pragma omp parallel for private (i, j, k) shared (data)
    for (k = 0; k < TEST_N_NUM_DEPTHS; k++)
	for (j = 0; j < TEST_N_NUM_ROWS; j++) {
	    for (i = 0; i < TEST_N_NUM_COLS; i++) {


		if (j == 0) {
		    N_put_array_3d_value_float(data, i, j, k, 50);
		}
		else {

		    N_put_array_3d_value_float(data, i, j, k, 50);
		}
	    }
	}

    return data;

}

/* *************************************************************** */
/* Test the gradient calculation with 3d array data ************** */
/* *************************************************************** */
int test_gradient_3d()
{
    N_array_3d *relax;
    N_array_3d *pot;
    N_array_3d *xcomp;
    N_array_3d *ycomp;
    N_array_3d *zcomp;
    N_gradient_field_3d *field;
    N_gradient_3d *grad;
    N_geom_data *geom;

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


    relax = create_relax_array_3d();
    pot = create_potential_array_3d();

    field = N_compute_gradient_field_3d(pot, relax, relax, relax, geom);
    N_free_gradient_field_3d(field);

    N_free_array_3d(relax);
    N_free_array_3d(pot);

    relax = N_alloc_array_3d(3, 3, 3, 0, G3D_DOUBLE);
    pot = N_alloc_array_3d(3, 3, 3, 0, G3D_DOUBLE);
    xcomp = N_alloc_array_3d(3, 3, 3, 0, G3D_DOUBLE);
    ycomp = N_alloc_array_3d(3, 3, 3, 0, G3D_DOUBLE);
    zcomp = N_alloc_array_3d(3, 3, 3, 0, G3D_DOUBLE);

    N_put_array_3d_value_double(relax, 0, 0, 0, 1);
    N_put_array_3d_value_double(relax, 0, 1, 0, 1);
    N_put_array_3d_value_double(relax, 0, 2, 0, 1);
    N_put_array_3d_value_double(relax, 1, 0, 0, 1);
    N_put_array_3d_value_double(relax, 1, 1, 0, 1);
    N_put_array_3d_value_double(relax, 1, 2, 0, 1);
    N_put_array_3d_value_double(relax, 2, 0, 0, 1);
    N_put_array_3d_value_double(relax, 2, 1, 0, 1);
    N_put_array_3d_value_double(relax, 2, 2, 0, 1);

    N_put_array_3d_value_double(relax, 0, 0, 1, 1);
    N_put_array_3d_value_double(relax, 0, 1, 1, 1);
    N_put_array_3d_value_double(relax, 0, 2, 1, 1);
    N_put_array_3d_value_double(relax, 1, 0, 1, 1);
    N_put_array_3d_value_double(relax, 1, 1, 1, 1);
    N_put_array_3d_value_double(relax, 1, 2, 1, 1);
    N_put_array_3d_value_double(relax, 2, 0, 1, 1);
    N_put_array_3d_value_double(relax, 2, 1, 1, 1);
    N_put_array_3d_value_double(relax, 2, 2, 1, 1);


    N_put_array_3d_value_double(relax, 0, 0, 2, 1);
    N_put_array_3d_value_double(relax, 0, 1, 2, 1);
    N_put_array_3d_value_double(relax, 0, 2, 2, 1);
    N_put_array_3d_value_double(relax, 1, 0, 2, 1);
    N_put_array_3d_value_double(relax, 1, 1, 2, 1);
    N_put_array_3d_value_double(relax, 1, 2, 2, 1);
    N_put_array_3d_value_double(relax, 2, 0, 2, 1);
    N_put_array_3d_value_double(relax, 2, 1, 2, 1);
    N_put_array_3d_value_double(relax, 2, 2, 2, 1);

    N_print_array_3d(relax);


  /**
   * 1 2 6        5
   * 3 7 10 ==  4  -3
   * 8 15 25     -8
   * */

    N_put_array_3d_value_double(pot, 0, 0, 0, 1.0);
    N_put_array_3d_value_double(pot, 1, 0, 0, 2.0);
    N_put_array_3d_value_double(pot, 2, 0, 0, 6.0);
    N_put_array_3d_value_double(pot, 0, 1, 0, 3.0);
    N_put_array_3d_value_double(pot, 1, 1, 0, 7.0);
    N_put_array_3d_value_double(pot, 2, 1, 0, 10.0);
    N_put_array_3d_value_double(pot, 0, 2, 0, 8.0);
    N_put_array_3d_value_double(pot, 1, 2, 0, 15.0);
    N_put_array_3d_value_double(pot, 2, 2, 0, 25.0);

    N_put_array_3d_value_double(pot, 0, 0, 1, 1.2);
    N_put_array_3d_value_double(pot, 1, 0, 1, 2.2);
    N_put_array_3d_value_double(pot, 2, 0, 1, 6.2);
    N_put_array_3d_value_double(pot, 0, 1, 1, 3.2);
    N_put_array_3d_value_double(pot, 1, 1, 1, 7.2);
    N_put_array_3d_value_double(pot, 2, 1, 1, 10.2);
    N_put_array_3d_value_double(pot, 0, 2, 1, 8.2);
    N_put_array_3d_value_double(pot, 1, 2, 1, 15.2);
    N_put_array_3d_value_double(pot, 2, 2, 1, 25.2);


    N_put_array_3d_value_double(pot, 0, 0, 2, 1.5);
    N_put_array_3d_value_double(pot, 1, 0, 2, 2.5);
    N_put_array_3d_value_double(pot, 2, 0, 2, 6.5);
    N_put_array_3d_value_double(pot, 0, 1, 2, 3.5);
    N_put_array_3d_value_double(pot, 1, 1, 2, 7.5);
    N_put_array_3d_value_double(pot, 2, 1, 2, 10.5);
    N_put_array_3d_value_double(pot, 0, 2, 2, 8.5);
    N_put_array_3d_value_double(pot, 1, 2, 2, 15.5);
    N_put_array_3d_value_double(pot, 2, 2, 2, 25.5);

    N_print_array_3d(pot);

    field = N_compute_gradient_field_3d(pot, relax, relax, relax, geom);

    grad = N_get_gradient_3d(field, NULL, 0, 0, 0);
    printf
	("Gradient 3d: NC %g == 0 ; SC %g == -2 ; WC %g == 0 ; EC %g == -1 BC %g == 0 TC %g == -0.2\n",
	 grad->NC, grad->SC, grad->WC, grad->EC, grad->BC, grad->TC);

    grad = N_get_gradient_3d(field, grad, 1, 0, 0);
    printf
	("Gradient 3d: NC %g == 0 ; SC %g == -5 ; WC %g == 1 ; EC %g == -4 BC %g == 0 TC %g == -0.2\n",
	 grad->NC, grad->SC, grad->WC, grad->EC, grad->BC, grad->TC);
    N_free_gradient_3d(grad);

    grad = N_get_gradient_3d(field, NULL, 1, 1, 1);
    printf
	("Gradient 3d: NC %g == 5 ; SC %g == -8 ; WC %g == 4 ; EC %g == -3 BC %g == 0.2 TC %g == -0.3\n",
	 grad->NC, grad->SC, grad->WC, grad->EC, grad->BC, grad->TC);

    grad = N_get_gradient_3d(field, grad, 1, 2, 2);
    printf
	("Gradient 3d: NC %g == 8 ; SC %g ==  0 ; WC %g == 7 ; EC %g == 10 BC %g == 0.3 TC %g == 0\n",
	 grad->NC, grad->SC, grad->WC, grad->EC, grad->BC, grad->TC);
    N_free_gradient_3d(grad);

    grad = N_get_gradient_3d(field, NULL, 2, 2, 2);
    printf
	("Gradient 3d: NC %g ==15 ; SC %g ==  0 ; WC %g ==10 ; EC %g ==  0 BC %g == 0.3 TC %g == 0\n",
	 grad->NC, grad->SC, grad->WC, grad->EC, grad->BC, grad->TC);
    N_free_gradient_3d(grad);

    N_compute_gradient_field_components_3d(field, xcomp, ycomp, zcomp);

    N_print_array_3d(xcomp);
    N_print_array_3d(ycomp);
    N_print_array_3d(zcomp);

    G_free(geom);

    return 0;
}


/* *************************************************************** */
/* Test the gradient calculation with 2d array data ************** */
/* *************************************************************** */
int test_gradient_2d()
{
    N_array_2d *relax;
    N_array_2d *pot;
    N_array_2d *xcomp;
    N_array_2d *ycomp;
    N_gradient_field_2d *field;
    N_geom_data *geom;
    N_gradient_2d *grad;

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


    relax = create_relax_array_2d();
    pot = create_potential_array_2d();


    field = N_compute_gradient_field_2d(pot, relax, relax, geom);
    N_free_gradient_field_2d(field);

    N_free_array_2d(relax);
    N_free_array_2d(pot);

    relax = N_alloc_array_2d(3, 3, 0, DCELL_TYPE);
    pot = N_alloc_array_2d(3, 3, 0, DCELL_TYPE);
    xcomp = N_alloc_array_2d(3, 3, 0, DCELL_TYPE);
    ycomp = N_alloc_array_2d(3, 3, 0, DCELL_TYPE);

    N_put_array_2d_value_dcell(relax, 0, 0, 1.0);
    N_put_array_2d_value_dcell(relax, 0, 1, 1.0);
    N_put_array_2d_value_dcell(relax, 0, 2, 1.0);
    N_put_array_2d_value_dcell(relax, 1, 0, 1.0);
    N_put_array_2d_value_dcell(relax, 1, 1, 1.0);
    N_put_array_2d_value_dcell(relax, 1, 2, 1.0);
    N_put_array_2d_value_dcell(relax, 2, 0, 1.0);
    N_put_array_2d_value_dcell(relax, 2, 1, 1.0);
    N_put_array_2d_value_dcell(relax, 2, 2, 1.0);

    N_print_array_2d(relax);

  /**
   * 1 2 6        5
   * 3 7 10 ==  4  -3
   * 8 15 25     -8
   * */

    N_put_array_2d_value_dcell(pot, 0, 0, 1.0);
    N_put_array_2d_value_dcell(pot, 1, 0, 2.0);
    N_put_array_2d_value_dcell(pot, 2, 0, 6.0);
    N_put_array_2d_value_dcell(pot, 0, 1, 3.0);
    N_put_array_2d_value_dcell(pot, 1, 1, 7.0);
    N_put_array_2d_value_dcell(pot, 2, 1, 10.0);
    N_put_array_2d_value_dcell(pot, 0, 2, 8.0);
    N_put_array_2d_value_dcell(pot, 1, 2, 15.0);
    N_put_array_2d_value_dcell(pot, 2, 2, 25.0);

    N_print_array_2d(pot);

    field = N_compute_gradient_field_2d(pot, relax, relax, geom);

    grad = N_get_gradient_2d(field, NULL, 0, 0);
    printf("Gradient 2d: NC %g == 0 ; SC %g == -2 ; WC %g == 0 ; EC %g == -1\n",
	   grad->NC, grad->SC, grad->WC, grad->EC);

    grad = N_get_gradient_2d(field, grad, 1, 0);
    printf("Gradient 2d: NC %g == 0 ; SC %g == -5 ; WC %g == 1 ; EC %g == -4\n",
	   grad->NC, grad->SC, grad->WC, grad->EC);
    N_free_gradient_2d(grad);

    grad = N_get_gradient_2d(field, NULL, 1, 1);
    printf("Gradient 2d: NC %g == 5 ; SC %g == -8 ; WC %g == 4 ; EC %g == -3\n",
	   grad->NC, grad->SC, grad->WC, grad->EC);

    grad = N_get_gradient_2d(field, grad, 1, 2);
    printf("Gradient 2d: NC %g == 8 ; SC %g ==  0 ; WC %g == 7 ; EC %g == 10\n",
	   grad->NC, grad->SC, grad->WC, grad->EC);
    N_free_gradient_2d(grad);

    grad = N_get_gradient_2d(field, NULL, 2, 2);
    printf("Gradient 2d: NC %g ==15 ; SC %g ==  0 ; WC %g ==10 ; EC %g ==  0\n",
	   grad->NC, grad->SC, grad->WC, grad->EC);
    N_free_gradient_2d(grad);

    N_compute_gradient_field_components_2d(field, xcomp, ycomp);

    N_print_array_2d(xcomp);
    N_print_array_2d(ycomp);

    N_free_gradient_field_2d(field);
    G_free(geom);

    N_free_array_2d(relax);
    N_free_array_2d(pot);


    return 0;
}
