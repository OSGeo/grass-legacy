
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:	Unit tests for arrays
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
#include <assert.h>
#include <grass/glocale.h>
#include <grass/N_pde.h>
#include "test_gpde_lib.h"

/* prototypes */
int fill_array_2d (N_array_2d * a);
int compare_array_2d (N_array_2d * a, N_array_2d * b);
int fill_array_3d (N_array_3d * a);
int compare_array_3d (N_array_3d * a, N_array_3d * b);
int test_array_2d ();
int test_array_3d ();

/* ************************************************************************* */
/* Performe the array unit tests ******************************************* */
/* ************************************************************************* */
int
unit_test_arrays ()
{
  int sum = 0;

  G_message (_("++ Running array unit tests ++"));

  G_message (_("\t 1. testing 2d arrays"));
  sum += test_array_2d ();

  G_message (_("\t 2. testing 3d arrays"));
  sum += test_array_3d ();

  if (sum > 0)
    G_warning (_("-- Array unit tests failure --"));
  else
    G_message (_("-- Array unit tests finished successfully --"));

  return sum;
}

/* ************************************************************************* */
/* Fill an 2d array with valid data **************************************** */
/* ************************************************************************* */
int
fill_array_2d (N_array_2d * a)
{
  int rows, cols, type;
  int i, j;

  rows = a->rows;
  cols = a->cols;
  type = N_get_array_2d_type (a);

#pragma omp parallel for private (i, j) shared (rows, cols, type, a)
  for (i = 0; i < rows; i++)
    {
      for (j = 0; j < cols; j++)
	{
	  if (type == CELL_TYPE)
	    {
	      N_put_array_2d_value_cell (a, i, j, 1 * (CELL) i * (CELL) j);
	      if (N_get_array_2d_value_cell (a, i, j) != 1 * (CELL) i * (CELL) j)
		assert (EXIT_FAILURE);
	    }
	  if (type == FCELL_TYPE)
	    {
	      N_put_array_2d_value_fcell (a, i, j, 1.0 * (FCELL) i * (FCELL) j);
	      if (N_get_array_2d_value_fcell (a, i, j) != 1 * (FCELL) i * (FCELL) j)
		assert (EXIT_FAILURE);
	    }
	  if (type == DCELL_TYPE)
	    {
	      N_put_array_2d_value_dcell (a, i, j, 1.0 * (DCELL) i * (DCELL) j);
	      if (N_get_array_2d_value_dcell (a, i, j) != 1 * (DCELL) i * (DCELL) j)
		assert (EXIT_FAILURE);
	    }
	}
    }

  return 0;
}

/* ************************************************************************* */
/* Compare two 2d arrays *************************************************** */
/* ************************************************************************* */
int
compare_array_2d (N_array_2d * a, N_array_2d * b)
{
  int rows, cols, type;
  int i, j;

  rows = a->rows;
  cols = a->cols;
  type = N_get_array_2d_type (a);

#pragma omp parallel for private (i, j) shared (rows, cols, type, a, b)
  for (i = 0; i < rows; i++)
    {
      for (j = 0; j < cols; j++)
	{
	  if (type == CELL_TYPE)
	    {
	      if (N_get_array_2d_value_cell (a, i, j) != N_get_array_2d_value_cell (b, i, j))
		assert (EXIT_FAILURE);
	    }
	  if (type == FCELL_TYPE)
	    {
	      if (N_get_array_2d_value_fcell (a, i, j) != N_get_array_2d_value_fcell (b, i, j))
		assert (EXIT_FAILURE);
	    }
	  if (type == DCELL_TYPE)
	    {
	      if (N_get_array_2d_value_dcell (a, i, j) != N_get_array_2d_value_dcell (b, i, j))
		assert (EXIT_FAILURE);
	    }
	}
    }

  return 0;
}

/* ************************************************************************* */
/* Fill an 3d array with valid data **************************************** */
/* ************************************************************************* */
int
fill_array_3d (N_array_3d * a)
{
  int rows, cols, depths, type;
  int i, j, k;

  rows = a->rows;
  cols = a->cols;
  depths = a->depths;
  type = N_get_array_3d_type (a);

#pragma omp parallel for private (k, i, j) shared (depths, rows, cols, type, a)
  for (k = 0; k < depths; k++)
    {
      for (i = 0; i < rows; i++)
	{
	  for (j = 0; j < cols; j++)
	    {
	      if (type == G3D_FLOAT)
		{
		  N_put_array_3d_value_float (a, k, i, j, 1.0 * (float) i * (float) j * (float) k);
		  if (N_get_array_3d_value_float (a, k, i, j) != 1.0 * (float) i * (float) j * (float) k)
		    assert (EXIT_FAILURE);
		}
	      if (type == G3D_DOUBLE)
		{
		  N_put_array_3d_value_double (a, k, i, j, 1.0 * (double) i * (double) j * (double) k);
		  if (N_get_array_3d_value_double (a, k, i, j) != 1.0 * (double) i * (double) j * (double) k)
		    assert (EXIT_FAILURE);
		}
	    }
	}
    }

  return 0;
}

/* ************************************************************************* */
/* Compare two 3d arrays *************************************************** */
/* ************************************************************************* */
int
compare_array_3d (N_array_3d * a, N_array_3d * b)
{
  int rows, cols, depths, type;
  int i, j, k;

  rows = a->rows;
  cols = a->cols;
  depths = a->depths;
  type = N_get_array_3d_type (a);

#pragma omp parallel for private (k, i, j) shared (depths, rows, cols, type, a, b)
  for (k = 0; k < depths; k++)
    {
      for (i = 0; i < rows; i++)
	{
	  for (j = 0; j < cols; j++)
	    {
	      if (type == G3D_FLOAT)
		{
		  if (N_get_array_3d_value_float (a, k, i, j) != N_get_array_3d_value_float (b, k, i, j))
		    assert (EXIT_FAILURE);
		}
	      if (type == G3D_DOUBLE)
		{
		  if (N_get_array_3d_value_double (a, k, i, j) != N_get_array_3d_value_double (b, k, i, j))
		    assert (EXIT_FAILURE);
		}
	    }
	}
    }

  return 0;
}


/* *************************************************************** */
/* *************************************************************** */
/* *************************************************************** */
int
test_array_2d ()
{
  int sum = 0;

  N_array_2d *data1;
  N_array_2d *data11;
  N_array_2d *data2;
  N_array_2d *data22;
  N_array_2d *data3;
  N_array_2d *data33;

  /*Alloacte memory for all arrays */
  data1 = N_alloc_array_2d (TEST_N_NUM_ROWS, TEST_N_NUM_COLS, 1, CELL_TYPE);
  data11 = N_alloc_array_2d (TEST_N_NUM_ROWS, TEST_N_NUM_COLS, 1, CELL_TYPE);
  data2 = N_alloc_array_2d (TEST_N_NUM_ROWS, TEST_N_NUM_COLS, 2, FCELL_TYPE);
  data22 = N_alloc_array_2d (TEST_N_NUM_ROWS, TEST_N_NUM_COLS, 2, FCELL_TYPE);
  data3 = N_alloc_array_2d (TEST_N_NUM_ROWS, TEST_N_NUM_COLS, 3, DCELL_TYPE);
  data33 = N_alloc_array_2d (TEST_N_NUM_ROWS, TEST_N_NUM_COLS, 3, DCELL_TYPE);

  /*Fill the first arrays with data */
  sum += fill_array_2d (data1);
  sum += fill_array_2d (data2);
  sum += fill_array_2d (data3);

  /*Copy the data */
  N_array_2d_copy (data1, data11);
  N_array_2d_copy (data2, data22);
  N_array_2d_copy (data3, data33);

  /*Compare the data */
  sum += compare_array_2d (data1, data11);
  sum += compare_array_2d (data1, data11);
  sum += compare_array_2d (data1, data11);

  N_free_array_2d (data1);
  N_free_array_2d (data11);
  N_free_array_2d (data2);
  N_free_array_2d (data22);
  N_free_array_2d (data3);
  N_free_array_2d (data33);

  return sum;
}

/* *************************************************************** */
/* *************************************************************** */
/* *************************************************************** */
int
test_array_3d ()
{
  int sum = 0;

  N_array_3d *data1;
  N_array_3d *data11;
  N_array_3d *data2;
  N_array_3d *data22;

  /*Alloacte memory for all arrays */
  data1 = N_alloc_array_3d (TEST_N_NUM_DEPTHS, TEST_N_NUM_ROWS, TEST_N_NUM_COLS, 1, G3D_FLOAT);
  data11 = N_alloc_array_3d (TEST_N_NUM_DEPTHS, TEST_N_NUM_ROWS, TEST_N_NUM_COLS, 1, G3D_FLOAT);
  data2 = N_alloc_array_3d (TEST_N_NUM_DEPTHS, TEST_N_NUM_ROWS, TEST_N_NUM_COLS, 2, G3D_DOUBLE);
  data22 = N_alloc_array_3d (TEST_N_NUM_DEPTHS, TEST_N_NUM_ROWS, TEST_N_NUM_COLS, 2, G3D_DOUBLE);

  /*Fill the first arrays with data */
  sum += fill_array_3d (data1);
  sum += fill_array_3d (data2);

  /*Copy the data */
  N_array_3d_copy (data1, data11);
  N_array_3d_copy (data2, data22);

  /*Compare the data */
  sum += compare_array_3d (data1, data11);
  sum += compare_array_3d (data2, data22);

  N_free_array_3d (data1);
  N_free_array_3d (data11);
  N_free_array_3d (data2);
  N_free_array_3d (data22);

  return sum;
}
