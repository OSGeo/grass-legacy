
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:     	Array managment functions 
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
#include <math.h>

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief Allocate memory for a N_array_2d array structure. 
 *
 * This functions allocates an array of type N_array_2d and returns a pointer 
 * to the new allocated memory.
 *
 * The data type of this array is set by "type" and must be 
 * CELL_TYPE, FCELL_TYPE or DCELL_TYPE accordingly to the raster map datatypes.
 * The offset sets the number of boundary cols and rows. 
 * This option is useful to generate homogeneous Neumann boundary conditions around  
 * an array. The arrays are initialized with 0 by default.
 *
 * If the offset is greater then 0, negative indices are possible.
 *
 * The data structure of a array with 3 rows and cols and an offset of 1 
 * will looks like this:
 *
  \verbatim
  0 0 0 0 0
  0 0 1 2 0
  0 3 4 5 0
  0 6 7 8 0
  0 0 0 0 0
  \endverbatim
 *
 * 0 is the boundary.
 * <br><br>
 * Internal a one dimensional array is allocated to speed up the memory access.
 * To access the one dimensional array with a two dimensional indexing use the provided 
 * get and put functions. The internal representation of the above data looks like this:
 *
  \verbatim
  0 0 0 0 0 0 0 1 2 0 0 3 4 5 0 0 6 7 8 0 0 0 0 0 0 
  \endverbatim
 *
 * \param rows int 
 * \param cols int 
 * \param offset int
 * \param type int
 * \return N_array_2d *
 * 
 * */
N_array_2d *
N_alloc_array_2d (int rows, int cols, int offset, int type)
{
  N_array_2d *data = NULL;

  if (rows < 1 || cols < 1)
    G_fatal_error ("N_alloc_array_2d: cols and rows should be > 0");

  if (type != CELL_TYPE && type != FCELL_TYPE && type != DCELL_TYPE)
    G_fatal_error ("N_alloc_array_2d: Wrong data type, should be CELL_TYPE, FCELL_TYPE or DCELL_TYPE");

  data = (N_array_2d *) G_calloc (1, sizeof (N_array_2d));

  data->rows = rows;
  data->cols = cols;
  data->type = type;
  data->offset = offset;
  data->rows_intern = rows + 2 * offset;
  data->cols_intern = cols + 2 * offset;
  data->cell_array = NULL;
  data->fcell_array = NULL;
  data->dcell_array = NULL;

  /*Allocation is in order array[rows*cols] */

  G_debug (3,
	   "N_alloc_array_2d: rows_intern %i cols_intern %i offset %i\n",
	   data->rows_intern, data->cols_intern, data->offset = offset);

  if (data->type == CELL_TYPE)
    {
      data->cell_array = (CELL *) G_calloc (data->rows_intern * data->cols_intern, sizeof (CELL));
    }
  else if (data->type == FCELL_TYPE)
    {
      data->fcell_array = (FCELL *) G_calloc (data->rows_intern * data->cols_intern, sizeof (FCELL));

    }
  else if (data->type == DCELL_TYPE)
    {
      data->dcell_array = (DCELL *) G_calloc (data->rows_intern * data->cols_intern, sizeof (DCELL));
    }

  return data;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief Release the memory of a N_array_2d structure
 *
 * \param data N_array_2d * 
 * \return void
 * */
void
N_free_array_2d (N_array_2d * data)
{

  if (data != NULL)
    {
      G_debug (3, "N_free_array_2d: free N_array_2d");

      if (data->type == CELL_TYPE && data->cell_array != NULL)
	{
	  G_free (data->cell_array);
	}
      else if (data->type == FCELL_TYPE && data->fcell_array != NULL)
	{
	  G_free (data->fcell_array);

	}
      else if (data->type == DCELL_TYPE && data->dcell_array != NULL)
	{
	  G_free (data->dcell_array);
	}

      G_free (data);
      data = NULL;

    }

  return;
}

/*!
 * \brief Return the data taype of the N_array_2d
 *
 * The data type can be CELL_TYPE, FCELL_TYPE or DCELL_TYPE accordingly to the raster map datatypes.
 *
 * \param array2d N_array_2d * 
 * \return type int
 * */
/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 *
 * */
int
N_get_array_2d_type (N_array_2d * array2d)
{
  return array2d->type;
}

/*!
 * \brief This function writes the value of N_array_2d data at position row, col to the variable value
 *
 * \param data N_array_2d * 
 * \param row int
 * \param col int
 * \param value void * 
 * \return void
 * */
/***********************************************************
 * *********************************************************
 * ********************************************************/
inline void
N_get_array_2d_value (N_array_2d * data, int row, int col, void *value)
{

  if (data->offset == 0)
    {
      if (data->type == CELL_TYPE && data->cell_array != NULL)
	{
	  *((CELL *) value) = data->cell_array[row * data->rows_intern + col];
	}
      else if (data->type == FCELL_TYPE && data->fcell_array != NULL)
	{
	  *((FCELL *) value) = data->fcell_array[row * data->rows_intern + col];
	}
      else if (data->type == DCELL_TYPE && data->dcell_array != NULL)
	{
	  *((DCELL *) value) = data->dcell_array[row * data->rows_intern + col];
	}
    }
  else
    {
      if (data->type == CELL_TYPE && data->cell_array != NULL)
	{
	  *((CELL *) value) = data->cell_array[(row + data->offset) * data->rows_intern + col + data->offset];
	}
      else if (data->type == FCELL_TYPE && data->fcell_array != NULL)
	{
	  *((FCELL *) value) = data->fcell_array[(row + data->offset) * data->rows_intern + col + data->offset];
	}
      else if (data->type == DCELL_TYPE && data->dcell_array != NULL)
	{
	  *((DCELL *) value) = data->dcell_array[(row + data->offset) * data->rows_intern + col + data->offset];
	}
    }

  return;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function returns the value of type CELL at position row, col 
 *
 * The data array can be of type CELL, FCELL or DCELL.
 *
 * \param data N_array_2d * 
 * \param row int
 * \param col int
 * \return CELL
 *        
 * */
inline CELL
N_get_array_2d_value_cell (N_array_2d * data, int row, int col)
{
  CELL value = 0;
  FCELL fvalue = 0.0;
  DCELL dvalue = 0.0;

  switch (data->type)
    {
    case CELL_TYPE:
      N_get_array_2d_value (data, row, col, (void *) &value);
      return (CELL) value;
    case FCELL_TYPE:
      N_get_array_2d_value (data, row, col, (void *) &fvalue);
      return (CELL) fvalue;
    case DCELL_TYPE:
      N_get_array_2d_value (data, row, col, (void *) &dvalue);
      return (CELL) dvalue;
    }

  return value;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function returns the value of type FCELL at position row, col 
 *        
 * The data array can be of type CELL, FCELL or DCELL.
 *
 * \param data N_array_2d * 
 * \param row int
 * \param col int
 * \return FCELL

 * */
inline FCELL
N_get_array_2d_value_fcell (N_array_2d * data, int row, int col)
{
  CELL value = 0;
  FCELL fvalue = 0.0;
  DCELL dvalue = 0.0;

  switch (data->type)
    {
    case CELL_TYPE:
      N_get_array_2d_value (data, row, col, (void *) &value);
      return (FCELL) value;
    case FCELL_TYPE:
      N_get_array_2d_value (data, row, col, (void *) &fvalue);
      return (FCELL) fvalue;
    case DCELL_TYPE:
      N_get_array_2d_value (data, row, col, (void *) &dvalue);
      return (FCELL) dvalue;
    }

  return fvalue;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function returns the value of type DCELL at position row, col 
 *
 * The data array can be of type CELL, FCELL or DCELL.
 * 
 * \param data N_array_2d *
 * \param row int
 * \param col int
 * \return DCELL
 *        
 * */
inline DCELL
N_get_array_2d_value_dcell (N_array_2d * data, int row, int col)
{
  CELL value = 0;
  FCELL fvalue = 0.0;
  DCELL dvalue = 0.0;

  switch (data->type)
    {
    case CELL_TYPE:
      N_get_array_2d_value (data, row, col, (void *) &value);
      return (DCELL) value;
    case FCELL_TYPE:
      N_get_array_2d_value (data, row, col, (void *) &fvalue);
      return (DCELL) fvalue;
    case DCELL_TYPE:
      N_get_array_2d_value (data, row, col, (void *) &dvalue);
      return (DCELL) dvalue;
    }

  return dvalue;

}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes a value to the N_array_2d data at position row, col
 *
 * The value will be automatically cast to the array type.
 *
 * \param data N_array_2d *
 * \param row int
 * \param col int
 * \param value char *
 * \return void
 * */
inline void
N_put_array_2d_value (N_array_2d * data, int row, int col, char *value)
{

  G_debug (6, "N_put_array_2d_value: adding value to array");

  if (data->offset == 0)
    {
      if (data->type == CELL_TYPE && data->cell_array != NULL)
	{
	  data->cell_array[row * data->rows_intern + col] = *((CELL *) value);
	}
      else if (data->type == FCELL_TYPE && data->fcell_array != NULL)
	{
	  data->fcell_array[row * data->rows_intern + col] = *((FCELL *) value);
	}
      else if (data->type == DCELL_TYPE && data->dcell_array != NULL)
	{
	  data->dcell_array[row * data->rows_intern + col] = *((DCELL *) value);
	}
    }
  else
    {
      if (data->type == CELL_TYPE && data->cell_array != NULL)
	{
	  data->cell_array[(row + data->offset) * data->rows_intern + col + data->offset] = *((CELL *) value);
	}
      else if (data->type == FCELL_TYPE && data->fcell_array != NULL)
	{
	  data->fcell_array[(row + data->offset) * data->rows_intern + col + data->offset] = *((FCELL *) value);
	}
      else if (data->type == DCELL_TYPE && data->dcell_array != NULL)
	{
	  data->dcell_array[(row + data->offset) * data->rows_intern + col + data->offset] = *((DCELL *) value);
	}
    }

  return;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes a CELL value to the N_array_2d data at position row, col
 *
 * \param data N_array_2d * 
 * \param row int
 * \param col int
 * \param value CELL
 * \return void
 * */
inline void
N_put_array_2d_value_cell (N_array_2d * data, int row, int col, CELL value)
{
  N_put_array_2d_value (data, row, col, (char *) &value);
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes a FCELL value to the N_array_2d data at position row, col
 *
 * \param data N_array_2d *
 * \param row int
 * \param col int
 * \param value FCELL
 * \return void
 * */
inline void
N_put_array_2d_value_fcell (N_array_2d * data, int row, int col, FCELL value)
{
  N_put_array_2d_value (data, row, col, (char *) &value);
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes a DCELL value to the N_array_2d data at position row, col
 *
 * \param data N_array_2d *
 * \param row int
 * \param col int
 * \param value DCELL
 * \return void
 * */
inline void
N_put_array_2d_value_dcell (N_array_2d * data, int row, int col, DCELL value)
{
  N_put_array_2d_value (data, row, col, (char *) &value);
}


/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function copies the source N_array_2d to the target N_array_2d
 *
 * \param source N_array_2d *
 * \param target N_array_2d *
 * \return void
 * */
void
N_array_2d_copy (N_array_2d * source, N_array_2d * target)
{
  int i;

  if (source->cols_intern != target->cols_intern)
    G_fatal_error ("N_array_2d_copy: the arrays are not of equal size");

  if (source->rows_intern != target->rows_intern)
    G_fatal_error ("N_array_2d_copy: the arrays are not of equal size");

  G_debug (3, "N_array_2d_copy: copy source array to target array size %i", source->cols_intern * source->rows_intern);

  for (i = 0; i < source->cols_intern * source->rows_intern; i++)
    {
      if (source->type == CELL_TYPE)
	{
	  if (target->type == CELL_TYPE)
	    target->cell_array[i] = source->cell_array[i];
	  if (target->type == FCELL_TYPE)
	    target->fcell_array[i] = (FCELL) source->cell_array[i];
	  if (target->type == DCELL_TYPE)
	    target->dcell_array[i] = (DCELL) source->cell_array[i];

	}
      if (source->type == FCELL_TYPE)
	{
	  if (target->type == CELL_TYPE)
	    target->cell_array[i] = (CELL) source->fcell_array[i];
	  if (target->type == FCELL_TYPE)
	    target->fcell_array[i] = source->fcell_array[i];
	  if (target->type == DCELL_TYPE)
	    target->dcell_array[i] = (DCELL) source->fcell_array[i];
	}
      if (source->type == DCELL_TYPE)
	{
	  if (target->type == CELL_TYPE)
	    target->cell_array[i] = (CELL) source->dcell_array[i];
	  if (target->type == FCELL_TYPE)
	    target->fcell_array[i] = (FCELL) source->dcell_array[i];
	  if (target->type == DCELL_TYPE)
	    target->dcell_array[i] = source->dcell_array[i];
	}
    }

  return;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function calculates the norm of the two input arrays
 *
 * \param array1 N_array_2d *
 * \param array2 N_array_2d *
 * \param type the type of the norm -> N_MAXIMUM_NORM, N_EUKLID_NORM
 * \return double the calculated norm
 * */
double
N_array_2d_norm (N_array_2d * array1, N_array_2d * array2, int type)
{
  int i;
  double norm, tmp;
  double v1, v2;

  v1 = 0.0;
  v2 = 0.0;
  norm = 0.0;

  if (array1->cols_intern != array2->cols_intern)
    G_fatal_error ("N_array_2d_copy: the arrays are not of equal size");

  if (array1->rows_intern != array2->rows_intern)
    G_fatal_error ("N_array_2d_copy: the arrays are not of equal size");

  G_debug (3, "N_array_2d_norm: norm of array1 and array2 size %g", array1->cols_intern * array1->rows_intern);

  for (i = 0; i < array1->cols_intern * array1->rows_intern; i++)
    {
      if (array1->type == CELL_TYPE)
	{
	  v1 = (double) array1->cell_array[i];

	}
      if (array1->type == FCELL_TYPE)
	{
	  v1 = (double) array1->fcell_array[i];
	}
      if (array1->type == DCELL_TYPE)
	{
	  v1 = (double) array1->dcell_array[i];
	}

      if (array2->type == CELL_TYPE)
	{
	  v2 = (double) array2->cell_array[i];
	}
      if (array2->type == FCELL_TYPE)
	{
	  v2 = (double) array2->fcell_array[i];
	}
      if (array2->type == DCELL_TYPE)
	{
	  v2 = (double) array2->dcell_array[i];
	}

      if (type == N_MAXIMUM_NORM)
	{
	  tmp = fabs (v2 - v1);
	  if ((tmp > norm))
	    norm = tmp;
	}
      if (type == N_EUKLID_NORM)
	{
	  norm += fabs (v2 - v1);
	}
    }

  return norm;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief Allocate memory for a N_array_3d array structure. 
 *
 * This functions allocates an array of type N_array_3d and returns a pointer 
 * to the new allocated memory.
 *
 * The data type of this array set by "type" must be 
 * G3D_FLOAT or G3D_DOUBLE accordingly to the raster3d map datatypes.
 * The offsets sets the number of boundary cols, rows and depths. 
 * This option is useful to generate homogeneous Neumann boundary conditions around  
 * an array. The arrays are initialized with 0 by default.
 *
 * If the offset is greater then 0, negative indices are possible.
 * The data structure of a array with 3 depths, rows and cols and an offset of 1 
 * will looks like this:
 *
  \verbatim
 0  0  0  0  0
 0  0  0  0  0
 0  0  0  0  0
 0  0  0  0  0
 0  0  0  0  0

 0  0  0  0  0
 0  0  1  2  0
 0  3  4  5  0
 0  6  7  8  0
 0  0  0  0  0

 0  0  0  0  0
 0  9 10 11  0
 0 12 13 14  0
 0 15 16 17  0
 0  0  0  0  0

 0  0  0  0  0
 0 18 19 20  0
 0 21 22 23  0
 0 24 25 26  0
 0  0  0  0  0

 0  0  0  0  0
 0  0  0  0  0
 0  0  0  0  0
 0  0  0  0  0
 0  0  0  0  0

  \endverbatim

 The depth counts from the bottom to the top.

 * <br><br>
 * Internal a one dimensional array is allocated to speed up the memory access.
 * To access the one dimensional array with a three dimensional indexing use the provided 
 * get and put functions.
 *
 * \param depths int
 * \param rows int
 * \param cols int
 * \param offset int 
 * \param type int
 * \return N_array_3d *
 * 
 * */
N_array_3d *
N_alloc_array_3d (int depths, int rows, int cols, int offset, int type)
{
  N_array_3d *data = NULL;

  if (rows < 1 || cols < 1 || depths < 1)
    G_fatal_error ("N_alloc_array_3d: depths, cols and rows should be > 0");

  if (type != G3D_DOUBLE && type != G3D_FLOAT)
    G_fatal_error ("N_alloc_array_3d: Wrong data type, should be G3D_FLOAT or G3D_DOUBLE");

  data = (N_array_3d *) G_calloc (1, sizeof (N_array_3d));

  data->rows = rows;
  data->cols = cols;
  data->depths = depths;
  data->type = type;
  data->offset = offset;
  data->rows_intern = rows + 2 * offset;
  data->cols_intern = cols + 2 * offset;
  data->depths_intern = depths + 2 * offset;
  data->float_array = NULL;
  data->double_array = NULL;

  /*Allocation is in order of array[depths][rows][cols] */

  G_debug (3,
	   "N_alloc_array_3d: rows_intern %i cols_intern %i depths_intern %i offset %i\n",
	   data->rows_intern, data->cols_intern, data->depths_intern, data->offset = offset);

  if (data->type == G3D_FLOAT)
    {
      data->float_array =
	(float *) G_calloc (data->depths_intern * data->rows_intern * data->cols_intern, sizeof (float));
    }
  else if (data->type == G3D_DOUBLE)
    {
      data->double_array =
	(double *) G_calloc (data->depths_intern * data->rows_intern * data->cols_intern, sizeof (double));
    }

  return data;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief Release the memory of a N_array_3d 
 *
 * \param data N_array_3d *
 * \return void
 * */
void
N_free_array_3d (N_array_3d * data)
{

  if (data != NULL)
    {
      G_debug (3, "N_free_array_3d: free N_array_3d");

      if (data->type == G3D_FLOAT && data->float_array != NULL)
	{
	  G_free (data->float_array);
	}
      else if (data->type == G3D_DOUBLE && data->double_array != NULL)
	{
	  G_free (data->double_array);
	}

      G_free (data);
      data = NULL;

    }

  return;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief Return the data taype of the N_array_3d
 *
 * The data type can be G3D_FLOAT and G3D_DOUBLE accordingly to the raster map datatypes.
 *
 * \param data N_array_3d *
 * \return type int -- G3D_FLOAT or G3D_DOUBLE
 * */

int
N_get_array_3d_type (N_array_3d * data)
{
  return data->type;
}


/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes the value of N_array_3d data at position depth, row, col
 *        to the variable value
 *
 * \param data N_array_3d *
 * \param depth int
 * \param row int
 * \param col int
 * \param value void *
 * \return void
 * */
inline void
N_get_array_3d_value (N_array_3d * data, int depth, int row, int col, void *value)
{

  if (data->offset == 0)
    {
      if (data->type == G3D_FLOAT && data->float_array != NULL)
	{
	  *((float *) value) =
	    data->float_array[depth * (data->rows_intern * data->cols_intern) + row * data->rows_intern + col];
	}
      else if (data->type == G3D_DOUBLE && data->double_array != NULL)
	{
	  *((double *) value) =
	    data->double_array[depth * (data->rows_intern * data->cols_intern) + row * data->rows_intern + col];
	}
    }
  else
    {
      if (data->type == G3D_FLOAT && data->float_array != NULL)
	{
	  *((float *) value) = data->float_array[(depth + data->offset) * (data->rows_intern * data->cols_intern) +
						 (row + data->offset) * data->rows_intern + (col + data->offset)];

	}
      else if (data->type == G3D_DOUBLE && data->double_array != NULL)
	{
	  *((double *) value) = data->double_array[(depth + data->offset) * (data->rows_intern * data->cols_intern) +
						   (row + data->offset) * data->rows_intern + (col + data->offset)];
	}
    }

  return;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function returns the value of type float at position depth, row, col
 *
 * The data array can be of type float or double.
 *
 * \param data N_array_3d *
 * \param depth int
 * \param row int
 * \param col int
 * \return float
 *
 * */
inline float
N_get_array_3d_value_float (N_array_3d * data, int depth, int row, int col)
{
  float fvalue = 0.0;
  double dvalue = 0.0;

  switch (data->type)
    {
    case G3D_FLOAT:
      N_get_array_3d_value (data, depth, row, col, (void *) &fvalue);
      return (float) fvalue;
    case G3D_DOUBLE:
      N_get_array_3d_value (data, depth, row, col, (void *) &dvalue);
      return (float) dvalue;
    }

  return fvalue;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function returns the value of type float at position depth, row, col
 *
 * The data array can be of type float or double.
 *
 * \param data N_array_3d *
 * \param depth int
 * \param row int
 * \param col int
 * \return double
 *
 * */
inline double
N_get_array_3d_value_double (N_array_3d * data, int depth, int row, int col)
{
  float fvalue = 0.0;
  double dvalue = 0.0;

  switch (data->type)
    {

    case G3D_FLOAT:
      N_get_array_3d_value (data, depth, row, col, (void *) &fvalue);
      return (double) fvalue;
    case G3D_DOUBLE:
      N_get_array_3d_value (data, depth, row, col, (void *) &dvalue);
      return (double) dvalue;
    }

  return dvalue;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes a value to the N_array_3d data at position depth, row, col
 *
 * The value will be automatically cast to the array type.
 *
 * \param data N_array_3d *
 * \param depth int
 * \param row int 
 * \param col int
 * \param value cahr *
 * \return void
 * */
inline void
N_put_array_3d_value (N_array_3d * data, int depth, int row, int col, char *value)
{

  G_debug (6, "N_put_array_3d_value: adding value to array");

  if (data->offset == 0)
    {
      if (data->type == G3D_FLOAT && data->float_array != NULL)
	{
	  data->float_array[depth * (data->rows_intern * data->cols_intern) + row * data->rows_intern + col]
	    = *((float *) value);
	}
      else if (data->type == G3D_DOUBLE && data->double_array != NULL)
	{
	  data->double_array[depth * (data->rows_intern * data->cols_intern) + row * data->rows_intern + col]
	    = *((double *) value);
	}
    }
  else
    {
      if (data->type == G3D_FLOAT && data->float_array != NULL)
	{
	  data->float_array[(depth + data->offset) * (data->rows_intern * data->cols_intern) +
			    (row + data->offset) * data->rows_intern + (col + data->offset)] = *((float *) value);
	}
      else if (data->type == G3D_DOUBLE && data->double_array != NULL)
	{
	  data->double_array[(depth + data->offset) * (data->rows_intern * data->cols_intern) +
			     (row + data->offset) * data->rows_intern + (col + data->offset)] = *((double *) value);
	}
    }

  return;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes a float value to the N_array_3d data at position depth, row, col
 *
 * \param data N_array_3d *
 * \param depth int
 * \param row int
 * \param col int
 * \param value float
 * \return void
 * */
inline void
N_put_array_3d_value_float (N_array_3d * data, int depth, int row, int col, float value)
{
  N_put_array_3d_value (data, depth, row, col, (void *) &value);

  return;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes a double value to the N_array_3d data at position depth, row, col
 *
 * \param data N_array_3d *
 * \param depth int
 * \param row int
 * \param col int
 * \param value double
 * \return void
 * */
inline void
N_put_array_3d_value_double (N_array_3d * data, int depth, int row, int col, double value)
{
  N_put_array_3d_value (data, depth, row, col, (void *) &value);

  return;
}


/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function copies the source N_array_3d to the target N_array_3d
 *
 * \param source N_array_3d *
 * \param target N_array_3d *
 * \return void
 * */
void
N_array_3d_copy (N_array_3d * source, N_array_3d * target)
{
  int i;

  if (source->cols_intern != target->cols_intern)
    G_fatal_error ("N_array_3d_copy: the arrays are not of equal size");

  if (source->rows_intern != target->rows_intern)
    G_fatal_error ("N_array_3d_copy: the arrays are not of equal size");

  if (source->depths_intern != target->depths_intern)
    G_fatal_error ("N_array_3d_copy: the arrays are not of equal size");


  G_debug (3, "N_array_3d_copy: copy source array to target array size %i",
	   source->cols_intern * source->rows_intern * source->depths_intern);

  for (i = 0; i < source->cols_intern * source->rows_intern * source->depths_intern; i++)
    {
      if (source->type == G3D_FLOAT)
	{
	  if (target->type == G3D_FLOAT)
	    target->float_array[i] = source->float_array[i];
	  if (target->type == G3D_DOUBLE)
	    target->double_array[i] = (double) source->float_array[i];

	}
      if (source->type == G3D_DOUBLE)
	{
	  if (target->type == G3D_FLOAT)
	    target->float_array[i] = (float) source->double_array[i];
	  if (target->type == G3D_DOUBLE)
	    target->double_array[i] = source->double_array[i];
	}
    }

  return;
}

