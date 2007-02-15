
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
#include "grass/glocale.h"
#include <math.h>

/* ******************** 2D ARRAY FUNCTIONS *********************** */

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief Allocate memory for a N_array_2d array structure. 
 *
 * This function allocates memory for an array of type N_array_2d 
 * and returns the pointer to the new allocated memory.
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
 * \param cols int 
 * \param rows int 
 * \param offset int
 * \param type int
 * \return N_array_2d *
 * 
 * */
N_array_2d *N_alloc_array_2d(int cols, int rows, int offset, int type)
{
    N_array_2d *data = NULL;

    if (rows < 1 || cols < 1)
	G_fatal_error("N_alloc_array_2d: cols and rows should be > 0");

    if (type != CELL_TYPE && type != FCELL_TYPE && type != DCELL_TYPE)
	G_fatal_error
	    ("N_alloc_array_2d: Wrong data type, should be CELL_TYPE, FCELL_TYPE or DCELL_TYPE");

    data = (N_array_2d *) G_calloc(1, sizeof(N_array_2d));

    data->cols = cols;
    data->rows = rows;
    data->type = type;
    data->offset = offset;
    data->rows_intern = rows + 2 * offset;	/*offset position at booth sides */
    data->cols_intern = cols + 2 * offset;	/*offset position at booth sides */
    data->cell_array = NULL;
    data->fcell_array = NULL;
    data->dcell_array = NULL;

    /*Allocation is in order array[rows*cols] */


    if (data->type == CELL_TYPE) {
	data->cell_array =
	    (CELL *) G_calloc(data->rows_intern * data->cols_intern,
			      sizeof(CELL));
	G_debug(3,
		"N_alloc_array_2d: CELL array allocated rows_intern %i cols_intern %i offset %i",
		data->rows_intern, data->cols_intern, data->offset = offset);
    }
    else if (data->type == FCELL_TYPE) {
	data->fcell_array =
	    (FCELL *) G_calloc(data->rows_intern * data->cols_intern,
			       sizeof(FCELL));
	G_debug(3,
		"N_alloc_array_2d: FCELL array allocated rows_intern %i cols_intern %i offset %i",
		data->rows_intern, data->cols_intern, data->offset = offset);

    }
    else if (data->type == DCELL_TYPE) {
	data->dcell_array =
	    (DCELL *) G_calloc(data->rows_intern * data->cols_intern,
			       sizeof(DCELL));
	G_debug(3,
		"N_alloc_array_2d: DCELL array allocated rows_intern %i cols_intern %i offset %i",
		data->rows_intern, data->cols_intern, data->offset = offset);
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
void N_free_array_2d(N_array_2d * data)
{

    if (data != NULL) {
	G_debug(3, "N_free_array_2d: free N_array_2d");

	if (data->type == CELL_TYPE && data->cell_array != NULL) {
	    G_free(data->cell_array);
	}
	else if (data->type == FCELL_TYPE && data->fcell_array != NULL) {
	    G_free(data->fcell_array);

	}
	else if (data->type == DCELL_TYPE && data->dcell_array != NULL) {
	    G_free(data->dcell_array);
	}

	G_free(data);
	data = NULL;

    }

    return;
}

/*!
 * \brief Return the data taype of the N_array_2d
 *
 * The data type can be CELL_TYPE, FCELL_TYPE or DCELL_TYPE accordingly to the raster map datatypes.
 *
 * \param array N_array_2d * 
 * \return type int
 * */

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 *
 * */
int N_get_array_2d_type(N_array_2d * array)
{
    return array->type;
}

/*!
 * \brief This function writes the value of N_array_2d data at position col, row to the variable value
 *
 * \param data N_array_2d * 
 * \param col int
 * \param row int
 * \param value void * 
 * \return void
 * */

/***********************************************************
 * *********************************************************
 * ********************************************************/
inline void
N_get_array_2d_value(N_array_2d * data, int col, int row, void *value)
{

    if (data->offset == 0) {
	if (data->type == CELL_TYPE && data->cell_array != NULL) {
	    *((CELL *) value) = data->cell_array[row * data->cols_intern + col];
	}
	else if (data->type == FCELL_TYPE && data->fcell_array != NULL) {
	    *((FCELL *) value) =
		data->fcell_array[row * data->cols_intern + col];
	}
	else if (data->type == DCELL_TYPE && data->dcell_array != NULL) {
	    *((DCELL *) value) =
		data->dcell_array[row * data->cols_intern + col];
	}
    }
    else {
	if (data->type == CELL_TYPE && data->cell_array != NULL) {
	    *((CELL *) value) =
		data->cell_array[(row + data->offset) * data->cols_intern +
				 col + data->offset];
	}
	else if (data->type == FCELL_TYPE && data->fcell_array != NULL) {
	    *((FCELL *) value) =
		data->fcell_array[(row + data->offset) * data->cols_intern +
				  col + data->offset];
	}
	else if (data->type == DCELL_TYPE && data->dcell_array != NULL) {
	    *((DCELL *) value) =
		data->dcell_array[(row + data->offset) * data->cols_intern +
				  col + data->offset];
	}
    }

    return;
}

/*!
 * \brief This function returns 1 if the value of N_array_2d at postion col, row 
 * is of type null, otherwise 0 
 *
 * \param data N_array_2d * 
 * \param col int
 * \param row int
 * \return int
 * */

/***********************************************************
 * *********************************************************
 * ********************************************************/
inline int N_is_array_2d_value_null(N_array_2d * data, int col, int row)
{

    if (data->offset == 0) {
	if (data->type == CELL_TYPE && data->cell_array != NULL) {
	    G_debug(6,
		    "N_is_array_2d_value_null: null value is of type CELL at pos [%i][%i]",
		    col, row);
	    return G_is_null_value((void *)
				   &(data->
				     cell_array[row * data->cols_intern + col]),
				   CELL_TYPE);
	}
	else if (data->type == FCELL_TYPE && data->fcell_array != NULL) {
	    G_debug(6,
		    "N_is_array_2d_value_null: null value is of type FCELL at pos [%i][%i]",
		    col, row);
	    return G_is_null_value((void *)
				   &(data->
				     fcell_array[row * data->cols_intern +
						 col]), FCELL_TYPE);
	}
	else if (data->type == DCELL_TYPE && data->dcell_array != NULL) {
	    G_debug(6,
		    "N_is_array_2d_value_null: null value is of type DCELL at pos [%i][%i]",
		    col, row);
	    return G_is_null_value((void *)
				   &(data->
				     dcell_array[row * data->cols_intern +
						 col]), DCELL_TYPE);
	}
    }
    else {
	if (data->type == CELL_TYPE && data->cell_array != NULL) {
	    G_debug(6,
		    "N_is_array_2d_value_null: null value is of type CELL at pos [%i][%i]",
		    col, row);
	    return G_is_null_value((void *)
				   &(data->
				     cell_array[(row +
						 data->offset) *
						data->cols_intern + col +
						data->offset]), CELL_TYPE);
	}
	else if (data->type == FCELL_TYPE && data->fcell_array != NULL) {
	    G_debug(6,
		    "N_is_array_2d_value_null: null value is of type FCELL at pos [%i][%i]",
		    col, row);
	    return G_is_null_value((void *)
				   &(data->
				     fcell_array[(row +
						  data->offset) *
						 data->cols_intern + col +
						 data->offset]), FCELL_TYPE);
	}
	else if (data->type == DCELL_TYPE && data->dcell_array != NULL) {
	    G_debug(6,
		    "N_is_array_2d_value_null: null value is of type DCELL at pos [%i][%i]",
		    col, row);
	    return G_is_null_value((void *)
				   &(data->
				     dcell_array[(row +
						  data->offset) *
						 data->cols_intern + col +
						 data->offset]), DCELL_TYPE);
	}
    }

    return 0;
}


/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function returns the value of type CELL at position col, row 
 *
 * The data array can be of type CELL, FCELL or DCELL.
 *
 * \param data N_array_2d * 
 * \param col int
 * \param row int
 * \return CELL
 *        
 * */
inline CELL N_get_array_2d_value_cell(N_array_2d * data, int col, int row)
{
    CELL value = 0;
    FCELL fvalue = 0.0;
    DCELL dvalue = 0.0;

    switch (data->type) {
    case CELL_TYPE:
	N_get_array_2d_value(data, col, row, (void *)&value);
	return (CELL) value;
    case FCELL_TYPE:
	N_get_array_2d_value(data, col, row, (void *)&fvalue);
	return (CELL) fvalue;
    case DCELL_TYPE:
	N_get_array_2d_value(data, col, row, (void *)&dvalue);
	return (CELL) dvalue;
    }

    return value;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function returns the value of type FCELL at position col, row 
 *        
 * The data array can be of type CELL, FCELL or DCELL.
 *
 * \param data N_array_2d * 
 * \param col int
 * \param row int
 * \return FCELL
 
 * */
inline FCELL N_get_array_2d_value_fcell(N_array_2d * data, int col, int row)
{
    CELL value = 0;
    FCELL fvalue = 0.0;
    DCELL dvalue = 0.0;

    switch (data->type) {
    case CELL_TYPE:
	N_get_array_2d_value(data, col, row, (void *)&value);
	return (FCELL) value;
    case FCELL_TYPE:
	N_get_array_2d_value(data, col, row, (void *)&fvalue);
	return (FCELL) fvalue;
    case DCELL_TYPE:
	N_get_array_2d_value(data, col, row, (void *)&dvalue);
	return (FCELL) dvalue;
    }

    return fvalue;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function returns the value of type DCELL at position col, row 
 *
 * The data array can be of type CELL, FCELL or DCELL.
 * 
 * \param data N_array_2d *
 * \param col int
 * \param row int
 * \return DCELL
 *        
 * */
inline DCELL N_get_array_2d_value_dcell(N_array_2d * data, int col, int row)
{
    CELL value = 0;
    FCELL fvalue = 0.0;
    DCELL dvalue = 0.0;

    switch (data->type) {
    case CELL_TYPE:
	N_get_array_2d_value(data, col, row, (void *)&value);
	return (DCELL) value;
    case FCELL_TYPE:
	N_get_array_2d_value(data, col, row, (void *)&fvalue);
	return (DCELL) fvalue;
    case DCELL_TYPE:
	N_get_array_2d_value(data, col, row, (void *)&dvalue);
	return (DCELL) dvalue;
    }

    return dvalue;

}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes a value to the N_array_2d data at position col, row
 *
 * The value will be automatically cast to the array type.
 *
 * \param data N_array_2d *
 * \param col int
 * \param row int
 * \param value char *
 * \return void
 * */
inline void
N_put_array_2d_value(N_array_2d * data, int col, int row, char *value)
{

    G_debug(6, "N_put_array_2d_value: put value to array");

    if (data->offset == 0) {
	if (data->type == CELL_TYPE && data->cell_array != NULL) {
	    data->cell_array[row * data->cols_intern + col] = *((CELL *) value);
	}
	else if (data->type == FCELL_TYPE && data->fcell_array != NULL) {
	    data->fcell_array[row * data->cols_intern + col] =
		*((FCELL *) value);
	}
	else if (data->type == DCELL_TYPE && data->dcell_array != NULL) {
	    data->dcell_array[row * data->cols_intern + col] =
		*((DCELL *) value);
	}
    }
    else {
	if (data->type == CELL_TYPE && data->cell_array != NULL) {
	    data->cell_array[(row + data->offset) * data->cols_intern + col +
			     data->offset] = *((CELL *) value);
	}
	else if (data->type == FCELL_TYPE && data->fcell_array != NULL) {
	    data->fcell_array[(row + data->offset) * data->cols_intern + col +
			      data->offset] = *((FCELL *) value);
	}
	else if (data->type == DCELL_TYPE && data->dcell_array != NULL) {
	    data->dcell_array[(row + data->offset) * data->cols_intern + col +
			      data->offset] = *((DCELL *) value);
	}
    }

    return;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes the null value to the N_array_2d data at position col, row
 *
 * The null value will be automatically set to the array type.
 *
 * \param data N_array_2d *
 * \param col int
 * \param row int
 * \return void
 * */
inline void N_put_array_2d_value_null(N_array_2d * data, int col, int row)
{

    G_debug(6,
	    "N_put_array_2d_value_null: put null value to array pos [%i][%i]",
	    col, row);

    if (data->offset == 0) {
	if (data->type == CELL_TYPE && data->cell_array != NULL) {
	    G_set_c_null_value((void *)
			       &(data->
				 cell_array[row * data->cols_intern + col]), 1);
	}
	else if (data->type == FCELL_TYPE && data->fcell_array != NULL) {
	    G_set_f_null_value((void *)
			       &(data->
				 fcell_array[row * data->cols_intern + col]),
			       1);
	}
	else if (data->type == DCELL_TYPE && data->dcell_array != NULL) {
	    G_set_d_null_value((void *)
			       &(data->
				 dcell_array[row * data->cols_intern + col]),
			       1);
	}
    }
    else {
	if (data->type == CELL_TYPE && data->cell_array != NULL) {
	    G_set_c_null_value((void *)
			       &(data->
				 cell_array[(row +
					     data->offset) * data->cols_intern +
					    col + data->offset]), 1);
	}
	else if (data->type == FCELL_TYPE && data->fcell_array != NULL) {
	    G_set_f_null_value((void *)
			       &(data->
				 fcell_array[(row +
					      data->offset) *
					     data->cols_intern + col +
					     data->offset]), 1);
	}
	else if (data->type == DCELL_TYPE && data->dcell_array != NULL) {
	    G_set_d_null_value((void *)
			       &(data->
				 dcell_array[(row +
					      data->offset) *
					     data->cols_intern + col +
					     data->offset]), 1);
	}
    }

    return;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes a CELL value to the N_array_2d data at position col, row
 *
 * \param data N_array_2d * 
 * \param col int
 * \param row int
 * \param value CELL
 * \return void
 * */
inline void
N_put_array_2d_value_cell(N_array_2d * data, int col, int row, CELL value)
{
    N_put_array_2d_value(data, col, row, (char *)&value);
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes a FCELL value to the N_array_2d data at position col, row
 *
 * \param data N_array_2d *
 * \param col int
 * \param row int
 * \param value FCELL
 * \return void
 * */
inline void
N_put_array_2d_value_fcell(N_array_2d * data, int col, int row, FCELL value)
{
    N_put_array_2d_value(data, col, row, (char *)&value);
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes a DCELL value to the N_array_2d data at position col, row
 *
 * \param data N_array_2d *
 * \param col int
 * \param row int
 * \param value DCELL
 * \return void
 * */
inline void
N_put_array_2d_value_dcell(N_array_2d * data, int col, int row, DCELL value)
{
    N_put_array_2d_value(data, col, row, (char *)&value);
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes the content of the array data to stdout
 *
 * \param data N_array_2d *
 * \return void
 * */
void N_print_array_2d(N_array_2d * data)
{
    int i, j;

    for (j = 0; j < data->rows; j++) {
	for (i = 0; i < data->cols; i++) {
	    if (data->type == CELL_TYPE)
		printf("%6d ", N_get_array_2d_value_cell(data, i, j));
	    else if (data->type == FCELL_TYPE)
		printf("%6.6f ", N_get_array_2d_value_fcell(data, i, j));
	    else if (data->type == DCELL_TYPE)
		printf("%6.6f ", N_get_array_2d_value_dcell(data, i, j));
	}
	printf("\n");
    }
    printf("\n");

    return;
}




/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function copies the source N_array_2d to the target N_array_2d
 *
 * The array types can be mixed, the values are automatically casted
 * and the null values are set accordingly.
 *
 * So if you copy a cell array into a dcell array, the values are casted to dcell and 
 * the null values are converted from cell-null to dcell-null
 *
 * \param source N_array_2d *
 * \param target N_array_2d *
 * \return void
 * */
void N_copy_array_2d(N_array_2d * source, N_array_2d * target)
{
    int i;
    int null = 0;

#pragma omp single
    {
	if (source->cols_intern != target->cols_intern)
	    G_fatal_error("N_copy_array_2d: the arrays are not of equal size");

	if (source->rows_intern != target->rows_intern)
	    G_fatal_error("N_copy_array_2d: the arrays are not of equal size");

	G_debug(3, "N_copy_array_2d: copy source array to target array size %i",
		source->cols_intern * source->rows_intern);
    }

#pragma omp for
    for (i = 0; i < source->cols_intern * source->rows_intern; i++) {
	null = 0;
	if (source->type == CELL_TYPE) {
	    if (G_is_c_null_value((void *)&source->cell_array[i]))
		null = 1;

	    if (target->type == CELL_TYPE) {
		target->cell_array[i] = source->cell_array[i];
	    }
	    if (target->type == FCELL_TYPE) {
		if (null)
		    G_set_f_null_value((void *)&(target->fcell_array[i]), 1);
		else
		    target->fcell_array[i] = (FCELL) source->cell_array[i];
	    }
	    if (target->type == DCELL_TYPE) {
		if (null)
		    G_set_d_null_value((void *)&(target->dcell_array[i]), 1);
		else
		    target->dcell_array[i] = (DCELL) source->cell_array[i];
	    }

	}
	if (source->type == FCELL_TYPE) {
	    if (G_is_f_null_value((void *)&source->fcell_array[i]))
		null = 1;

	    if (target->type == CELL_TYPE) {
		if (null)
		    G_set_c_null_value((void *)&(target->cell_array[i]), 1);
		else
		    target->cell_array[i] = (CELL) source->fcell_array[i];
	    }
	    if (target->type == FCELL_TYPE) {
		target->fcell_array[i] = source->fcell_array[i];
	    }
	    if (target->type == DCELL_TYPE) {
		if (null)
		    G_set_d_null_value((void *)&(target->dcell_array[i]), 1);
		else
		    target->dcell_array[i] = (DCELL) source->fcell_array[i];
	    }
	}
	if (source->type == DCELL_TYPE) {
	    if (G_is_d_null_value((void *)&source->dcell_array[i]))
		null = 1;

	    if (target->type == CELL_TYPE) {
		if (null)
		    G_set_c_null_value((void *)&(target->cell_array[i]), 1);
		else
		    target->cell_array[i] = (CELL) source->dcell_array[i];
	    }
	    if (target->type == FCELL_TYPE) {
		if (null)
		    G_set_f_null_value((void *)&(target->fcell_array[i]), 1);
		else
		    target->fcell_array[i] = (FCELL) source->dcell_array[i];
	    }
	    if (target->type == DCELL_TYPE) {
		target->dcell_array[i] = source->dcell_array[i];
	    }
	}
    }

    return;
}

/********************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief This function calculates the norm of the two input arrays
 *
 * \param a N_array_2d *
 * \param b N_array_2d *
 * \param type the type of the norm -> N_MAXIMUM_NORM, N_EUKLID_NORM
 * \return double the calculated norm
 * */
double N_norm_array_2d(N_array_2d * a, N_array_2d * b, int type)
{
    int i = 0;
    double norm = 0.0, tmp = 0.0;
    double v1 = 0.0, v2 = 0.0;

    if (a->cols_intern != b->cols_intern)
	G_fatal_error("N_norm_array_2d: the arrays are not of equal size");

    if (a->rows_intern != b->rows_intern)
	G_fatal_error("N_norm_array_2d: the arrays are not of equal size");

    G_debug(3, "N_norm_array_2d: norm of a and b size %i",
	    a->cols_intern * a->rows_intern);

    for (i = 0; i < a->cols_intern * a->rows_intern; i++) {
	v1 = 0.0;
	v2 = 0.0;

	if (a->type == CELL_TYPE) {
	    if (!G_is_f_null_value((void *)&(a->cell_array[i])))
		v1 = (double)a->cell_array[i];
	}
	if (a->type == FCELL_TYPE) {
	    if (!G_is_f_null_value((void *)&(a->fcell_array[i])))
		v1 = (double)a->fcell_array[i];
	}
	if (a->type == DCELL_TYPE) {
	    if (!G_is_f_null_value((void *)&(a->dcell_array[i])))
		v1 = (double)a->dcell_array[i];
	}
	if (b->type == CELL_TYPE) {
	    if (!G_is_f_null_value((void *)&(b->cell_array[i])))
		v2 = (double)b->cell_array[i];
	}
	if (b->type == FCELL_TYPE) {
	    if (!G_is_f_null_value((void *)&(b->fcell_array[i])))
		v2 = (double)b->fcell_array[i];
	}
	if (b->type == DCELL_TYPE) {
	    if (!G_is_f_null_value((void *)&(b->dcell_array[i])))
		v2 = (double)b->dcell_array[i];
	}

	if (type == N_MAXIMUM_NORM) {
	    tmp = fabs(v2 - v1);
	    if ((tmp > norm))
		norm = tmp;
	}
	if (type == N_EUKLID_NORM) {
	    norm += fabs(v2 - v1);
	}
    }

    return norm;
}

/********************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief This function performes calculates with two input arrays, 
 * the result is written to a third array.
 *
 * All arrays must have equal sizes and offsets.
 * The complete data array inclusively offsets is used for calucaltions.
 * Only non-null values are used. If one array value is null, 
 * the result array value will be null too.
 * 
 * If a division with zero is detected, the resulting arrays 
 * value will set to null and not to NaN.
 *
 * The result array is optional, if the result arrays poins to NULL,
 * a new array will be allocated with the largest arrays datatype
 * (CELL; FCELL or DCELL) used by the input arrays.
 *
 * the calculations are of the following form:
 *
 * <ul>
 * <li>result = a + b -> N_ARRAY_SUM</li>
 * <li>result = a - b -> N_ARRAY_DIF</li>
 * <li>result = a * b -> N_ARRAY_MUL</li>
 * <li>result = a / b -> N_ARRAY_DIV</li>
 * </ul>
 *
 * \param a N_array_2d * - first input array
 * \param b N_array_2d * - second input array
 * \param result N_array_2d * - the optional result array
 * \param type  - the type of calculation
 * \return N_array_2d * - the pointer to the result array
 * */
N_array_2d *N_math_array_2d(N_array_2d * a, N_array_2d * b, N_array_2d * result,
			    int type)
{
    N_array_2d *c;
    int i, j, setnull = 0;
    double va = 0.0, vb = 0.0, vc = 0.0;	/*variables used for calculation */

    /*Set the pointer */
    c = result;

    /*Check the array sizes */
    if (a->cols_intern != b->cols_intern)
	G_fatal_error("N_math_array_2d: the arrays are not of equal size");
    if (a->rows_intern != b->rows_intern)
	G_fatal_error("N_math_array_2d: the arrays are not of equal size");
    if (a->offset != b->offset)
	G_fatal_error("N_math_array_2d: the arrays have different offsets");

    G_debug(3, "N_math_array_2d: mathematical calculations, size: %i",
	    a->cols_intern * a->rows_intern);

    /*if the result array is null, allocate a new one, use the 
     * largest data type of the input arrays*/
    if (c == NULL) {
	if (a->type == DCELL_TYPE || b->type == DCELL_TYPE) {
	    c = N_alloc_array_2d(a->cols, a->rows, a->offset, DCELL_TYPE);
	    G_debug(3, "N_math_array_2d: array of type DCELL_TYPE created");
	}
	else if (a->type == FCELL_TYPE || b->type == FCELL_TYPE) {
	    c = N_alloc_array_2d(a->cols, a->rows, a->offset, FCELL_TYPE);
	    G_debug(3, "N_math_array_2d: array of type FCELL_TYPE created");
	}
	else {
	    c = N_alloc_array_2d(a->cols, a->rows, a->offset, CELL_TYPE);
	    G_debug(3, "N_math_array_2d: array of type CELL_TYPE created");
	}
    }
    else {
	/*Check the array sizes */
	if (a->cols_intern != c->cols_intern)
	    G_fatal_error("N_math_array_2d: the arrays are not of equal size");
	if (a->rows_intern != c->rows_intern)
	    G_fatal_error("N_math_array_2d: the arrays are not of equal size");
	if (a->offset != c->offset)
	    G_fatal_error("N_math_array_2d: the arrays have different offsets");
    }

    for (j = 0 - a->offset; j < a->rows + a->offset; j++) {
	for (i = 0 - a->offset; i < a->cols + a->offset; i++) {
	    if (!N_is_array_2d_value_null(a, i, j) &&
		!N_is_array_2d_value_null(a, i, j)) {
		/*we always calulate internally with double values */
		va = (double)N_get_array_2d_value_dcell(a, i, j);
		vb = (double)N_get_array_2d_value_dcell(b, i, j);
		vc = 0;
		setnull = 0;

		switch (type) {
		case N_ARRAY_SUM:
		    vc = va + vb;
		    break;
		case N_ARRAY_DIF:
		    vc = va - vb;
		    break;
		case N_ARRAY_MUL:
		    vc = va * vb;
		    break;
		case N_ARRAY_DIV:
		    if (vb != 0)
			vc = va / vb;
		    else
			setnull = 1;
		    break;
		}

		if (c->type == CELL_TYPE) {
		    if (setnull)
			N_put_array_2d_value_null(c, i, j);
		    else
			N_put_array_2d_value_cell(c, i, j, (CELL) vc);
		}
		if (c->type == FCELL_TYPE) {
		    if (setnull)
			N_put_array_2d_value_null(c, i, j);
		    else
			N_put_array_2d_value_fcell(c, i, j, (FCELL) vc);
		}
		if (c->type == DCELL_TYPE) {
		    if (setnull)
			N_put_array_2d_value_null(c, i, j);
		    else
			N_put_array_2d_value_dcell(c, i, j, (DCELL) vc);
		}

	    }
	    else {
		N_put_array_2d_value_null(c, i, j);
	    }
	}
    }

    return c;
}


/********************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief This function converts all null values into zero values
 *
 * The complete data array inclusively offsets is used.
 *
 * \param a N_array_2d *
 * \return int - number of replaced values
 * */
int N_convert_array_2d_null_to_zero(N_array_2d * a)
{
    int i = 0, count = 0;

    G_debug(3, "N_convert_array_2d_null_to_zero: convert array of size %i",
	    a->cols_intern * a->rows_intern);

    if (a->type == CELL_TYPE)
	for (i = 0; i < a->cols_intern * a->rows_intern; i++) {
	    if (G_is_c_null_value((void *)&(a->cell_array[i]))) {
		a->cell_array[i] = 0;
		count++;
	    }
	}

    if (a->type == FCELL_TYPE)
	for (i = 0; i < a->cols_intern * a->rows_intern; i++) {
	    if (G_is_f_null_value((void *)&(a->fcell_array[i]))) {
		a->fcell_array[i] = 0.0;
		count++;
	    }
	}


    if (a->type == DCELL_TYPE)
	for (i = 0; i < a->cols_intern * a->rows_intern; i++) {
	    if (G_is_d_null_value((void *)&(a->dcell_array[i]))) {
		a->dcell_array[i] = 0.0;
		count++;
	    }
	}


    if (a->type == CELL_TYPE)
	G_debug(3,
		"N_convert_array_2d_null_to_zero: %i values of type CELL_TYPE are converted",
		count);
    if (a->type == FCELL_TYPE)
	G_debug(3,
		"N_convert_array_2d_null_to_zero: %i valuess of type FCELL_TYPE are converted",
		count);
    if (a->type == DCELL_TYPE)
	G_debug(3,
		"N_convert_array_2d_null_to_zero: %i valuess of type DCELL_TYPE are converted",
		count);

    return count;
}


/********************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief Read a raster map into a N_array_2d structure
 *
 * The raster map is opened in the current region settings.
 * If no N_array_2d structure is provided (NULL pointer), a new structure will be
 * allocated with the same datatype as the raster map and the size of the current region. 
 * The array offset will be set to 0.
 *
 * If a N_array_2d structure is provided, the values from the raster map are 
 * casted to the N_array_2d type. The array must have the same size 
 * as the current region. 
 *
 * The new created or the provided array are returned.
 * If the reading of the raster map fails, G_fatal_error() will
 * be invoked.
 *
 * \param name * char - the name of an existing raster map
 * \param array * N_array_2d - an existing array or NULL
 * \return N_array_2d * - the existing or new allocated array
 * */
N_array_2d *N_read_rast_to_array_2d(char *name, N_array_2d * array)
{
    int map;			/*The rastermap */
    int x, y, cols, rows, type;
    void *rast;
    void *ptr;
    struct Cell_head region;
    N_array_2d *data = array;

    if (NULL == G_find_cell2(name, ""))
	G_fatal_error(_("Requested raster map <%s> not found"), name);

    /* Get the active region */
    G_get_set_window(&region);

    /*set the rows and cols */
    rows = region.rows;
    cols = region.cols;

    /*open the raster map */
    map = G_open_cell_old(name, G_find_cell2(name, ""));
    if (map < 0)
	G_fatal_error(_("Error opening raster map %s"), name);

    type = G_get_raster_map_type(map);

    /*if the array is NULL create a new one with the datatype of the raster map */
    /*the offset is 0 by default */
    if (data == NULL) {
	if (type == DCELL_TYPE) {
	    data = N_alloc_array_2d(cols, rows, 0, DCELL_TYPE);
	}
	if (type == FCELL_TYPE) {
	    data = N_alloc_array_2d(cols, rows, 0, FCELL_TYPE);
	}
	if (type == CELL_TYPE) {
	    data = N_alloc_array_2d(cols, rows, 0, CELL_TYPE);
	}
    }
    else {
	/*Check the array sizes */
	if (data->cols != cols)
	    G_fatal_error
		("N_read_rast_to_array_2d: the data array size is different from the current region settings");
	if (data->rows != rows)
	    G_fatal_error
		("N_read_rast_to_array_2d: the data array size is different from the current region settings");
    }

    rast = G_allocate_raster_buf(type);

    G_message(_("Reading raster map <%s> into memory"), name);

    for (y = 0; y < rows; y++) {
	G_percent(y, rows - 1, 10);

	if (!G_get_raster_row(map, rast, y, type)) {
	    G_close_cell(map);
	    G_fatal_error(_("Could not get raster row"));
	}

	for (x = 0, ptr = rast; x < cols;
	     x++, ptr = G_incr_void_ptr(ptr, G_raster_size(type))) {
	    if (type == CELL_TYPE) {
		if (data->type == CELL_TYPE)
		    N_put_array_2d_value_cell(data, x, y,
					      (CELL) * (CELL *) ptr);
		if (data->type == FCELL_TYPE)
		    N_put_array_2d_value_fcell(data, x, y,
					       (FCELL) * (CELL *) ptr);
		if (data->type == DCELL_TYPE)
		    N_put_array_2d_value_dcell(data, x, y,
					       (DCELL) * (CELL *) ptr);
	    }
	    if (type == FCELL_TYPE) {
		if (data->type == CELL_TYPE)
		    N_put_array_2d_value_cell(data, x, y,
					      (CELL) * (FCELL *) ptr);
		if (data->type == FCELL_TYPE)
		    N_put_array_2d_value_fcell(data, x, y,
					       (FCELL) * (FCELL *) ptr);
		if (data->type == DCELL_TYPE)
		    N_put_array_2d_value_dcell(data, x, y,
					       (DCELL) * (FCELL *) ptr);
	    }
	    if (type == DCELL_TYPE) {
		if (data->type == CELL_TYPE)
		    N_put_array_2d_value_cell(data, x, y,
					      (CELL) * (DCELL *) ptr);
		if (data->type == FCELL_TYPE)
		    N_put_array_2d_value_fcell(data, x, y,
					       (FCELL) * (DCELL *) ptr);
		if (data->type == DCELL_TYPE)
		    N_put_array_2d_value_dcell(data, x, y,
					       (DCELL) * (DCELL *) ptr);
	    }
	}
    }

    /* Close file */
    if (G_close_cell(map) < 0)
	G_fatal_error(_("Unable to close input map"));

    return data;
}


/********************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief Write a N_array_2d struct to a raster map
 *
 * A new raster map is created with the same type as the N_array_2d.
 * The current region is used to open the raster map.
 * The N_array_2d must have the same size as the current region.
 *
 * \param array N_array_2d * 
 * \param name char * - the name of the raster map
 * \return void
 *
 * */
void N_write_array_2d_to_rast(N_array_2d * array, char *name)
{
    int map;			/*The rastermap */
    int x, y, cols, rows, count, type;
    CELL *rast = NULL;
    FCELL *frast = NULL;
    DCELL *drast = NULL;
    struct Cell_head region;

    if (!array)
	G_fatal_error(_("N_array_2d * array is empty"));

    /* Get the current region */
    G_get_set_window(&region);

    rows = region.rows;
    cols = region.cols;
    type = array->type;

    /*Open the new map */
    map = G_open_raster_new(name, type);
    if (map < 0)
	G_fatal_error(_("Error opening raster map %s"), name);

    if (type == CELL_TYPE)
	rast = G_allocate_raster_buf(type);
    if (type == FCELL_TYPE)
	frast = G_allocate_raster_buf(type);
    if (type == DCELL_TYPE)
	drast = G_allocate_raster_buf(type);

    G_message(_("Write 2d array to raster map <%s>"), name);

    count = 0;
    for (y = 0; y < rows; y++) {
	G_percent(y, rows - 1, 10);
	for (x = 0; x < cols; x++) {
	    if (type == CELL_TYPE)
		rast[x] = N_get_array_2d_value_cell(array, x, y);
	    if (type == FCELL_TYPE)
		frast[x] = N_get_array_2d_value_fcell(array, x, y);
	    if (type == DCELL_TYPE)
		drast[x] = N_get_array_2d_value_dcell(array, x, y);
	}
	if (type == CELL_TYPE)
	    if (!G_put_c_raster_row(map, rast)) {
		G_unopen_cell(map);	/*unopen the new raster map */
		G_fatal_error(_("Unable to write rast row %i"), y);
	    }
	if (type == FCELL_TYPE)
	    if (!G_put_f_raster_row(map, frast)) {
		G_unopen_cell(map);	/*unopen the new raster map */
		G_fatal_error(_("Unable to write rast row %i"), y);
	    }
	if (type == DCELL_TYPE)
	    if (!G_put_d_raster_row(map, drast)) {
		G_unopen_cell(map);	/*unopen the new raster map */
		G_fatal_error(_("Unable to write rast row %i"), y);
	    }
    }

    /* Close file */
    if (G_close_cell(map) < 0)
	G_fatal_error(_("Unable to close input map"));

    return;

}


/* ******************** 3D ARRAY FUNCTIONS *********************** */


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
 * \param cols int
 * \param rows int
 * \param depths int
 * \param offset int 
 * \param type int
 * \return N_array_3d *
 * 
 * */
N_array_3d *N_alloc_array_3d(int cols, int rows, int depths, int offset,
			     int type)
{
    N_array_3d *data = NULL;

    if (rows < 1 || cols < 1 || depths < 1)
	G_fatal_error("N_alloc_array_3d: depths, cols and rows should be > 0");

    if (type != G3D_DOUBLE && type != G3D_FLOAT)
	G_fatal_error
	    ("N_alloc_array_3d: Wrong data type, should be G3D_FLOAT or G3D_DOUBLE");

    data = (N_array_3d *) G_calloc(1, sizeof(N_array_3d));

    data->cols = cols;
    data->rows = rows;
    data->depths = depths;
    data->type = type;
    data->offset = offset;
    data->rows_intern = rows + 2 * offset;
    data->cols_intern = cols + 2 * offset;
    data->depths_intern = depths + 2 * offset;
    data->float_array = NULL;
    data->double_array = NULL;

    /*Allocation is in order of array[depths][rows][cols] */


    if (data->type == G3D_FLOAT) {
	data->float_array =
	    (float *)G_calloc(data->depths_intern * data->rows_intern *
			      data->cols_intern, sizeof(float));
	G_debug(3,
		"N_alloc_array_3d: float array allocated rows_intern %i cols_intern %i depths_intern %i offset %i",
		data->rows_intern, data->cols_intern, data->depths_intern,
		data->offset = offset);
    }
    else if (data->type == G3D_DOUBLE) {
	data->double_array =
	    (double *)G_calloc(data->depths_intern * data->rows_intern *
			       data->cols_intern, sizeof(double));
	G_debug(3,
		"N_alloc_array_3d: double array allocated rows_intern %i cols_intern %i depths_intern %i offset %i",
		data->rows_intern, data->cols_intern, data->depths_intern,
		data->offset = offset);
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
void N_free_array_3d(N_array_3d * data)
{

    if (data != NULL) {
	G_debug(3, "N_free_array_3d: free N_array_3d");

	if (data->type == G3D_FLOAT && data->float_array != NULL) {
	    G_free(data->float_array);
	}
	else if (data->type == G3D_DOUBLE && data->double_array != NULL) {
	    G_free(data->double_array);
	}

	G_free(data);
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

int N_get_array_3d_type(N_array_3d * data)
{
    return data->type;
}


/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes the value of N_array_3d data at position col, row, depth
 *        to the variable value
 *
 * \param data N_array_3d *
 * \param col int
 * \param row int
 * \param depth int
 * \param value void *
 * \return void
 * */
inline void
N_get_array_3d_value(N_array_3d * data, int col, int row, int depth,
		     void *value)
{

    if (data->offset == 0) {
	if (data->type == G3D_FLOAT && data->float_array != NULL) {
	    *((float *)value) =
		data->float_array[depth *
				  (data->rows_intern * data->cols_intern) +
				  row * data->cols_intern + col];
	}
	else if (data->type == G3D_DOUBLE && data->double_array != NULL) {
	    *((double *)value) =
		data->double_array[depth *
				   (data->rows_intern * data->cols_intern) +
				   row * data->cols_intern + col];
	}
    }
    else {
	if (data->type == G3D_FLOAT && data->float_array != NULL) {
	    *((float *)value) =
		data->float_array[(depth + data->offset) *
				  (data->rows_intern * data->cols_intern) +
				  (row + data->offset) * data->cols_intern +
				  (col + data->offset)];

	}
	else if (data->type == G3D_DOUBLE && data->double_array != NULL) {
	    *((double *)value) =
		data->double_array[(depth + data->offset) *
				   (data->rows_intern * data->cols_intern) +
				   (row + data->offset) * data->cols_intern +
				   (col + data->offset)];
	}
    }

    return;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function returns 1 if value of N_array_3d data at position col, row, depth
 * is of type null, otherwise 0
 *
 * \param data N_array_3d *
 * \param col int
 * \param row int
 * \param depth int
 * \return void
 * */
inline int
N_is_array_3d_value_null(N_array_3d * data, int col, int row, int depth)
{

    if (data->offset == 0) {
	if (data->type == G3D_FLOAT && data->float_array != NULL) {
	    G_debug(6,
		    "N_is_array_3d_value_null: null value is of type G3D_DOUBLE at pos [%i][%i][%i]",
		    depth, row, col);
	    return G3d_isNullValueNum((void *)
				      &(data->
					float_array[depth *
						    (data->rows_intern *
						     data->cols_intern) +
						    row * data->cols_intern +
						    col]), G3D_FLOAT);
	}
	else if (data->type == G3D_DOUBLE && data->double_array != NULL) {
	    G_debug(6,
		    "N_is_array_3d_value_null: null value is of type G3D_DOUBLE at pos [%i][%i][%i]",
		    depth, row, col);
	    return G3d_isNullValueNum((void *)
				      &(data->
					double_array[depth *
						     (data->rows_intern *
						      data->cols_intern) +
						     row * data->cols_intern +
						     col]), G3D_DOUBLE);
	}
    }
    else {
	if (data->type == G3D_FLOAT && data->float_array != NULL) {
	    G_debug(6,
		    "N_is_array_3d_value_null: null value is of type G3D_DOUBLE at pos [%i][%i][%i]",
		    depth, row, col);
	    return G3d_isNullValueNum((void *)
				      &(data->
					float_array[(depth +
						     data->offset) *
						    (data->rows_intern *
						     data->cols_intern) + (row +
									   data->
									   offset)
						    * data->cols_intern + (col +
									   data->
									   offset)]),
				      G3D_FLOAT);

	}
	else if (data->type == G3D_DOUBLE && data->double_array != NULL) {
	    G_debug(6,
		    "N_is_array_3d_value_null: null value is of type G3D_DOUBLE at pos [%i][%i][%i]",
		    depth, row, col);
	    return G3d_isNullValueNum((void *)
				      &(data->
					double_array[(depth +
						      data->offset) *
						     (data->rows_intern *
						      data->cols_intern) +
						     (row +
						      data->offset) *
						     data->cols_intern + (col +
									  data->
									  offset)]),
				      G3D_DOUBLE);
	}
    }

    return 0;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function returns the value of type float at position col, row, depth
 *
 * The data array can be of type float or double.
 *
 * \param data N_array_3d *
 * \param col int
 * \param row int
 * \param depth int
 * \return float
 *
 * */
inline float
N_get_array_3d_value_float(N_array_3d * data, int col, int row, int depth)
{
    float fvalue = 0.0;
    double dvalue = 0.0;

    switch (data->type) {
    case G3D_FLOAT:
	N_get_array_3d_value(data, col, row, depth, (void *)&fvalue);
	return (float)fvalue;
    case G3D_DOUBLE:
	N_get_array_3d_value(data, col, row, depth, (void *)&dvalue);
	return (float)dvalue;
    }

    return fvalue;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function returns the value of type float at position col, row, depth
 *
 * The data array can be of type float or double.
 *
 * \param data N_array_3d *
 * \param col int
 * \param row int
 * \param depth int
 * \return double
 *
 * */
inline double
N_get_array_3d_value_double(N_array_3d * data, int col, int row, int depth)
{
    float fvalue = 0.0;
    double dvalue = 0.0;

    switch (data->type) {

    case G3D_FLOAT:
	N_get_array_3d_value(data, col, row, depth, (void *)&fvalue);
	return (double)fvalue;
    case G3D_DOUBLE:
	N_get_array_3d_value(data, col, row, depth, (void *)&dvalue);
	return (double)dvalue;
    }

    return dvalue;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes a value to the N_array_3d data at position col, row, depth
 *
 * The value will be automatically cast to the array type.
 *
 * \param data N_array_3d *
 * \param col int
 * \param row int 
 * \param depth int
 * \param value cahr *
 * \return void
 * */
inline void
N_put_array_3d_value(N_array_3d * data, int col, int row, int depth,
		     char *value)
{

    G_debug(6, "N_put_array_3d_value: put value to array at pos [%i][%i][%i]",
	    depth, row, col);

    if (data->offset == 0) {
	if (data->type == G3D_FLOAT && data->float_array != NULL) {
	    data->float_array[depth * (data->rows_intern * data->cols_intern) +
			      row * data->cols_intern + col]
		= *((float *)value);
	}
	else if (data->type == G3D_DOUBLE && data->double_array != NULL) {

	    data->double_array[depth * (data->rows_intern * data->cols_intern) +
			       row * data->cols_intern + col]
		= *((double *)value);
	}
    }
    else {
	if (data->type == G3D_FLOAT && data->float_array != NULL) {
	    data->float_array[(depth + data->offset) *
			      (data->rows_intern * data->cols_intern) + (row +
									 data->
									 offset)
			      * data->cols_intern + (col + data->offset)] =
		*((float *)value);
	}
	else if (data->type == G3D_DOUBLE && data->double_array != NULL) {
	    data->double_array[(depth + data->offset) *
			       (data->rows_intern * data->cols_intern) + (row +
									  data->
									  offset)
			       * data->cols_intern + (col + data->offset)] =
		*((double *)value);
	}
    }

    return;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes a null value to the N_array_3d data at position col, row, depth
 *
 * The null value will be automatically set to the array type.
 *
 * \param data N_array_3d *
 * \param col int
 * \param row int 
 * \param depth int
 * \return void
 * */
inline void
N_put_array_3d_value_null(N_array_3d * data, int col, int row, int depth)
{

    G_debug(6,
	    "N_put_array_3d_value_null: put null value to array at pos [%i][%i][%i]",
	    depth, row, col);

    if (data->offset == 0) {
	if (data->type == G3D_FLOAT && data->float_array != NULL) {
	    G3d_setNullValue((void *)
			     &(data->
			       float_array[depth *
					   (data->rows_intern *
					    data->cols_intern) +
					   row * data->cols_intern + col]), 1,
			     G3D_FLOAT);
	}
	else if (data->type == G3D_DOUBLE && data->double_array != NULL) {
	    G3d_setNullValue((void *)
			     &(data->
			       double_array[depth *
					    (data->rows_intern *
					     data->cols_intern) +
					    row * data->cols_intern + col]), 1,
			     G3D_DOUBLE);
	}
    }
    else {
	if (data->type == G3D_FLOAT && data->float_array != NULL) {
	    G3d_setNullValue((void *)
			     &(data->
			       float_array[(depth +
					    data->offset) * (data->rows_intern *
							     data->
							     cols_intern) +
					   (row +
					    data->offset) * data->cols_intern +
					   (col + data->offset)]), 1,
			     G3D_FLOAT);
	}
	else if (data->type == G3D_DOUBLE && data->double_array != NULL) {
	    G3d_setNullValue((void *)
			     &(data->
			       double_array[(depth +
					     data->offset) *
					    (data->rows_intern *
					     data->cols_intern) + (row +
								   data->
								   offset) *
					    data->cols_intern + (col +
								 data->
								 offset)]), 1,
			     G3D_DOUBLE);
	}
    }

    return;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes a float value to the N_array_3d data at position col, row, depth
 *
 * \param data N_array_3d *
 * \param col int
 * \param row int
 * \param depth int
 * \param value float
 * \return void
 * */
inline void
N_put_array_3d_value_float(N_array_3d * data, int col, int row, int depth,
			   float value)
{
    N_put_array_3d_value(data, col, row, depth, (void *)&value);

    return;
}

/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes a double value to the N_array_3d data at position col, row, depth
 *
 * \param data N_array_3d *
 * \param col int
 * \param row int
 * \param depth int
 * \param value double
 * \return void
 * */
inline void
N_put_array_3d_value_double(N_array_3d * data, int col, int row, int depth,
			    double value)
{
    N_put_array_3d_value(data, col, row, depth, (void *)&value);

    return;
}


/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function writes the content of the array data to stdout
 *
 * \param data N_array_2d *
 * \return void
 * */
void N_print_array_3d(N_array_3d * data)
{
    int i, j, k;

    for (k = 0; k < data->depths; k++) {
	for (j = 0; j < data->rows; j++) {
	    for (i = 0; i < data->cols; i++) {
		if (data->type == G3D_FLOAT)
		    printf("%6.6f ", N_get_array_3d_value_float(data, i, j, k));
		else if (data->type == G3D_DOUBLE)
		    printf("%6.6f ",
			   N_get_array_3d_value_double(data, i, j, k));
	    }
	    printf("\n");
	}
	printf("\n");
    }
    printf("\n");

    return;
}


/***********************************************************
 * *********************************************************
 * ********************************************************/
/*!
 * \brief This function copies the source N_array_3d to the target N_array_3d
 *
 * The array types can be mixed, the values are automatically casted
 * and the null values are set accordingly.
 *
 * So if you copy a float array to a double array, the values are casted to double and 
 * the null values are converted from float-null to double-null
 *
 * \param source N_array_3d *
 * \param target N_array_3d *
 * \return void
 * */
void N_copy_array_3d(N_array_3d * source, N_array_3d * target)
{
    int i;
    int null;

    if (source->cols_intern != target->cols_intern)
	G_fatal_error("N_copy_array_3d: the arrays are not of equal size");

    if (source->rows_intern != target->rows_intern)
	G_fatal_error("N_copy_array_3d: the arrays are not of equal size");

    if (source->depths_intern != target->depths_intern)
	G_fatal_error("N_copy_array_3d: the arrays are not of equal size");


    G_debug(3, "N_copy_array_3d: copy source array to target array size %i",
	    source->cols_intern * source->rows_intern * source->depths_intern);

    for (i = 0;
	 i < source->cols_intern * source->rows_intern * source->depths_intern;
	 i++) {
	null = 0;
	if (source->type == G3D_FLOAT) {
	    if (G3d_isNullValueNum
		((void *)&(source->float_array[i]), G3D_FLOAT))
		null = 1;

	    if (target->type == G3D_FLOAT) {
		target->float_array[i] = source->float_array[i];
	    }
	    if (target->type == G3D_DOUBLE) {
		if (null)
		    G3d_setNullValue((void *)&(target->double_array[i]), 1,
				     G3D_DOUBLE);
		else
		    target->double_array[i] = (double)source->float_array[i];
	    }

	}
	if (source->type == G3D_DOUBLE) {
	    if (G3d_isNullValueNum
		((void *)&(source->double_array[i]), G3D_DOUBLE))
		null = 1;

	    if (target->type == G3D_FLOAT) {
		if (null)
		    G3d_setNullValue((void *)&(target->float_array[i]), 1,
				     G3D_FLOAT);
		else
		    target->float_array[i] = (float)source->double_array[i];
	    }
	    if (target->type == G3D_DOUBLE) {
		target->double_array[i] = source->double_array[i];
	    }
	}
    }

    return;
}


/********************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief This function calculates the norm of the two input arrays
 *
 * All arrays must have equal sizes and offsets.
 * The complete data array inclusively offsets is used for norm calucaltion.
 * Only non-null values are used to calcualte the norm.
 *
 * \param a N_array_3d *
 * \param b N_array_3d *
 * \param type the type of the norm -> N_MAXIMUM_NORM, N_EUKLID_NORM
 * \return double the calculated norm
 * */
double N_norm_array_3d(N_array_3d * a, N_array_3d * b, int type)
{
    int i = 0;
    double norm = 0.0, tmp = 0.0;
    double v1 = 0.0, v2 = 0.0;

    if (a->cols_intern != b->cols_intern)
	G_fatal_error("N_norm_array_3d: the arrays are not of equal size");

    if (a->rows_intern != b->rows_intern)
	G_fatal_error("N_norm_array_3d: the arrays are not of equal size");

    if (a->depths_intern != b->depths_intern)
	G_fatal_error("N_norm_array_3d: the arrays are not of equal size");

    G_debug(3, "N_norm_array_3d: norm of a and b size %i",
	    a->cols_intern * a->rows_intern * a->depths_intern);

    for (i = 0; i < a->cols_intern * a->rows_intern * a->depths_intern; i++) {
	v1 = 0.0;
	v2 = 0.0;

	if (a->type == G3D_FLOAT) {
	    if (!G3d_isNullValueNum((void *)&(a->float_array[i]), G3D_FLOAT))
		v1 = (double)a->float_array[i];
	}
	if (a->type == G3D_DOUBLE) {
	    if (!G3d_isNullValueNum((void *)&(a->double_array[i]), G3D_DOUBLE))
		v1 = (double)a->double_array[i];
	}
	if (b->type == G3D_FLOAT) {
	    if (!G3d_isNullValueNum((void *)&(b->float_array[i]), G3D_FLOAT))
		v2 = (double)b->float_array[i];
	}
	if (b->type == G3D_DOUBLE) {
	    if (!G3d_isNullValueNum((void *)&(b->double_array[i]), G3D_DOUBLE))
		v2 = (double)b->double_array[i];
	}

	if (type == N_MAXIMUM_NORM) {
	    tmp = fabs(v2 - v1);
	    if ((tmp > norm))
		norm = tmp;
	}
	if (type == N_EUKLID_NORM) {
	    norm += fabs(v2 - v1);
	}
    }

    return norm;
}


/********************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief This function performes calculates of two input arrays, 
 * the result is written to a third array.
 *
 * All arrays must have equal sizes and offsets.
 * The complete data array inclusively offsets is used for calucaltions.
 * Only non-null values are used. If one array value is null, 
 * the result array value will be null too.
 *
 * If a division with zero is detected, the resulting arrays 
 * value will set to null and not to NaN.
 *
 * The result array is optional, if the result arrays poins to NULL,
 * a new array will be allocated with the largest arrays datatype
 * (G3D_FLOAT or G3D_DOUBLE) used by the input arrays.
 *
 * the calculations are of the following form:
 *
 * <ul>
 * <li>result = a + b -> N_ARRAY_SUM</li>
 * <li>result = a - b -> N_ARRAY_DIF</li>
 * <li>result = a * b -> N_ARRAY_MUL</li>
 * <li>result = a / b -> N_ARRAY_DIV</li>
 * </ul>
 *
 * \param a N_array_3d * - first input array
 * \param b N_array_3d * - second input array
 * \param result N_array_3d * - the optional result array
 * \param type  - the type of calculation
 * \return N_array_3d * - the pointer to the result array
 * */
N_array_3d *N_math_array_3d(N_array_3d * a, N_array_3d * b, N_array_3d * result,
			    int type)
{
    N_array_3d *c;
    int i, j, k, setnull = 0;
    double va = 0.0, vb = 0.0, vc = 0.0;	/*variables used for calculation */

    /*Set the pointer */
    c = result;

    /*Check the array sizes */
    if (a->cols_intern != b->cols_intern)
	G_fatal_error("N_math_array_3d: the arrays are not of equal size");
    if (a->rows_intern != b->rows_intern)
	G_fatal_error("N_math_array_3d: the arrays are not of equal size");
    if (a->depths_intern != b->depths_intern)
	G_fatal_error("N_math_array_3d: the arrays are not of equal size");
    if (a->offset != b->offset)
	G_fatal_error("N_math_array_3d: the arrays have different offsets");

    G_debug(3, "N_math_array_3d: mathematical calculations, size: %i",
	    a->cols_intern * a->rows_intern * a->depths_intern);

    /*if the result array is null, allocate a new one, use the 
     * largest data type of the input arrays*/
    if (c == NULL) {
	if (a->type == G3D_DOUBLE || b->type == G3D_DOUBLE) {
	    c = N_alloc_array_3d(a->cols, a->rows, a->depths, a->offset,
				 G3D_DOUBLE);
	    G_debug(3, "N_math_array_3d: array of type G3D_DOUBLE created");
	}
	else {
	    c = N_alloc_array_3d(a->cols, a->rows, a->depths, a->offset,
				 G3D_FLOAT);
	    G_debug(3, "N_math_array_3d: array of type G3D_FLOAT created");
	}
    }
    else {
	/*Check the array sizes */
	if (a->cols_intern != c->cols_intern)
	    G_fatal_error("N_math_array_3d: the arrays are not of equal size");
	if (a->rows_intern != c->rows_intern)
	    G_fatal_error("N_math_array_3d: the arrays are not of equal size");
	if (a->depths_intern != c->depths_intern)
	    G_fatal_error("N_math_array_3d: the arrays are not of equal size");
	if (a->offset != c->offset)
	    G_fatal_error("N_math_array_3d: the arrays have different offsets");
    }

    for (k = 0 - a->offset; k < a->depths + a->offset; k++) {
	for (j = 0 - a->offset; j < a->rows + a->offset; j++) {
	    for (i = 0 - a->offset; i < a->cols + a->offset; i++) {
		if (!N_is_array_3d_value_null(a, i, j, k) &&
		    !N_is_array_3d_value_null(a, i, j, k)) {
		    /*we always calulate internally with double values */
		    va = (double)N_get_array_3d_value_double(a, i, j, k);
		    vb = (double)N_get_array_3d_value_double(b, i, j, k);
		    vc = 0;
		    setnull = 0;

		    switch (type) {
		    case N_ARRAY_SUM:
			vc = va + vb;
			break;
		    case N_ARRAY_DIF:
			vc = va - vb;
			break;
		    case N_ARRAY_MUL:
			vc = va * vb;
			break;
		    case N_ARRAY_DIV:
			if (vb != 0)
			    vc = va / vb;
			else
			    setnull = 1;
			break;
		    }

		    if (c->type == G3D_FLOAT) {
			if (setnull)
			    N_put_array_3d_value_null(c, i, j, k);
			else
			    N_put_array_3d_value_float(c, i, j, k, (float)vc);
		    }
		    if (c->type == G3D_DOUBLE) {
			if (setnull)
			    N_put_array_3d_value_null(c, i, j, k);
			else
			    N_put_array_3d_value_double(c, i, j, k, vc);
		    }
		}
		else {
		    N_put_array_3d_value_null(c, i, j, k);
		}
	    }
	}
    }

    return c;
}



/********************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief This function converts all null values into zero values
 *
 * The complete data array inclusively offsets is used.
 *
 * \param a N_array_3d *
 * \return int - number of replaced null values
 * */
int N_convert_array_3d_null_to_zero(N_array_3d * a)
{
    int i = 0, count = 0;

    G_debug(3, "N_convert_array_3d_null_to_zero: convert array of size %i",
	    a->cols_intern * a->rows_intern * a->depths_intern);

    if (a->type == G3D_FLOAT)
	for (i = 0; i < a->cols_intern * a->rows_intern * a->depths_intern; i++) {
	    if (G3d_isNullValueNum((void *)&(a->float_array[i]), G3D_FLOAT)) {
		a->float_array[i] = 0.0;
		count++;
	    }
	}

    if (a->type == G3D_DOUBLE)
	for (i = 0; i < a->cols_intern * a->rows_intern * a->depths_intern; i++) {
	    if (G3d_isNullValueNum((void *)&(a->double_array[i]), G3D_DOUBLE)) {
		a->double_array[i] = 0.0;
		count++;
	    }
	}


    if (a->type == G3D_FLOAT)
	G_debug(3,
		"N_convert_array_3d_null_to_zero: %i values of type G3D_FLOAT are converted",
		count);

    if (a->type == G3D_DOUBLE)
	G_debug(3,
		"N_convert_array_3d_null_to_zero: %i values of type G3D_DOUBLE are converted",
		count);

    return count;
}



/* ************************************************************************* */
/* Read the data from a g3d map into an N_array_3d ************************* */
/* ************************************************************************* */
/*!
 * \brief Read a volume map into a N_array_3d structure
 *
 * The volume map is opened in the current region settings.
 * If no N_array_3d structure is provided (NULL pointer), a new structure will be
 * allocated with the same datatype as the volume map and the size of the current region. 
 * The array offset will be set to 0.
 *
 * If a N_array_3d structure is provided, the values from the volume map are 
 * casted to the N_array_3d type. The array must have the same size 
 * as the current region. 
 *
 * The new created or the provided array is returned.
 * If the reading of the volume map fails, G3d_fatalError() will
 * be invoked.
 *
 * \param name * char - the name of an existing volume map
 * \param array * N_array_3d - an existing array or NULL
 * \param mask int - 0 = false, 1 = ture : if a mask is presenent, use it with the input volume map
 * \return N_array_3d * - the existing or new allocated array
 * */
N_array_3d *N_read_rast3d_to_array_3d(char *name, N_array_3d * array, int mask)
{
    void *map = NULL;		/*The 3D Rastermap */
    int changemask = 0;
    int x, y, z, cols, rows, depths, type;
    double d1 = 0, f1 = 0;
    N_array_3d *data = array;
    G3D_Region region;


    /*get the current region */
    G3d_getWindow(&region);

    cols = region.cols;
    rows = region.rows;
    depths = region.depths;


    if (NULL == G_find_grid3(name, ""))
	G3d_fatalError(_("Requested g3d map <%s> not found"), name);

    /*Open all maps with default region */
    map =
	G3d_openCellOld(name, G_find_grid3(name, ""), G3D_DEFAULT_WINDOW,
			G3D_TILE_SAME_AS_FILE, G3D_USE_CACHE_DEFAULT);

    if (map == NULL)
	G3d_fatalError(_("Error opening g3d map <%s>"), name);

    type = G3d_tileTypeMap(map);

    /*if the array is NULL create a new one with the datatype of the volume map */
    /*the offset is 0 by default */
    if (data == NULL) {
	if (type == G3D_FLOAT) {
	    data = N_alloc_array_3d(cols, rows, depths, 0, G3D_FLOAT);
	}
	if (type == G3D_DOUBLE) {
	    data = N_alloc_array_3d(cols, rows, depths, 0, G3D_DOUBLE);
	}
    }
    else {
	/*Check the array sizes */
	if (data->cols != cols)
	    G_fatal_error
		("N_read_rast_to_array_3d: the data array size is different from the current region settings");
	if (data->rows != rows)
	    G_fatal_error
		("N_read_rast_to_array_3d: the data array size is different from the current region settings");
	if (data->depths != depths)
	    G_fatal_error
		("N_read_rast_to_array_3d: the data array size is different from the current region settings");
    }


    G_message(_("Read g3d map <%s> into the memory"), name);

    /*if requested set the Mask on */
    if (mask) {
	if (G3d_maskFileExists()) {
	    changemask = 0;
	    if (G3d_maskIsOff(map)) {
		G3d_maskOn(map);
		changemask = 1;
	    }
	}
    }

    for (z = 0; z < depths; z++) {	/*From the bottom to the top */
	G_percent(z, depths - 1, 10);
	for (y = 0; y < rows; y++) {
	    for (x = 0; x < cols; x++) {
		if (type == G3D_FLOAT) {
		    G3d_getValue(map, x, y, z, &f1, type);
		    if (data->type == G3D_FLOAT)
			N_put_array_3d_value_float(data, x, y, z, f1);
		    if (data->type == G3D_DOUBLE)
			N_put_array_3d_value_double(data, x, y, z, (double)f1);
		}
		else {
		    G3d_getValue(map, x, y, z, &d1, type);
		    if (data->type == G3D_FLOAT)
			N_put_array_3d_value_float(data, x, y, z, (float)d1);
		    if (data->type == G3D_DOUBLE)
			N_put_array_3d_value_double(data, x, y, z, d1);

		}
	    }
	}
    }

    /*We set the Mask off, if it was off before */
    if (mask) {
	if (G3d_maskFileExists())
	    if (G3d_maskIsOn(map) && changemask)
		G3d_maskOff(map);
    }

    /* Close files and exit */
    if (!G3d_closeCell(map))
	G3d_fatalError(map, NULL, 0, _("Error closing g3d file"));

    return data;
}

/********************************************************* *
 * ******************************************************* *
 * ******************************************************* */
/*!
 * \brief Write a N_array_3d struct to a volume map
 *
 * A new volume map is created with the same type as the N_array_3d.
 * The current region is used to open the volume map.
 * The N_array_3d must have the same size as the current region.
 * If the wrting of the volume map fails, G3d_fatalError() will
 * be invoked.
 *
 *
 * \param array N_array_3d * 
 * \param name char * - the name of the volume map
 * \return void
 *
 * */
void N_write_array_3d_to_rast3d(N_array_3d * array, char *name, int mask)
{
    void *map = NULL;		/*The 3D Rastermap */
    int changemask = 0;
    int x, y, z, cols, rows, depths, count, type;
    double d1 = 0.0, f1 = 0.0;
    N_array_3d *data = array;
    G3D_Region region;

    /*get the current region */
    G3d_getWindow(&region);


    cols = region.cols;
    rows = region.rows;
    depths = region.depths;
    type = data->type;

    /*Check the array sizes */
    if (data->cols != cols)
	G_fatal_error
	    ("N_write_array_3d_to_rast3d: the data array size is different from the current region settings");
    if (data->rows != rows)
	G_fatal_error
	    ("N_write_array_3d_to_rast3d: the data array size is different from the current region settings");
    if (data->depths != depths)
	G_fatal_error
	    ("N_write_array_3d_to_rast3d: the data array size is different from the current region settings");

    /*Open the new map */
    if (type == G3D_DOUBLE)
	map = G3d_openCellNew(name, G3D_DOUBLE, G3D_USE_CACHE_DEFAULT, &region);
    else if (type == G3D_FLOAT)
	map = G3d_openCellNew(name, G3D_FLOAT, G3D_USE_CACHE_DEFAULT, &region);

    if (map == NULL)
	G3d_fatalError(_("Error opening g3d map <%s>"), name);

    G_message(_("Write 3d array to g3d map <%s>"), name);

    /*if requested set the Mask on */
    if (mask) {
	if (G3d_maskFileExists()) {
	    changemask = 0;
	    if (G3d_maskIsOff(map)) {
		G3d_maskOn(map);
		changemask = 1;
	    }
	}
    }

    count = 0;
    for (z = 0; z < depths; z++) {	/*From the bottom to the top */
	G_percent(z, depths - 1, 10);
	for (y = 0; y < rows; y++) {
	    for (x = 0; x < cols; x++) {
		if (type == G3D_FLOAT) {
		    f1 = N_get_array_3d_value_float(data, x, y, z);
		    G3d_putFloat(map, x, y, z, f1);
		}
		else if (type == G3D_DOUBLE) {
		    d1 = N_get_array_3d_value_double(data, x, y, z);
		    G3d_putDouble(map, x, y, z, d1);
		}
	    }
	}
    }

    /*We set the Mask off, if it was off before */
    if (mask) {
	if (G3d_maskFileExists())
	    if (G3d_maskIsOn(map) && changemask)
		G3d_maskOff(map);
    }

    /* Close files and exit */
    if (!G3d_closeCell(map))
	G3d_fatalError(map, NULL, 0, _("Error closing g3d file"));

    return;
}
