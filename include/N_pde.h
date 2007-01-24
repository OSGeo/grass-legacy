
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:      This file contains definitions of variables and data types
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/

#include <grass/gis.h>
#include <grass/G3d.h>

#ifndef _N_PDE_H_
#define _N_PDE_H_

#define N_NORMAL_LES 0
#define N_SPARSE_LES 1

#define N_CELL_INACTIVE 0
#define N_CELL_ACTIVE 1
#define N_CELL_DIRICHLET 2

#define N_5_POINT_STAR 0
#define N_7_POINT_STAR 1
#define N_9_POINT_STAR 2

#define N_MAXIMUM_NORM 0
#define N_EUKLID_NORM 1

/* *************************************************************** */
/* *************** LINEARE EQUATION SYSTEM PART ****************** */
/* *************************************************************** */

/*!
 * \brief The row vector of the sparse matrix
 * */
typedef struct
{
  int cols;			/*Number of entries */
  double *values;		/*The non null values of the row */
  int *index;			/*the index number */
} N_spvector;


/*!
 * \brief The linear equation system (les) structure 
 *
 * This structure manages the Ax = b system.
 * It manages regular quadratic matrices or
 * sparse matrices. The vector b and x are normal one dimensional 
 * memory structures of type double. Also the number of rows
 * and the matrix type are stored in this structure.
 * */
typedef struct
{
  double *x;			/*the value vector */
  double *b;			/*the right side of Ax = b */
  double **A;			/*the normal quadratic matrix */
  N_spvector **Asp;		/*the sparse matrix */
  int rows;			/*number of rows */
  int type;			/*the type of the les, normal == 0, sparse == 1 */
} N_les;

extern N_spvector *N_alloc_spvector (int cols);
extern N_les *N_alloc_les (int rows, int type);
extern void N_print_les (N_les * les);
extern int N_add_spvector_to_les (N_les * les, N_spvector * vector, int row);
extern void N_free_spvector (N_spvector * vector);
extern void N_free_les (N_les * les);

/* *************************************************************** */
/* *************** GEOMETRY INFORMATION ************************** */
/* *************************************************************** */

/*!
 * \brief Geometric informations about the structured grid
 * */
typedef struct
{

  double dx;
  double dy;
  double dz;

  double Ax;
  double Ay;
  double Az;

  int depths;
  int rows;
  int cols;

} N_geom_data;

extern N_geom_data *N_alloc_geom_data ();


/* *************************************************************** */
/* *************** LINEARE EQUATION SOLVER PART ****************** */
/* *************************************************************** */
extern int N_solver_gauss (N_les * les);
extern int N_solver_lu (N_les * les);
extern int N_solver_cg (N_les * les, int maxit, double error);
extern int N_solver_bicgstab (N_les * les, int maxit, double error);

/* *************************************************************** */
/* *************** READING RASTER AND VOLUME DATA **************** */
/* *************************************************************** */

/*
 * \brief The 2d data array keeping the data for matrix assembling 
 * */
typedef struct
{
  int type;			/* which raster type CELL_TYPE, FCELL_TYPE, DCELL_TYPE */
  int rows, cols;
  int rows_intern, cols_intern;
  int offset;			/*number of cols/rows offset at each boundary */
  CELL *cell_array;		/*The data is stored in an one dimensional array internally */
  FCELL *fcell_array;		/*The data is stored in an one dimensional array internally */
  DCELL *dcell_array;		/*The data is stored in an one dimensional array internally */
} N_array_2d;

extern N_array_2d *N_alloc_array_2d (int rows, int cols, int offset, int type);
extern void N_free_array_2d (N_array_2d * data_array);
extern int N_get_array_2d_type (N_array_2d * array2d);
extern inline void N_get_array_2d_value (N_array_2d * array2d, int row, int col, void *value);
extern inline CELL N_get_array_2d_value_cell (N_array_2d * array2d, int row, int col);
extern inline FCELL N_get_array_2d_value_fcell (N_array_2d * array2d, int row, int col);
extern inline DCELL N_get_array_2d_value_dcell (N_array_2d * array2d, int row, int col);
extern inline void N_put_array_2d_value (N_array_2d * array2d, int row, int col, char *value);
extern inline void N_put_array_2d_value_cell (N_array_2d * array2d, int row, int col, CELL value);
extern inline void N_put_array_2d_value_fcell (N_array_2d * array2d, int row, int col, FCELL value);
extern inline void N_put_array_2d_value_dcell (N_array_2d * array2d, int row, int col, DCELL value);
void N_array_2d_copy (N_array_2d * source, N_array_2d * target);
double N_array_2d_norm (N_array_2d * array1, N_array_2d * array2, int type);

/*
 * \brief The 3d data array keeping the data for matrix assembling 
 * */
typedef struct
{
  int type;			/* which raster type G3D_FLOAT, G3D_DOUBLE */
  int rows, cols, depths;
  int rows_intern, cols_intern, depths_intern;
  int offset;			/*number of cols/rows/depths offset at each boundary */
  float *float_array;		/*The data is stored in an one dimensional array internally */
  double *double_array;		/*The data is stored in an one dimensional array internally */
} N_array_3d;

extern N_array_3d *N_alloc_array_3d (int depths, int rows, int cols, int offset, int type);
extern void N_free_array_3d (N_array_3d * data_array);
extern int N_get_array_3d_type (N_array_3d * array3d);
extern inline void N_get_array_3d_value (N_array_3d * array3d, int depth, int row, int col, void *value);
extern inline float N_get_array_3d_value_float (N_array_3d * array3d, int depth, int row, int col);
extern inline double N_get_array_3d_value_double (N_array_3d * array3d, int depth, int row, int col);
extern inline void N_put_array_3d_value (N_array_3d * array3d, int depth, int row, int col, char *value);
extern inline void N_put_array_3d_value_float (N_array_3d * array3d, int depth, int row, int col, float value);
extern inline void N_put_array_3d_value_double (N_array_3d * array3d, int depth, int row, int col, double value);
extern void N_array_3d_copy (N_array_3d * source, N_array_3d * target);

/* *************************************************************** */
/* *************** MATRIX ASSEMBLING METHODS ********************* */
/* *************************************************************** */
/*!
 * \brief Matrix entries for a mass balance 5/7/9 star system
 *
 * Matrix entries for the mass balance of a 5 star system
 *
 * The entries are center, east, west, north, south and the 
 * right side vector b of Ax = b. This system is typically used in 2d.

 \verbatim
     N
     |
 W-- C --E
     |
     S
 \endverbatim

 * Matrix entries for the mass balance of a 7 star system
 *
 * The entries are center, east, west, north, south, top, bottom and the 
 * right side vector b of Ax = b. This system is typically used in 3d.
 
 \verbatim
      T N
      |/
  W-- C --E
     /|
    S B
 \endverbatim
 
 * Matrix entries for the mass balance of a 9 star system
 *
 * The entries are center, east, west, north, south, north-east, south-east,
 * north-wast, south-west and the 
 * right side vector b of Ax = b. This system is typically used in 2d.
 
 \verbatim
  NW  N  NE
    \ | /
  W-- C --E
    / | \
  SW  S  SE
 \endverbatim
  */
typedef struct
{
  int type;
  int count;
  double W, E, N, S, C, T, B, NE, NW, SE, SW, V;
} N_les_row_entries;

/*!
 * \brief callback structure for 3d matrix assembling
 * */
typedef struct
{
  N_les_row_entries *(*callback) ();
} N_les_callback_3d;

/*!
 * \brief callback structure for 2d matrix assembling
 * */
typedef struct
{
  N_les_row_entries *(*callback) ();
} N_les_callback_2d;


extern void N_set_les_callback_3d_func (N_les_callback_3d * data, N_les_row_entries * (*callback_func_3d) ());
extern void N_set_les_callback_2d_func (N_les_callback_2d * data, N_les_row_entries * (*callback_func_2d) ());
extern N_les_callback_3d *N_alloc_les_callback_3d ();
extern N_les_callback_2d *N_alloc_les_callback_2d ();
extern inline N_les_row_entries *N_alloc_5star ();
extern inline N_les_row_entries *N_alloc_7star ();
extern inline N_les_row_entries *N_alloc_9star ();
extern inline N_les_row_entries *N_create_5star (double C, double W, double E, double N, double S, double V);
extern inline N_les_row_entries *N_create_7star (double C, double W, double E, double N, double S, double T, double B,
						 double V);
extern inline N_les_row_entries *N_create_9star (double C, double W, double E, double N, double S, double NW, double SW,
						 double NE, double SE, double V);
extern N_les_row_entries *N_callback_template_3d (void *data, N_geom_data * geom, int depth, int row, int col);
extern N_les_row_entries *N_callback_template_2d (void *data, N_geom_data * geom, int row, int col);
extern N_les *N_assemble_les_3d (int les_type, N_geom_data * geom, N_array_3d * status, N_array_3d * start_val,
				 void *data, N_les_callback_3d * callback);
extern N_les *N_assemble_les_2d (int les_type, N_geom_data * geom, N_array_2d * status, N_array_2d * start_val,
				 void *data, N_les_callback_2d * callback);

#endif
