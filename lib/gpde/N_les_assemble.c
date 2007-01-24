
/*****************************************************************************
*
* MODULE:       Grass PDE Numerical Library
* AUTHOR(S):    Soeren Gebbert, Berlin (GER) Dec 2006
* 		soerengebbert <at> gmx <dot> de
*               
* PURPOSE:      functions to assemble a linear equation system
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

/* *************************************************************** * 
 * ********************** N_alloc_5star ************************** * 
 * *************************************************************** */
/*!
 * \brief allocate a 5 point star data structure
 *
 * \return N_les_row_entries *
 * */
inline N_les_row_entries *
N_alloc_5star ()
{
  N_les_row_entries *star = (N_les_row_entries *) G_calloc (1, sizeof (N_les_row_entries));
  star->type = N_5_POINT_STAR;
  star->count = 5;
  return star;
}

/* *************************************************************** * 
 * ********************* N_alloc_7star *************************** * 
 * *************************************************************** */
/*!
 * \brief allocate a 7 point star data structure
 *
 * \return N_les_row_entries *
 * */
inline N_les_row_entries *
N_alloc_7star ()
{
  N_les_row_entries *star = (N_les_row_entries *) G_calloc (1, sizeof (N_les_row_entries));
  star->type = N_7_POINT_STAR;
  star->count = 7;
  return star;
}

/* *************************************************************** * 
 * ********************* N_alloc_9star *************************** * 
 * *************************************************************** */
/*!
 * \brief allocate a 5 point star data structure
 *
 * \return N_les_row_entries *
 *
 * \attention The 9 point start is not yet implemented in the matrix assembling function
 *
 * */
inline N_les_row_entries *
N_alloc_9star ()
{
  N_les_row_entries *star = (N_les_row_entries *) G_calloc (1, sizeof (N_les_row_entries));
  star->type = N_9_POINT_STAR;
  star->count = 9;
  return star;
}

/* *************************************************************** * 
 * ********************** N_create_5star ************************* * 
 * *************************************************************** */
/*!
 * \brief allocate and initialize a 5 point star data structure
 *
 * \param C double
 * \param W double
 * \param E double
 * \param N double
 * \param S double
 * \param V double
 * \return N_les_row_entries *
 * */
inline N_les_row_entries *
N_create_5star (double C, double W, double E, double N, double S, double V)
{
  N_les_row_entries *star = N_alloc_5star ();

  star->C = C;
  star->W = W;
  star->E = E;
  star->N = N;
  star->S = S;

  star->V = V;

  G_debug (5, "N_create_5star:  w %g e %g n %g s %g c %g v %g\n", star->W, star->E, star->N, star->S, star->C, star->V);

  return star;
}

/* *************************************************************** * 
 * ************************* N_create_7star ********************** * 
 * *************************************************************** */
/*!
 * \brief allocate and initialize a 7 point star data structure
 *
 * \param C double
 * \param W double
 * \param E double
 * \param N double
 * \param S double
 * \param T double
 * \param B double
 * \param V double
 * \return N_les_row_entries *
 * */
inline N_les_row_entries *
N_create_7star (double C, double W, double E, double N, double S, double T, double B, double V)
{
  N_les_row_entries *star = N_alloc_7star ();

  star->C = C;
  star->W = W;
  star->E = E;
  star->N = N;
  star->S = S;

  star->T = T;
  star->B = B;

  star->V = V;

  G_debug (5, "N_create_7star:  w %g e %g n %g s %g t %g b %g c %g v %g\n",
	   star->W, star->E, star->N, star->S, star->T, star->B, star->C, star->V);

  return star;
}

/* *************************************************************** * 
 * ************************ N_create_9star *********************** * 
 * *************************************************************** */
/*!
 * \brief allocate and initialize a 9 point star data structure
 *
 * \param C  double
 * \param W  double
 * \param E  double
 * \param N  double
 * \param S  double
 * \param NW double
 * \param SW double
 * \param NE double
 * \param SE double
 * \param V  double
 * \return N_les_row_entries *
 * */
inline N_les_row_entries *
N_create_9star (double C, double W, double E, double N, double S, double NW, double SW, double NE, double SE, double V)
{
  N_les_row_entries *star = N_alloc_9star ();

  star->C = C;
  star->W = W;
  star->E = E;
  star->N = N;
  star->S = S;

  star->NW = NW;
  star->SW = SW;
  star->NE = NE;
  star->SE = SE;

  star->V = V;

  G_debug (5,
	   "N_create_9star:  w %g e %g n %g s %g nw %g sw %g ne %g se %g c %g v %g\n",
	   star->W, star->E, star->N, star->S, star->NW, star->SW, star->NE, star->SE, star->C, star->V);

  return star;
}

/* *************************************************************** * 
 * ****************** N_set_les_callback_3d_func ***************** * 
 * *************************************************************** */
/*!
 * \brief Set the callback function which is called while assembling the les in 3d
 *
 * \param data N_les_callback_3d *
 * \param callback_func_3d N_les_row_entries *
 * \return void
 * */
void
N_set_les_callback_3d_func (N_les_callback_3d * data, N_les_row_entries * (*callback_func_3d) ())
{
  data->callback = callback_func_3d;
}

/* *************************************************************** * 
 * *************** N_set_les_callback_2d_func ******************** * 
 * *************************************************************** */
/*!
 * \brief Set the callback function which is called while assembling the les in 2d
 *
 * \param data N_les_callback_2d *
 * \param callback_func_2d N_les_row_entries * 
 * \return void
 * */
void
N_set_les_callback_2d_func (N_les_callback_2d * data, N_les_row_entries * (*callback_func_2d) ())
{
  data->callback = callback_func_2d;
}

/* *************************************************************** * 
 * ************** N_alloc_les_callback_3d ************************ * 
 * *************************************************************** */
/*!
 * \brief Allocate the structure holding the callback function
 *
 * A template callback is set. Use N_set_les_callback_3d_func
 * to set up a specific function.
 *
 * \return N_les_callback_3d *
 * */
N_les_callback_3d *
N_alloc_les_callback_3d ()
{
  N_les_callback_3d *call;
  call = (N_les_callback_3d *) G_calloc (1, sizeof (N_les_callback_3d *));
  call->callback = N_callback_template_3d;

  return call;
}

/* *************************************************************** * 
 * *************** N_alloc_les_callback_2d *********************** * 
 * *************************************************************** */
/*!
 * \brief Allocate the structure holding the callback function
 *
 * A template callback is set. Use N_set_les_callback_2d_func
 * to set up a specific function.
 *
 * \return N_les_callback_2d *
 * */
N_les_callback_2d *
N_alloc_les_callback_2d ()
{
  N_les_callback_2d *call;
  call = (N_les_callback_2d *) G_calloc (1, sizeof (N_les_callback_2d *));
  call->callback = N_callback_template_2d;

  return call;
}

/* *************************************************************** * 
 * ******************** N_assemble_les_3d ************************ * 
 * *************************************************************** */
/*!
 * \brief Assemble a linear equation system (les) based on 3d location data (g3d)
 *
 * The linear equation system type can be set to N_NORMAL_LES to create a regular
 * matrix, or to N_SPARSE_LES to create a sparse matrix. This function returns
 * a new created linear equation system which can be solved with 
 * linear equation solvers. An 3d array with start values and an 3d status array
 * must be provided as well as the location geometry and a void pointer to data 
 * passed to the callback which creates the les row entries. This callback
 * have to be defined in the N_les_callback_3d structure.
 * 
 * The creation of the les is parallelized with OpenMP. 
 * If you implement new callbacks, please make sure that the 
 * function calls are thread safe.
 *
 * \param les_type int
 * \param geom      N_geom_data*
 * \param status    N_array_3d *
 * \param start_val N_array_3d *
 * \param data void *
 * \param call N_les_callback_3d *
 * \return N_les *
 * */
N_les *
N_assemble_les_3d (int les_type, N_geom_data * geom, N_array_3d * status,
		   N_array_3d * start_val, void *data, N_les_callback_3d * call)
{
  int i, j, k, count = 0, K = 0, pos = 0;
  double dcount = 0.0;
  N_array_3d *cell_count;
  N_les *les = NULL;
  int **index_ij;

  G_debug (2, "N_assemble_les_3d: assemble the linear equation system");

  cell_count = N_alloc_array_3d (geom->depths, geom->rows, geom->cols, 1, G3D_DOUBLE);


  /* First count the number of valid cells and save  
   * each number in a new 3d array. Those numbers are used 
   * to create the linear equation system.
   * */

  for (k = 0; k < geom->depths; k++)
    {
      for (i = 0; i < geom->rows; i++)
	{
	  for (j = 0; j < geom->cols; j++)
	    {
	      if (N_CELL_ACTIVE == (int) N_get_array_3d_value_double (status, k, i, j))
		{
		  dcount++;
		}
	    }
	}
    }

  if (dcount == 0.0)
    G_fatal_error
      ("Not enough active cells [%g] to create the linear equation system. Check the cell status. Only active cells (value = 1) are used to create the equation system.",
       dcount);

  /* allocate the memory for the linear equation system (les). 
   * Only valid cells are used to create the les. */
  les = N_alloc_les ((int) dcount, les_type);

  index_ij = (int **) G_calloc (dcount, sizeof (int *));
  for (i = 0; i < dcount; i++)
    index_ij[i] = (int *) G_calloc (3, sizeof (int));

  count = 0;


  /*save the k, i and j indices */
  for (k = 0; k < geom->depths; k++)
    {
      for (i = 0; i < geom->rows; i++)
	{
	  for (j = 0; j < geom->cols; j++)
	    {
	      if (N_CELL_ACTIVE == N_get_array_3d_value_double (status, k, i, j))
		{
		  N_put_array_3d_value_double (cell_count, k, i, j, count);
		  index_ij[count][0] = k;
		  index_ij[count][1] = i;
		  index_ij[count][2] = j;
		  count++;
		}
	    }
	}
    }

#pragma omp parallel for private(i, j, k, pos, K, count) schedule(static)
  for (count = 0; count < (int) dcount; count++)
    {
      k = index_ij[count][0];
      i = index_ij[count][1];
      j = index_ij[count][2];



      /*create the entries for the */
      N_les_row_entries *items = call->callback (data, geom, k, i, j);

      N_spvector *spvect = NULL;

      /*allocate a sprase vector */
      if (les_type == N_SPARSE_LES)
	spvect = N_alloc_spvector (items->count);
      /*
       * initial conditions
       */

      les->x[count] = N_get_array_3d_value_double (start_val, k, i, j);

      /*
       * the entry in the vector b
       */
      les->b[count] = items->V;

      /* pos describes the position in the sparse vector.
       * the first entry is always the diagonal entry of the matrix*/
      pos = 0;

      if (les_type == N_SPARSE_LES)
	{
	  spvect->index[pos] = count;
	  spvect->values[pos] = items->C;
	}
      else
	{
	  les->A[count][count] = items->C;
	}

      /*
       * western neighbour, entry is col - 1
       */
      if (j > 0)
	{
	  K = N_get_array_3d_value_double (cell_count, k, i, j - 1) - N_get_array_3d_value_double (cell_count, k, i, j);


	  if ((int) N_get_array_3d_value_double (status, k, i, j - 1) == N_CELL_DIRICHLET)
	    {
	      les->b[count] -= N_get_array_3d_value_double (start_val, k, i, j - 1) * items->W;
	    }
	  else if ((int) N_get_array_3d_value_double (status, k, i, j - 1) == N_CELL_ACTIVE)
	    {
	      if ((count + K) >= 0 && (count + K) < les->rows)
		{
		  pos++;
		  if (les_type == N_SPARSE_LES)
		    {
		      spvect->index[pos] = count + K;
		      spvect->values[pos] = items->W;
		    }
		  else
		    {
		      les->A[count][count + K] = items->W;
		    }
		}
	    }
	}

      /*
       * eastern neighbour, entry col + 1
       */
      if (j < geom->cols)
	{
	  K = N_get_array_3d_value_double (cell_count, k, i, j + 1) - N_get_array_3d_value_double (cell_count, k, i, j);


	  if ((int) N_get_array_3d_value_double (status, k, i, j + 1) == N_CELL_DIRICHLET)
	    {
	      les->b[count] -= N_get_array_3d_value_double (start_val, k, i, j + 1) * items->E;
	    }
	  else if ((int) N_get_array_3d_value_double (status, k, i, j + 1) == N_CELL_ACTIVE)
	    {
	      if ((count + K) >= 0 && (count + K) < les->rows)
		{
		  pos++;
		  if (les_type == N_SPARSE_LES)
		    {
		      spvect->index[pos] = count + K;
		      spvect->values[pos] = items->E;
		    }
		  else
		    {
		      les->A[count][count + K] = items->E;
		    }
		}
	    }
	}

      /*
       * northern neighbour, entry cols - number of cell in x direction
       */
      if (i > 0)
	{
	  K = N_get_array_3d_value_double (cell_count, k, i - 1, j) - N_get_array_3d_value_double (cell_count, k, i, j);


	  if ((int) N_get_array_3d_value_double (status, k, i - 1, j) == N_CELL_DIRICHLET)
	    {
	      les->b[count] -= N_get_array_3d_value_double (start_val, k, i - 1, j) * items->N;
	    }
	  else if ((int) N_get_array_3d_value_double (status, k, i - 1, j) == N_CELL_ACTIVE)
	    {
	      if ((count + K) >= 0 && (count + K) < les->rows)
		{
		  pos++;
		  if (les_type == N_SPARSE_LES)
		    {
		      spvect->index[pos] = count + K;
		      spvect->values[pos] = items->N;
		    }
		  else
		    {
		      les->A[count][count + K] = items->N;
		    }
		}
	    }
	}

      /*
       * southern entry, cols + number of cells in x - direction
       */
      if (i < geom->rows)
	{
	  K = N_get_array_3d_value_double (cell_count, k, i + 1, j) - N_get_array_3d_value_double (cell_count, k, i, j);


	  if ((int) N_get_array_3d_value_double (status, k, i + 1, j) == N_CELL_DIRICHLET)
	    {
	      les->b[count] -= N_get_array_3d_value_double (start_val, k, i + 1, j) * items->S;
	    }
	  else if ((int) N_get_array_3d_value_double (status, k, i + 1, j) == N_CELL_ACTIVE)
	    {
	      if ((count + K) >= 0 && (count + K) < les->rows)
		{
		  pos++;
		  if (les_type == N_SPARSE_LES)
		    {
		      spvect->index[pos] = count + K;
		      spvect->values[pos] = items->S;
		    }
		  else
		    {
		      les->A[count][count + K] = items->S;
		    }
		}
	    }
	}

      /*only for a 7 star entry needed */
      if (items->type == N_7_POINT_STAR)
	{
	  /*
	   * the upper cell (top) 
	   * Entry at postion - number of cells cols*rows direction
	   */
	  if (k < geom->depths)
	    {
	      K = N_get_array_3d_value_double (cell_count, k + 1,
					       i, j) - N_get_array_3d_value_double (cell_count, k, i, j);


	      if ((int) N_get_array_3d_value_double (status, k + 1, i, j) == N_CELL_DIRICHLET)
		{
		  les->b[count] -= N_get_array_3d_value_double (start_val, k + 1, i, j) * items->T;
		}
	      else if ((int) N_get_array_3d_value_double (status, k + 1, i, j) == N_CELL_ACTIVE)
		{
		  if ((count + K) >= 0 && (count + K) < les->rows)
		    {
		      pos++;
		      if (les_type == N_SPARSE_LES)
			{
			  spvect->index[pos] = count + K;
			  spvect->values[pos] = items->T;
			}
		      else
			{
			  les->A[count][count + K] = items->T;
			}
		    }
		}
	    }

	  /*
	   * the lower cell (bottom)
	   * Entry at position + number of cells cols*rows direction
	   */
	  if (k > 0)
	    {
	      K = N_get_array_3d_value_double (cell_count, k - 1,
					       i, j) - N_get_array_3d_value_double (cell_count, k, i, j);


	      if ((int) N_get_array_3d_value_double (status, k - 1, i, j) == N_CELL_DIRICHLET)
		{
		  les->b[count] -= N_get_array_3d_value_double (start_val, k - 1, i, j) * items->B;
		}
	      else if ((int) N_get_array_3d_value_double (status, k - 1, i, j) == N_CELL_ACTIVE)
		{
		  if ((count + K) >= 0 && (count + K) < les->rows)
		    {
		      pos++;
		      if (les_type == N_SPARSE_LES)
			{
			  spvect->index[pos] = count + K;
			  spvect->values[pos] = items->B;
			}
		      else
			{
			  les->A[count][count + K] = items->B;
			}
		    }
		}
	    }
	}

      /*Wie viele Eintrage gibt es */
      if (les->type == N_SPARSE_LES)
	{
	  spvect->cols = pos + 1;
	  N_add_spvector_to_les (les, spvect, count);
	}

      if (items)
	G_free (items);

    }

  N_free_array_3d (cell_count);

  for (i = 0; i < dcount; i++)
    G_free (index_ij[i]);

  G_free (index_ij);


  return les;
}



/* *************************************************************** * 
 * ******************** N_assemble_les_2d ************************ * 
 * *************************************************************** */
/*!
 * \brief Assemble a linear equation system (les) based on 2d location data  (raster)
 *
 * 
 * The linear equation system type can be set to N_NORMAL_LES to create a regular
 * matrix, or to N_SPARSE_LES to create a sparse matrix. This function returns
 * a new created linear equation system which can be solved with 
 * linear equation solvers. An 2d array with start values and an 2d status array
 * must be provided as well as the location geometry and a void pointer to data 
 * passed to the callback which creates the les row entries. This callback
 * have to be defined in the N_les_callback_2d structure.
 * must be defined in the N_les_callback_2d strcuture.
 *
 * The creation of the les is parallelized with OpenMP. 
 * If you implement new callbacks, please make sure that the 
 * function calls are thread safe.
 *
 * \param les_type int
 * \param geom      N_geom_data*
 * \param status    N_array_2d *
 * \param start_val N_array_2d *
 * \param data void *
 * \param call N_les_callback_2d *
 * \return N_les *
 * */

N_les *
N_assemble_les_2d (int les_type, N_geom_data * geom, N_array_2d * status,
		   N_array_2d * start_val, void *data, N_les_callback_2d * call)
{
  int i, j, count = 0, K = 0, pos = 0;
  double dcount = 0.0;
  int **index_ij;
  N_array_2d *cell_count;
  N_les *les = NULL;

  G_debug (2, "N_assemble_les_2d: assemble the linear equation system");

  cell_count = N_alloc_array_2d (geom->rows, geom->cols, 1, DCELL_TYPE);


  /* At first count the number of valid cells and save the 
   * each number in a new 3d array. Those numbers are used 
   * to create the linear equation system.
   * */
  for (i = 0; i < geom->rows; i++)
    {
      for (j = 0; j < geom->cols; j++)
	{
	  if (N_CELL_ACTIVE == N_get_array_2d_value_dcell (status, i, j))
	    {
	      dcount++;
	    }
	}
    }

  if (dcount == 0.0)
    G_fatal_error
      ("Not enough active cells [%g] to create the linear equation system. Check the cell status. Only active cells (value = 1) are used to create the equation system.",
       dcount);


  /* Then allocate the memory for the linear equation system (les). 
   * Only valid cells are used to create the les. */

  index_ij = (int **) G_calloc (dcount, sizeof (int *));
  for (i = 0; i < dcount; i++)
    index_ij[i] = (int *) G_calloc (2, sizeof (int));

  les = N_alloc_les ((int) dcount, les_type);

  count = 0;

  for (i = 0; i < geom->rows; i++)
    {
      for (j = 0; j < geom->cols; j++)
	{
	  if (N_CELL_ACTIVE == (int) N_get_array_2d_value_dcell (status, i, j))
	    {
	      N_put_array_2d_value_dcell (cell_count, i, j, (double) count);
	      index_ij[count][0] = i;
	      index_ij[count][1] = j;
	      count++;
	    }
	}
    }

/* Assemble the matrix in parallel*/
#pragma omp parallel for private(i, j, pos, K, count) schedule(static)
  for (count = 0; count < (int) dcount; count++)
    {
      i = index_ij[count][0];
      j = index_ij[count][1];

      /*create the entries for the */
      N_les_row_entries *items = call->callback (data, geom, i, j);

      N_spvector *spvect = NULL;

      /*allocate a sprase vector */
      if (les_type == N_SPARSE_LES)
	{
	  spvect = N_alloc_spvector (items->count);
	}
      /*
       * initial conditions
       */
      les->x[count] = N_get_array_2d_value_dcell (start_val, i, j);

      /*
       * the entry in the vector b
       */
      les->b[count] = items->V;

      /* pos describes the position in the sparse vector.
       * the first entry is always the diagonal entry of the matrix*/
      pos = 0;

      if (les_type == N_SPARSE_LES)
	{
	  spvect->index[pos] = count;
	  spvect->values[pos] = items->C;
	}
      else
	{
	  les->A[count][count] = items->C;
	}

      /*
       * western neighbour, entry is col - 1
       */
      if (j > 0)
	{
	  K = N_get_array_2d_value_dcell (cell_count, i, j - 1) - N_get_array_2d_value_dcell (cell_count, i, j);


	  if (N_get_array_2d_value_dcell (status, i, j - 1) == N_CELL_DIRICHLET)
	    {
	      les->b[count] -= N_get_array_2d_value_dcell (start_val, i, j - 1) * items->W;
	    }
	  else if (N_get_array_2d_value_dcell (status, i, j - 1) == N_CELL_ACTIVE)
	    {
	      if ((count + K) >= 0 && (count + K) < les->rows)
		{

		  pos++;
		  if (les_type == N_SPARSE_LES)
		    {
		      spvect->index[pos] = count + K;
		      spvect->values[pos] = items->W;
		    }
		  else
		    {
		      les->A[count][count + K] = items->W;
		    }
		}
	    }
	}

      /*
       * eastern neighbour, entry col + 1
       */
      if (j < geom->cols)
	{
	  K = N_get_array_2d_value_dcell (cell_count, i, j + 1) - N_get_array_2d_value_dcell (cell_count, i, j);


	  if (N_get_array_2d_value_dcell (status, i, j + 1) == N_CELL_DIRICHLET)
	    {
	      les->b[count] -= N_get_array_2d_value_dcell (start_val, i, j + 1) * items->E;
	    }
	  else if (N_get_array_2d_value_dcell (status, i, j + 1) == N_CELL_ACTIVE)
	    {
	      if ((count + K) >= 0 && (count + K) < les->rows)
		{
		  pos++;
		  if (les_type == N_SPARSE_LES)
		    {
		      spvect->index[pos] = count + K;
		      spvect->values[pos] = items->E;
		    }
		  else
		    {
		      les->A[count][count + K] = items->E;
		    }
		}
	    }
	}

      /*
       * northern neighbour, entry cols - number of cell in x direction
       */
      if (i > 0)
	{
	  K = N_get_array_2d_value_dcell (cell_count, i - 1, j) - N_get_array_2d_value_dcell (cell_count, i, j);


	  if (N_get_array_2d_value_dcell (status, i - 1, j) == N_CELL_DIRICHLET)
	    {
	      les->b[count] -= N_get_array_2d_value_dcell (start_val, i - 1, j) * items->N;
	    }
	  else if (N_get_array_2d_value_dcell (status, i - 1, j) == N_CELL_ACTIVE)
	    {
	      if ((count + K) >= 0 && (count + K) < les->rows)
		{
		  pos++;
		  if (les_type == N_SPARSE_LES)
		    {
		      spvect->index[pos] = count + K;
		      spvect->values[pos] = items->N;
		    }
		  else
		    {
		      les->A[count][count + K] = items->N;
		    }
		}
	    }
	}

      /*
       * southern entry, cols + number of cells in x - direction
       */
      if (i < geom->rows)
	{
	  K = N_get_array_2d_value_dcell (cell_count, i + 1, j) - N_get_array_2d_value_dcell (cell_count, i, j);


	  if (N_get_array_2d_value_dcell (status, i + 1, j) == N_CELL_DIRICHLET)
	    {
	      les->b[count] -= N_get_array_2d_value_dcell (start_val, i + 1, j) * items->S;
	    }
	  else if (N_get_array_2d_value_dcell (status, i + 1, j) == N_CELL_ACTIVE)
	    {
	      if ((count + K) >= 0 && (count + K) < les->rows)
		{
		  pos++;
		  if (les_type == N_SPARSE_LES)
		    {
		      spvect->index[pos] = count + K;
		      spvect->values[pos] = items->S;
		    }
		  else
		    {
		      les->A[count][count + K] = items->S;
		    }
		}
	    }
	}

      /*Wie viele Eintrage gibt es */
      if (les->type == N_SPARSE_LES)
	{
	  spvect->cols = pos + 1;
	  N_add_spvector_to_les (les, spvect, count);
	}

      if (items)
	G_free (items);
    }

  /*release memory */
  N_free_array_2d (cell_count);

  for (i = 0; i < dcount; i++)
    G_free (index_ij[i]);

  G_free (index_ij);

  return les;
}


/* *************************************************************** * 
 * ******************** N_callback_template_3d ******************* * 
 * *************************************************************** */
/*!
 * \brief A callback template creates a 7 point star structure
 *
 * This is a template callback for mass balance calculation with 7 point stars
 * based on 3d data (g3d).
 *
 * \param data void *
 * \param geom N_geom_data *
 * \param depth int
 * \param row   int
 * \param col   int
 * \return N_les_row_entries *
 *
 * */
N_les_row_entries *
N_callback_template_3d (void *data, N_geom_data * geom, int depth, int row, int col)
{
  N_les_row_entries *star = N_alloc_7star ();

  star->E = 1 * geom->dx;
  star->W = 1 * geom->dx;
  star->N = 1 * geom->dy;
  star->S = 1 * geom->dy;
  star->T = 1 * geom->dz;
  star->B = 1 * geom->dz;
  star->C = -4 * geom->dx * geom->dy * geom->dz;
  star->V = -1;

  G_debug (5,
	   "N_callback_template_3d:  w %g e %g n %g s %g t %g b %g c %g v %g\n",
	   star->W, star->E, star->N, star->S, star->T, star->B, star->C, star->V);


  return star;
}

/* *************************************************************** * 
 * ********************* N_callback_template_2d ****************** * 
 * *************************************************************** */
/*!
 * \brief A callback template creates a 5 point star structure
 *
 * This is a template callback for mass balance calculation with 5 point stars
 * based on 2d data (raster).
 *
 * \param data void *
 * \param geom N_geom_data *
 * \param row int
 * \param col int
 * \return N_les_row_entries *
 *
 * */
N_les_row_entries *
N_callback_template_2d (void *data, N_geom_data * geom, int row, int col)
{
  N_les_row_entries *star = N_alloc_5star ();

  star->E = 1 * geom->dx;
  star->W = 1 * geom->dx;
  star->N = 1 * geom->dy;
  star->S = 1 * geom->dy;
  star->C = -4 * geom->dx * geom->dy;
  star->V = -1;

  G_debug (5, "N_callback_template_2d:  w %g e %g n %g s %g c %g v %g\n",
	   star->W, star->E, star->N, star->S, star->C, star->V);

  return star;
}
