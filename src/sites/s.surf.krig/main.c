/*- KRIGING SUBROUTINE
 * This program interpolates from a list of site values to a raster map.
 * The interpolation routine s.surf.idw by M. Shapiro was used as a template
 * for this interpolation routine.
 *
 * PROGRAMMER:  Chris Skelly
 *              School of Earth Sciences
 *              Macquarie Univerity
 *              North Ryde 2109 NSW Australia
 *              chris@mqatmos.cic.mq.edu.au
 *
 * Modifications:  Darrel McCauley, David D. Gray
 * Last updated: 09/2000
 */

#include<math.h>
#include<stdlib.h>
#include "gis.h"
#include "krig.h"

int search_points = 12;

mat_struct *dist_btw_smpl, *x, *sv_to_cell, *sv_btw_smpl;
mat_struct *smpl_dist, *smpl_z, *smpl_north, *smpl_east;

int npoints = 0;
int npoints_alloc = 0;
int nsearch;

Point *points = NULL;

int zindx = -1, nindx = -1, sindx = -1;

int 
main (int argc, char *argv[])
{
  int fd1, fd2, maskfd;
  DCELL *cell1, *cell2, *mask;
  struct Cell_head window;
  struct GModule *module;
  int row, col;
  double north, east;
  double maxdist, dist;
  double sum1, sum2;
  double semi_range, semi_sill, semi_nugget, max_lag;
  double semi_power;
  int i, n, max, model_num;
  char var_model[40];
  struct
  {
    struct Option *input, *zdim, *znum, *zstr, *npoints, *outz, *outvarz;
    struct Option *model, *range, *sill, *nugget, *max_lag, *power;
  } parm;

  /* The following segment is the command line input format	 */

  parm.input = G_define_option ();
  parm.input->key = "input";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->description = "Name of input site map";
  parm.input->gisprompt = "old,site_lists,sites";

  parm.zdim = G_define_option ();
  parm.zdim->key = "zdimension";
  parm.zdim->type = TYPE_STRING;
  parm.zdim->required = NO;
  parm.zdim->description = "Index of extra dimension with heights";
  parm.zdim->answer = "";

  parm.znum = G_define_option ();
  parm.znum->key = "znumeric";
  parm.znum->type = TYPE_STRING;
  parm.znum->required = NO;
  parm.znum->description = "Index of numeric field with heights";
  parm.znum->answer = "";

  parm.zstr = G_define_option ();
  parm.zstr->key = "zstring";
  parm.zstr->type = TYPE_STRING;
  parm.zstr->required = NO;
  parm.zstr->description = "Index of string field with heights";
  parm.zstr->answer = "";

  parm.outz = G_define_option ();
  parm.outz->key = "outz";
  parm.outz->type = TYPE_STRING;
  parm.outz->required = YES;
  parm.outz->description = "Name of output z value raster map";
  parm.outz->gisprompt = "any,cell,raster";

  parm.outvarz = G_define_option ();
  parm.outvarz->key = "outvarz";
  parm.outvarz->type = TYPE_STRING;
  parm.outvarz->required = YES;
  parm.outvarz->description = "Name of output z variance raster map";
  parm.outvarz->gisprompt = "any,cell,raster";

  parm.model = G_define_option ();
  parm.model->key = "model";
  parm.model->type = TYPE_STRING;
  parm.model->required = YES;
  parm.model->description = "Type of semivariogram model to be used";
  parm.model->options = "power,spherical,exp,log,gaussian";

  parm.npoints = G_define_option ();
  parm.npoints->key = "npoints";
  parm.npoints->key_desc = "count";
  parm.npoints->type = TYPE_INTEGER;
  parm.npoints->required = NO;
  parm.npoints->description = "Number of interpolation points";
  parm.npoints->answer = "12";

  parm.range = G_define_option ();
  parm.range->key = "range";
  parm.range->key_desc = "semivariogram range";
  parm.range->type = TYPE_DOUBLE;
  parm.range->required = NO;
  parm.range->description = "Not used with log or power models";
  parm.range->answer = "1.0";

  parm.power = G_define_option ();
  parm.power->key = "power";
  parm.power->key_desc = "exponetial power";
  parm.power->type = TYPE_DOUBLE;
  parm.power->required = NO;
  parm.power->description = "Only for use with the power model";
  parm.power->answer = "1.0";

  parm.nugget = G_define_option ();
  parm.nugget->key = "nugget";
  parm.nugget->key_desc = "semivariogram nugget";
  parm.nugget->type = TYPE_DOUBLE;
  parm.nugget->required = NO;
  parm.nugget->description = "The variogram nugget variance";
  parm.nugget->answer = "0.0";

  parm.sill = G_define_option ();
  parm.sill->key = "sill";
  parm.sill->key_desc = "semivariogram sill";
  parm.sill->type = TYPE_DOUBLE;
  parm.sill->required = NO;
  parm.sill->description = "The variogram sill scaled to the sample variance";
  parm.sill->answer = "1.0";

  parm.max_lag = G_define_option ();
  parm.max_lag->key = "max_lag";
  parm.max_lag->key_desc = "max_lag";
  parm.max_lag->type = TYPE_DOUBLE;
  parm.max_lag->required = NO;
  parm.max_lag->description = "The max_lag is only used with the power model";
  parm.max_lag->answer = "1000.0";

  G_gisinit (argv[0]);

  module = G_define_module();
  module->description =        
                  "Surface interpolation from site data via kriging";
                  
  /* The following segment evaluates the command line input parameters  */

  /* Main I/O files first */

  if (G_parser (argc, argv))
    exit (1);

  if (G_legal_filename (parm.outz->answer) < 0)
  {
    fprintf (stderr, "%s=%s - illegal name\n",
	     parm.outz->key, parm.outz->answer);
    exit (1);
  }

  if (G_legal_filename (parm.outvarz->answer) < 0)
  {
    fprintf (stderr, "%s=%s - illegal name\n",
	     parm.outvarz->key, parm.outvarz->answer);
    exit (1);
  }

  /* Where is the height field stored? */

  if(strcmp(parm.zdim->answer, ""))
    zindx = atoi(parm.zdim->answer);

  if(zindx < 0 && strcmp(parm.znum->answer, ""))
    nindx = atoi(parm.znum->answer);  

  if(zindx < 0 && nindx < 0 && strcmp(parm.zstr->answer, ""))
    sindx = atoi(parm.zstr->answer);  

  if(zindx < 0 && nindx < 0 && sindx < 0)
    G_fatal_error("You must specify one of options zdimension|znumeric|zstring\n");

  /* Now examine kriging parameters */

  if (sscanf (parm.sill->answer, "%f", &semi_sill) < 0)
  {
    fprintf (stderr, "%s=%s - illegal semivariogram sill\n",
	     parm.sill->key, parm.sill->answer);
    G_usage ();
    exit (1);
  }

  if (sscanf (parm.nugget->answer, "%f", &semi_nugget) < 0.0 ||
      semi_sill < semi_nugget)
  {
    fprintf (stderr, "%s=%s - illegal semivariogram nugget value\n",
	     parm.nugget->key, parm.nugget->answer);
    G_usage ();
    exit (1);
  }

  if (sscanf (parm.npoints->answer, "%d", &search_points) != 1 ||
      search_points < 1)
  {
    fprintf (stderr, "%s=%s - illegal number of interpolation points\n",
	     parm.npoints->key, parm.npoints->answer);
    G_usage ();
    exit (1);
  }

  sscanf (parm.model->answer, "%s", var_model);
  if (strcmp (var_model, "spherical") != 0 && strcmp (var_model, "log") != 0
    && strcmp (var_model, "exp") != 0 && strcmp (var_model, "power") != 0
      && strcmp (var_model, "gaussian") != 0)
  {

    fprintf (stderr, "%s=%s - not an available model\n", parm.model->key,
	     parm.model->answer);
    G_usage ();
    exit (1);
  }
  else
  {
    if (strcmp (var_model, "spherical") == 0)
      model_num = 0;
    if (strcmp (var_model, "power") == 0)
      model_num = 1;
    if (strcmp (var_model, "gaussian") == 0)
      model_num = 2;
    if (strcmp (var_model, "exp") == 0)
      model_num = 3;
    if (strcmp (var_model, "log") == 0)
      model_num = 4;
  }

  if (sscanf (parm.range->answer, "%lf", &semi_range) < 0)
  {
    fprintf (stderr, "%s=%s - illegal semivariogram range\n",
	     parm.range->key, parm.range->answer);
    G_usage ();
    exit (1);
  }
  if (sscanf (parm.max_lag->answer, "%lf", &max_lag) < 0)
  {
    fprintf (stderr, "%s=%s - illegal variogram max_lag value\n",
	     parm.max_lag->key, parm.max_lag->answer);
    G_usage ();
    exit (1);
  }

  if (sscanf (parm.power->answer, "%lf", &semi_power) < 0)
  {
    fprintf (stderr, "%s=%s - illegal semivariogram power\n",
	     parm.power->key, parm.power->answer);
    G_usage ();
    exit (1);
  }


  /* Read the data values (z) from the input site file  */

  read_sites (parm.input->answer);

  if (npoints == 0)
  {
    fprintf (stderr, "%s: no data points found\n", G_program_name ());
    exit (1);
  }
  nsearch = npoints < search_points ? npoints : search_points;

  /* dimension arrays */

  if( (sv_btw_smpl = G_matrix_init(nsearch, nsearch, nsearch)) == NULL )
    G_fatal_error("Couldn't allocate space for matrix\n");
  if( (dist_btw_smpl = G_matrix_init(nsearch, nsearch, nsearch)) == NULL )
    G_fatal_error("Couldn't allocate space for matrix\n");
  if( (sv_to_cell = G_matrix_init(nsearch, 1, nsearch)) == NULL )
    G_fatal_error("Couldn't allocate space for matrix\n");
  if( (x = G_matrix_init(nsearch + 1, 1, nsearch + 1)) == NULL )
    G_fatal_error("Couldn't allocate space for matrix\n");

  if( (smpl_dist = G_matrix_init(100, 1, 100)) == NULL )
    G_fatal_error("Couldn't allocate space for matrix\n");
  if( (smpl_z = G_matrix_init(100, 1, 100)) == NULL )
    G_fatal_error("Couldn't allocate space for matrix\n");
  if( (smpl_north = G_matrix_init(100, 1, 100)) == NULL )
    G_fatal_error("Couldn't allocate space for matrix\n");
  if( (smpl_east = G_matrix_init(100, 1, 100)) == NULL )
    G_fatal_error("Couldn't allocate space for matrix\n");



  /* Get the window, allocate buffers, etc.  */

  G_get_set_window (&window);
  cell1 = G_allocate_d_raster_buf ();
  cell2 = G_allocate_d_raster_buf ();

  if ((maskfd = G_maskfd ()) >= 0) {
    mask = G_allocate_raster_buf(CELL_TYPE);
  }
  else {
    mask = NULL;
  }

  fd1 = G_open_raster_new (parm.outz->answer, DCELL_TYPE);
  if (fd1 < 0)
  {
    fprintf (stderr, "%s: can't create %s\n", G_program_name (),
	     parm.outz->answer);
    exit (1);
  }

  fd2 = G_open_raster_new (parm.outvarz->answer, DCELL_TYPE);
  if (fd2 < 0)
  {
    fprintf (stderr, "%s: can't create %s\n", G_program_name (),
	     parm.outvarz->answer);
    exit (1);
  }

  fprintf (stderr, "Interpolating raster maps <%s & %s> ... %d rows ... ",
	   parm.outz->answer, parm.outvarz->answer, window.rows);

  /* Start the interpolation.  Set up row x row and col x col looping  */

  north = window.north + window.ns_res / 2.0;
  for (row = 0; row < window.rows; row++)
  {
    fprintf (stderr, "%-10d\b\b\b\b\b\b\b\b\b\b", window.rows - row);

    if (mask)
    {
      if (G_get_raster_row(maskfd, mask, row, CELL_TYPE) < 0)
	exit (1);
    }
    north -= window.ns_res;
    east = window.west - window.ew_res / 2.0;

    fprintf(stderr, "Processing row %d of %d\n", row + 1, window.rows);

    for (col = 0; col < window.cols; col++)
    {
      east += window.ew_res;

      if (mask && mask[col] == 0)
      {				/* don't interpolate outside of the mask */
	if( G_set_null_value(cell1, 1, DCELL_TYPE) < 0 ) {
	  fprintf(stderr, "Failed to assign raster cell value\n");
	  exit(1);  /* raster map of value z */
	}
	cell1++;

	if( G_set_null_value(cell2, 1, DCELL_TYPE) < 0 ) {
	  fprintf(stderr, "Failed to assign raster cell value\n");
	  exit(1);  /* raster map of the 
		       var. of value z */
	}
	cell2++;
      }
      else
      {
	/* Preform the Kriging interpolation  */
	dist_to_n_smpls (east, north);	/* 1st - find dist to closest n
					 * sample pts     */
	dist_btw_n_smpls (semi_range);	/* 2nd - find dist between these
					 * sample pts    */
	variogram_model (model_num, semi_range, semi_sill, semi_nugget,
			 max_lag, semi_power);
	/*
	 * 3rd - find semivariance from variogram model for the calculated
	 * distances
	 */
	krig_weights ();	/* 4th - find the kriging weights through
				 * the maniplulation of semivar. Matrices
				 * A . x = b                             */
	G_set_raster_value_d(cell1, krig_z_est(), DCELL_TYPE);
	                                /* 5th - return the kriged
					 * estimate of value z */
	G_set_raster_value_d(cell2, krig_var_est(), DCELL_TYPE);
	                                /* 6th - return the kriged
					 * estimate of the variance of
					 * value z                   */
	cell1++; cell2++;
      }
    }
    G_put_d_raster_row(fd1, cell1);  /* write row of z values to fd1         */
    G_put_d_raster_row(fd2, cell2);  /* write row of z values to fd1         */
  }
  G_close_cell (fd1);		/* close files */
  G_close_cell (fd2);

  G_matrix_free(sv_btw_smpl);
  G_matrix_free(dist_btw_smpl);
  G_matrix_free(sv_to_cell);
  G_matrix_free(x);

  G_matrix_free(smpl_dist);
  G_matrix_free(smpl_z);
  G_matrix_free(smpl_north);
  G_matrix_free(smpl_east);

  fprintf (stderr, "done          \n");
  exit (0);
}
