/****************************************************************************
*
* MODULE:       s.kernel
*
* AUTHOR(S):    Stefano Menegon, ITC-irst, Trento, Italy
* PURPOSE:      Generates a raster density map from sites data using a moving
*               2D isotropic Gaussian kernel
* COPYRIGHT:    (C) 2002 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include "gis.h"
#include "Vect.h"
#include "global.h"


int main(int argc, char **argv)
{
  struct Option *in_opt, *net_opt, *out_opt;
  struct Option *stddev_opt, *dsize_opt, *segmax_opt, *netmax_opt, *multip_opt;
  struct Flag *flag_o, *flag_v, *flag_q;
  int    net = 0;

  char   *mapset;
  struct Map_info In, Net, Out;
  int    fdout, maskfd;
  int    row,col;
  struct Cell_head window;
  double gaussian;
  double N,E;
  CELL  *mask;
  DCELL *output_cell;
  double sigma, dmax, segmax, netmax, multip;
  
  double term1;
  double term2;
  double **coordinate;
  double sigmaOptimal;
  double predict;
  struct GModule *module;
  double dsize;
  int verbose = 1 ;

  double gausmax = 0;

  /* Initialize the GIS calls */
  G_gisinit(argv[0]);

  module = G_define_module();
  module->description = 
      "Generates a raster density map from sites data using a moving 2D isotropic Gaussian kernel. "
      "The density map maybe calculated on vector network (options 'net=' and 'voutput')";

  in_opt = G_define_standard_option(G_OPT_V_INPUT);
  in_opt->description = "Input vector with training points.";

  net_opt = G_define_standard_option(G_OPT_V_INPUT);
  net_opt->key         = "net";
  net_opt->description = "Input network vector.";
  net_opt->required    = NO;

  out_opt              = G_define_option();
  out_opt->key         = "output";
  out_opt->type        = TYPE_STRING;
  out_opt->required    = YES;
  out_opt->description = "output raster/vector map";  

  stddev_opt              = G_define_option() ;
  stddev_opt->key         = "stddeviation";
  stddev_opt->type        = TYPE_DOUBLE;
  stddev_opt->required    = YES;
  stddev_opt->description = "stddeviation in map units";

  dsize_opt              = G_define_option() ;
  dsize_opt->key         = "dsize";
  dsize_opt->type        = TYPE_DOUBLE;
  dsize_opt->required    = NO;
  dsize_opt->description = "discretization error in map units" ;
  dsize_opt->answer      = "0.";

  segmax_opt              = G_define_option() ;
  segmax_opt->key         = "segmax";
  segmax_opt->type        = TYPE_DOUBLE;
  segmax_opt->required    = NO;
  segmax_opt->description = "maximum length of segment on network" ;
  segmax_opt->answer      = "100.";

  netmax_opt              = G_define_option() ;
  netmax_opt->key         = "distmax";
  netmax_opt->type        = TYPE_DOUBLE;
  netmax_opt->required    = NO;
  netmax_opt->description = "maximum distance from point to network" ;
  netmax_opt->answer      = "10.";

  multip_opt              = G_define_option() ;
  multip_opt->key         = "multip";
  multip_opt->type        = TYPE_DOUBLE;
  multip_opt->required    = NO;
  multip_opt->description = "multiply the result by this number" ;
  multip_opt->answer      = "1.";

  flag_o              = G_define_flag();
  flag_o->key         = 'o';
  flag_o->description = "Try to calculate optimal standard deviation. 'stddeviation' is taken as maximum "
                        "value. standard deviation is calculated using ALL points, not only those in "
			"the current region";

  flag_q              = G_define_flag();
  flag_q->key         = 'q';
  flag_q->description = "Calculate optimal standard deviation and exit (no map is written).";

  flag_v = G_define_flag();
  flag_v->key = 'v';
  flag_v->description = "Run verbosely";

  if (G_parser(argc, argv))
    exit(1);

  /*read options*/
  sigma = atof(stddev_opt->answer);
  dsize = atof(dsize_opt->answer);
  segmax = atof(segmax_opt->answer);
  netmax = atof(netmax_opt->answer);
  multip = atof(multip_opt->answer);
  verbose = flag_v->answer;

  G_get_window(&window);
  
  fprintf(stderr,"STDDEV: %f\nRES: %f\tROWS: %d\tCOLS: %d\n",
	  sigma, window.ew_res, window.rows, window.cols);

  /* Open input vector */
  if ((mapset = G_find_vector2 (in_opt->answer, "")) == NULL)
      G_fatal_error ( "Could not find input map '%s'\n", in_opt->answer);

  Vect_set_open_level (2);
  Vect_open_old (&In, in_opt->answer, mapset);
  
  if ( net_opt->answer ) {
      net = 1;

      /* Open input network */
      if ((mapset = G_find_vector2 (net_opt->answer, "")) == NULL)
	  G_fatal_error ( "Could not find network input map '%s'\n", net_opt->answer);

      Vect_set_open_level (2);
      Vect_open_old (&Net, net_opt->answer, mapset);
      Vect_net_build_graph ( &Net, GV_LINES, 0, 0, NULL, NULL, NULL, 0, 0 );

      if( !flag_q->answer ) {
	  Vect_open_new (&Out, out_opt->answer, 0);
	  Vect_hist_command ( &Out );
      }
  } else {
      /* check and open the name of output map */
      if( !flag_q->answer ) {
	  if(G_legal_filename( out_opt->answer ) < 0)
	    G_fatal_error("illegal file name [%s]", out_opt->answer);
	 
	  G_set_fp_type (DCELL_TYPE);
	  if((fdout = G_open_raster_new(out_opt->answer,DCELL_TYPE)) < 0)
	    G_fatal_error("error opening raster map [%s]", out_opt->answer);

	  /* open mask file */
	  if ((maskfd = G_maskfd()) >= 0)
	    mask = G_allocate_cell_buf();
	  else
	    mask = NULL;

	  /* allocate output raster */
	  output_cell=G_allocate_raster_buf(DCELL_TYPE);
      }
  }

  /* valutazione distanza ottimale */ 
  if ( flag_o->answer ) {
    double *dists; /* array of all distances < dmax */
    int ndists;    /* number of distances in dists */
    int    npoints;

    /* Note: sigmaOptimal calculates using ALL points (also those outside the region) */ 

    fprintf (stderr, "Automatic choose of smoothing parameter (standard deviation), maximum possible "
	             "value of standard deviation is was set to %f\n", sigma);     

    /* maximum distance 4*sigma (3.9*sigma ~ 1.0000), keep it small, otherwise it takes 
     * too much points and calculation on network becomes slow */
    dmax = 4*sigma; /* used as maximum value */

    fprintf (stderr, "Using maximum distance between points: %f\n", dmax);     

    if ( net_opt->answer ) {
	npoints = Vect_get_num_primitives(&In,GV_POINTS);
        /* Warning: each distance is registered twice (both directions) */     
        ndists = compute_all_net_distances(&In,&Net,netmax,&dists,dmax);
    } else {
	/* Read points */
	npoints = read_points ( &In, &coordinate, dsize );
        ndists = compute_all_distances(coordinate,&dists,npoints,dmax);
    }
    
    fprintf (stderr, "Number of input points: %d \n", npoints); 
    fprintf (stderr, "%d distances read from the map\n", ndists);     

    /* define score function L(window size) */
    double L(double smooth) 
    {
      int ii;
      double resL,n,sigmaConv;

      n = npoints;
      resL = 0.;
      sigmaConv = sqrt(2.0);

      for(ii=0; ii < ndists; ii++) 
	  resL+= gaussian2dBySigma(dists[ii]/smooth,sigmaConv) - 2. * gaussian2dBySigma(dists[ii]/smooth,1.);

      if ( !net_opt->answer ) resL *= 2.; 

      resL = (1/pow(n*smooth,2.)) * (resL + n*( gaussian2dBySigma(0.,sigmaConv) -2. * gaussian2dBySigma(0.,1.)) ) + (2/(n*pow(smooth,2.)))*gaussian2dBySigma(0.,1.);

      G_debug(3, "smooth = %e resL = %e", smooth, resL);

      if(verbose){
	fprintf (stderr, "\tScore Value=%f\tsmoothing parameter (standard deviation)=%f \n",resL, smooth);   
      }

      return(resL);
    }

    /* sigma is used in brent as maximum possible value for sigmaOptimal */
    predict= (double) brent( 0.0, sigma/2, sigma, L, 1.0e-6, &sigmaOptimal);
    fprintf (stderr, "Optimal smoothing parameter (standard deviation): %f\n", sigmaOptimal);     

    /* Reset sigma to calculated optimal value */
    sigma=sigmaOptimal;
    
    if( flag_q->answer ) {
        Vect_close (&In);
        if ( net_opt->answer )
	    Vect_close (&Net);

        exit (0);
    }
  }

  term1=1./(2.*M_PI*sigma*sigma);
  term2=(2.*sigma*sigma);  
  
  dmax= sigma*4.;

  if ( net ) {
      int line, nlines;
      struct line_pnts *Points, *SPoints;
      struct line_cats *SCats;
      
      /* Divide lines to segments and calculate gaussian for center of each segment */
      
      Points = Vect_new_line_struct ();
      SPoints = Vect_new_line_struct ();
      SCats = Vect_new_cats_struct ();
      
      nlines = Vect_get_num_lines(&Net);
      G_debug (3, "net nlines = %d", nlines);

      for ( line = 1; line <= nlines; line++){
	  int    seg, nseg, ltype;
	  double length, x, y;
	  
	  ltype = Vect_read_line (&Net, Points, NULL, line);
	  if ( !(ltype & GV_LINES ) ) continue;
	  
	  length = Vect_line_length (Points);
	  nseg = (int) (1 + length / segmax);
	  length = length / nseg;

	  G_debug (3, "net line = %d, nseg = %d, seg length = %f", line, nseg, length);

	  for ( seg = 0; seg < nseg; seg++ ) {
	      double offset1, offset2;

	      offset1 = (seg + 0.5) * length; 
	      Vect_point_on_line ( Points, offset1, &x, &y, NULL, NULL, NULL);

	      G_debug (3, "  segment = %d, offset = %f, xy = %f %f", seg, offset1, x, y);

	      compute_net_distance ( x, y, &In, &Net, netmax, term1, term2, &gaussian, dmax );
	      gaussian *= multip;
	      if ( gaussian > gausmax ) gausmax = gaussian;

	      G_debug (3, "  gaussian = %f", gaussian);

	      /* Write segment */
	      if ( gaussian > 0 ) {
		  offset1 = seg * length;
		  offset2 = (seg + 1) * length;
		  Vect_line_segment ( Points, offset1, offset2, SPoints ); 

		  Vect_reset_cats ( SCats );
		  Vect_cat_set ( SCats, 1, (int) gaussian );

		  Vect_write_line ( &Out, GV_LINE, SPoints, SCats );
	      }
	  }
      }	

      Vect_close (&Net);

      Vect_build (&Out, stderr);
      Vect_close (&Out);
  } else { 
      fprintf (stderr, "\nWriting output raster file using smooth parameter=%f\n",sigma);
      for(row=0; row<window.rows; row++){
	G_percent(row,window.rows,2);
	if (mask)
	  {
	    if(G_get_map_row(maskfd, mask, row) < 0)
	      G_fatal_error("error reading MASK");
	  }
	
	for(col=0; col<window.cols; col++) {
	  /* don't interpolate outside of the mask */
	  if (mask && mask[col] == 0)
	    {
	      G_set_d_null_value(&output_cell[col], 1);
	      continue;
	    }     

	  N = G_row_to_northing(row+0.5,&window);
	  E = G_col_to_easting(col+0.5,&window);
	  
	  compute_distance ( N, E, &In, term1, term2, &gaussian,dmax );
	  output_cell[col] = multip * gaussian;      
	  if ( gaussian > gausmax ) gausmax = gaussian;
	}
	G_put_raster_row(fdout,output_cell,DCELL_TYPE);  
      }
      
      G_close_cell(fdout);
  }

  fprintf (stderr, "Maximum value in output: %f\n", gausmax);

  Vect_close (&In);

  exit(0);
}

/* Read points to array return number of points */
int read_points( struct Map_info *In, double ***coordinate, double dsize)
{
  int    line, nlines, npoints, ltype, i = 0;
  double **xySites;
  static struct line_pnts *Points = NULL;
  
  if (!Points)
      Points = Vect_new_line_struct ();
  
  /* Allocate array of pointers */
  npoints = Vect_get_num_primitives(In,GV_POINT);
  xySites = (double **) calloc ( npoints, sizeof(double*) );
  
  nlines = Vect_get_num_lines(In);

  for ( line = 1; line <= nlines; line++){
      ltype = Vect_read_line (In, Points, NULL, line);
      if ( !(ltype & GV_POINT ) ) continue;
      
      xySites[i] = (double *) calloc (2,sizeof(double));
      
      xySites[i][0] = Points->x[0];
      xySites[i][1] = Points->y[0]; 
      i++;
  }	

  *coordinate = xySites;

  return (npoints);
}

/* Calculate distances < dmax between all sites in coordinate 
 * Return: number of distances in dists */
double compute_all_distances(double **coordinate, double **dists, int n, double dmax)
{
  int ii,jj,kk;
  int nn;

  nn = n*(n-1)/2;
  *dists = (double *)calloc(nn,sizeof(double));  
  kk=0;

  for(ii=0; ii < n-1; ii++){
    for(jj=ii+1; jj<n; jj++){
      double dist;

      dist = euclidean_distance(coordinate[ii],coordinate[jj],2);
      G_debug (3, "dist = %f", dist);

      if ( dist <= dmax ) {
          (*dists)[kk] = dist;
	  kk++;
      }
    }
  }

  return (kk);
}

/* Calculate distances < dmax between all sites in coordinate 
 * Return: number of distances in dists */
double compute_all_net_distances( struct Map_info *In, struct Map_info *Net, 
	                          double netmax, double **dists, double dmax)
{
  int   nn, kk, nalines, aline;
  double dist;
  struct line_pnts *APoints, *BPoints;
  BOUND_BOX box;
  struct ilist *List;

  APoints = Vect_new_line_struct ();
  BPoints = Vect_new_line_struct ();
  List = Vect_new_list ();
  
  nn = Vect_get_num_primitives(In,GV_POINTS);
  nn = nn*(nn-1);
  *dists = (double *)calloc(nn,sizeof(double));  
  kk=0;

  nalines = Vect_get_num_lines(In);
  for ( aline = 1; aline <= nalines; aline++){
      int   i, altype;
      
      G_debug (3, "  aline = %d", aline);

      altype = Vect_read_line (In, APoints, NULL, aline);
      if ( !(altype & GV_POINTS ) ) continue;

      box.E = APoints->x[0] + dmax; box.W = APoints->x[0] - dmax;
      box.N = APoints->y[0] + dmax; box.S = APoints->y[0] - dmax;
      box.T = PORT_DOUBLE_MAX; box.B = -PORT_DOUBLE_MAX;

      Vect_select_lines_by_box ( In, &box, GV_POINT, List);
      G_debug (3, "  %d points in box", List->n_values);

      for ( i = 0; i < List->n_values; i++){
	  int bline, ret;

	  bline = List->value[i];

	  if ( bline == aline ) continue;
	  
          G_debug (3, "    bline = %d", bline);
	  Vect_read_line (In, BPoints, NULL, bline);

	  ret = Vect_net_shortest_path_coor ( Net, APoints->x[0], APoints->y[0], 0.0, 
		                              BPoints->x[0], BPoints->y[0], 0.0, 
					      netmax, netmax, &dist, NULL, NULL, NULL );

          G_debug (3, "  SP: %f %f -> %f %f", APoints->x[0], APoints->y[0], BPoints->x[0], BPoints->y[0]);
		  
	  if ( ret == 0 ) {
	      G_debug (0, "not reachable");
	      continue; /* Not reachable */
	  } 

	  G_debug (3, "  dist = %f", dist);

	  if ( dist <= dmax ) {
	      (*dists)[kk] = dist;
	      kk++;
	  }
	  G_debug (3, "  kk = %d", kk);
      }
  }

  return (kk);
}

/* Compute gausian for x, y along Net, using all points in In */
void compute_net_distance( double x, double y, struct Map_info *In, struct Map_info *Net, double netmax, 
	               double term1, double term2, double *gaussian, double dmax)
{  
  int    i;
  double dist;
  static struct line_pnts *Points = NULL;
  BOUND_BOX box;
  static struct ilist *List = NULL;
  
  if (!Points)
      Points = Vect_new_line_struct ();

  if (!List)
      List = Vect_new_list ();

  *gaussian=.0;  

  
  /* The network is usually much bigger than dmax and to calculate shortest path is slow
   * -> use spatial index to select points */ 
  box.E = x + dmax; box.W = x - dmax;
  box.N = y + dmax; box.S = y - dmax;
  box.T = PORT_DOUBLE_MAX; box.B = -PORT_DOUBLE_MAX;

  Vect_select_lines_by_box ( In, &box, GV_POINT, List);
  G_debug (3, "  %d points in box", List->n_values);

  for ( i = 0; i < List->n_values; i++){
      int line, ret;

      line = List->value[i];
      Vect_read_line (In, Points, NULL, line);

      G_debug (3, "  SP: %f %f -> %f %f", x, y, Points->x[0], Points->y[0]);
      ret = Vect_net_shortest_path_coor ( Net, x, y, 0.0, Points->x[0], Points->y[0], 0.0, 
	                                  netmax, netmax, &dist, NULL, NULL, NULL );
	      
      if ( ret == 0 ) {
	  G_debug (0, "not reachable");
	  continue; /* Not reachable */
      } 

      G_debug (3, "  dist = %f", dist);

      if(dist<=dmax)
	*gaussian += gaussian2dByTerms(dist,term1,term2);    
  }

}

void compute_distance( double N, double E, struct Map_info *In, 
	               double term1, double term2, double *gaussian, double dmax)
{  
  int    line, nlines, ltype;
  double a[2],b[2];
  double dist;
  static struct line_pnts *Points = NULL;
  
  if (!Points)
      Points = Vect_new_line_struct ();

  a[0] = E;
  a[1] = N;

  *gaussian=.0;  
  
  nlines = Vect_get_num_lines(In);

  /* TODO ? : use spatial index */
  for ( line = 1; line <= nlines; line++){
      ltype = Vect_read_line (In, Points, NULL, line);
      if ( !(ltype & GV_POINT ) ) continue;

      b[0] = Points->x[0];
      b[1] = Points->y[0];

      dist = euclidean_distance(a,b,2);
      
      if(dist<=dmax) 
	*gaussian += gaussian2dByTerms(dist,term1,term2);    

  }

}

