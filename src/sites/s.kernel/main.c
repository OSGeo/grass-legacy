/*
****************************************************************************
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


#include "gis.h"
#include "global.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <float.h>


int main(int argc, char **argv)
{
  struct Option *opt1;
  struct Option *opt2;
  struct Option *opt3;
  /*  struct Flag *flag_d;*/

  ListSite Lsite[10000];
  char *out;
  int nfiles;
  int fdout;
  int maskfd;
  int row,col;
  int nSites;
  struct Cell_head window;
  double gaussian;
  double N,E;
  CELL  *mask;
  DCELL *output_cell;
  double sigma, dmax;
  
  double term1;
  double term2;
  double **coordinate;
  struct GModule *module;
  

  /* Initialize the GIS calls */
  G_gisinit(argv[0]);

        module = G_define_module();
	module->description =
        "Generates a raster density map from sites data using a moving 2D isotropic Gaussian kernel";

  opt1              = G_define_option();
  opt1->key         = "sitefile";
  opt1->type        = TYPE_STRING;
  opt1->required    = YES;
  opt1->multiple   = YES;
  opt1->description = "input training sites files";

  opt2              = G_define_option();
  opt2->key         = "output";
  opt2->type        = TYPE_STRING;
  opt2->required    = YES;
  opt2->gisprompt   = "new,cell,raster";
  opt2->description = "output raster map";  

  opt3 = G_define_option() ;
  opt3->key        = "stddeviation";
  opt3->type       = TYPE_DOUBLE;
  opt3->required   = NO;
  opt3->description= "stddeviation in map units" ;
  opt3->answer = "1";

  /*  flag_d              = G_define_flag();
  flag_d->key         = 'd';
  flag_d->description = "consider distances";*/


  if (G_parser(argc, argv))
    exit(1);
 
  /*read options*/
  nfiles = read_list_of_sites(opt1->answers,&Lsite);
  sscanf(opt3->answer,"%lf",&sigma);

  G_get_window(&window);
  
  fprintf(stderr,"STDDEV: %f\nRES: %f\tROWS: %d\tCOLS: %d\n",
	  sigma, window.ew_res,window.rows, window.cols);
  
  
  /* check and open the name of output map */
  out  = opt2->answer;
  
  if(G_legal_filename(out) < 0){
    G_fatal_error("illegal file name [%s]",out);
  }
 
  G_set_fp_type (DCELL_TYPE);
  if((fdout = G_open_raster_new(out,DCELL_TYPE)) < 0){
    G_fatal_error("error opening raster map [%s]", out);
  }
  /* open mask file */
   if ((maskfd = G_maskfd()) >= 0)
	mask = G_allocate_cell_buf();
    else
	mask = NULL;
  
  /*work*/
  output_cell=G_allocate_raster_buf(DCELL_TYPE);

  term1=1./(2.*M_PI*sigma*sigma);
  term2=(2.*sigma*sigma);  

  /*calcolo distanza limite uguale a 10 sigma
    dmax= invGaussian2d(sigma,1E-14); */
  dmax= sigma*10.;
  
  /*  fprintf (stderr, "distanza massima %f %f \n", dmax,DBL_EPSILON);*/

  /* leggo i file di siti */
  nSites=readSitesFiles(Lsite,nfiles,&coordinate);
  fprintf (stderr, "number of sites %d \n", nSites); 

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
      
      compute_distance(N,E,&Lsite,nfiles,term1,term2,&gaussian,dmax);
      output_cell[col] = gaussian;      
    }
    G_put_raster_row(fdout,output_cell,DCELL_TYPE);  
  }
  G_close_cell(fdout);
  exit(0);
}

int readSitesFiles(Lsite,nfiles,coordinate)
     /* read list of sites lists and 
	return number of sites */
     ListSite *Lsite;
     int nfiles;
     double ***coordinate;
{
  int l, s, jj;
  double **xySites;
  xySites=(double **)calloc(1,sizeof(double*));
  jj=0;
  xySites[jj]=(double *)calloc(2,sizeof(double));
  for(l=0; l<nfiles; l++){
    for(s=0; s<Lsite[l].nsites; s++){
      xySites[jj][0] = Lsite[l].sites[s]->east;
      xySites[jj][1] = Lsite[l].sites[s]->north;
      jj++;
      xySites=realloc(xySites,(jj+1)*sizeof(double*));
      xySites[jj]=(double *)calloc(2,sizeof(double));
    }
  }	
  *coordinate=xySites;
  return(jj);
}

void compute_distance(N,E,Lsite,nfiles,term1,term2,gaussian,dmax)
     double N,E;	
     ListSite *Lsite;
     int nfiles;
     double term1, term2;
     double *gaussian;
     double dmax;
{  
  int l,s;
  double a[2],b[2];
  double dist;

  a[0] = E;
  a[1] = N;

  *gaussian=.0;  
  for(l=0; l<nfiles; l++){
    for(s=0; s<Lsite[l].nsites; s++){
      b[0] = Lsite[l].sites[s]->east;
      b[1] = Lsite[l].sites[s]->north;

      dist = euclidean_distance(a,b,2);
      if(dist<=dmax) 
	*gaussian += gaussian2dByTerms(dist,term1,term2);    
    }
  }	
}
