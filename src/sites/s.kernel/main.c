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


int main(int argc, char **argv)
{
  struct Option *opt1;
  struct Option *opt2;
  struct Option *opt3;
  struct Option *opt4;
  /*  struct Flag *flag_d;*/

  ListSite Lsite[10000];
  char *out;
  double ray;
  int nfiles;
  int fdout;
  char buf[500];
  int r,c;
  struct Cell_head cellhd;
  struct Cell_head cellhd_orig;
  double gaussian;
  double N,E;
  DCELL *output_cell;
  double sigma;
  
  double term1;
  double term2;
  
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

  opt4 = G_define_option() ;
  opt4->key        = "resolution";
  opt4->type       = TYPE_DOUBLE;
  opt4->required   = NO;
  opt4->description= "resolution of the output raster map" ;
  opt4->answer = "25.0";


  /*  flag_d              = G_define_flag();
  flag_d->key         = 'd';
  flag_d->description = "consider distances";*/


  if (G_parser(argc, argv))
    exit(1);
 
  /*read sites*/
  nfiles = read_list_of_sites(opt1->answers,&Lsite);

  /*set ray and working resolution according to area*/
  sscanf(opt3->answer,"%lf",&sigma);
  ray = sigma;

  G_get_window(&cellhd_orig);

  cellhd = cellhd_orig;
  sscanf(opt4->answer,"%lf",&(cellhd.ns_res));
  cellhd.ew_res = cellhd.ns_res;
  cellhd.rows = (int) (cellhd.north - cellhd.south) / cellhd.ns_res;
  cellhd.cols = (int) (cellhd.east - cellhd.west) / cellhd.ew_res;
  
  G_set_window(&cellhd);
  
  fprintf(stderr,"\n\nSTDDEV: %f\n\nRES: %f\tROWS: %d\tCOLS: %d\n\n",
	  sigma, cellhd.ew_res,cellhd.rows, cellhd.cols);
  
  
  /* check and open the name of output map */
  out  = opt2->answer;
  
  if(G_legal_filename(out) < 0){
    sprintf(buf,"illegal file name [%s]",out);
    G_fatal_error(buf);
  }

  if((fdout = G_open_fp_cell_new(out)) < 0){
    sprintf(buf,"error opening raster map [%s]", out);
    G_fatal_error(buf);
  }
  
  /*work*/
  output_cell=G_allocate_d_raster_buf();
  term1=1./(sqrt(2.*PIG)*ray);
  term2=(2.*ray*ray);  
  for(r=0; r<cellhd.rows; r++){
    for(c=0; c<cellhd.cols; c++) {
      N = G_row_to_northing(r+0.5,&cellhd);
      E = G_col_to_easting(c+0.5,&cellhd);
      
      compute_distance(N,E,&Lsite,nfiles,term1,term2,&gaussian);
      /*      if(!flag_d->answer)
	output_cell[c] = (double) npoints;
	else*/
	output_cell[c] = gaussian;
      
    }

    G_put_d_raster_row(fdout,output_cell);
    G_percent(r,cellhd.rows,10);
    
  }

  G_close_cell(fdout);
  
  exit(0);

}


void compute_distance(N,E,Lsite,nfiles,term1,term2,gaussian)
     double N,E;	
     ListSite *Lsite;
     int nfiles;
     double term1;
     double term2;
     /*     int *npoints;
	    double *meandist;*/
     double *gaussian;
{
  
  int l,s;
  double a[2],b[2];
  double dist;

  a[0] = E;
  a[1] = N;

  /*  *npoints = 0;
   *meandist = .0;*/
  *gaussian=.0;  
  for(l=0; l<nfiles; l++){
    for(s=0; s<Lsite[l].nsites; s++){
      b[0] = Lsite[l].sites[s]->east;
      b[1] = Lsite[l].sites[s]->north;

      /*      fprintf(stderr,"%s\n",Lsite[l].sites[s]->str_att[1]);*/
      
      dist = euclidean_distance(a,b,2);
      *gaussian +=term1*exp(-dist*dist/term2);    
    /*      if(dist < ray){
	*npoints += 1;
	*meandist += dist;
	}*/
    }
  }	
}
