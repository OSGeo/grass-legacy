/*************************************************************
* name
*    r.in.dem
*
* function
*    convert DEM to GRASS raster
*
* Author:
*    Eric Buddington (ebuddington@wesleyan.edu)
*    November 1995
*
* This code is PUBLIC DOMAIN. Modification and redistribution
*    is encouraged.
***************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "bintree.h"
#include "gis.h"
#include "dem.h"
#include "conv.h"
#include "main.h"

struct flags flag;
#define VERBOSE flag.v->answer

int main(const int argc, char *argv[]){
  unsigned int i;
  int isok, cell_fd;
  struct dem_head dem_head;
  struct dem_data *record[2], *trans;
  struct dem_accuracy acc;
  struct Cell_head cell_head;
  struct parameter {struct Option *input, *output, *spheroid, *res, *size;} parm; 
  extern struct flags flag;
  FILE *dem_ascii;

  record[0] = (struct dem_data *)G_malloc(sizeof(**record));
  record[1] = (struct dem_data *)G_malloc(sizeof(**record));

/********* Get and parse command-line arguments ***************************/
  G_gisinit(argv[0]);

  parm.input = G_define_option();
  parm.input->key = "input";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->description = "DEM file to be imported";

  parm.output = G_define_option();
  parm.output->key = "output";
  parm.output->type = TYPE_STRING;
  parm.output->required = YES;
  parm.output->description = "Name for new raster map";

  parm.spheroid = G_define_option();
  parm.spheroid->key = "spheroid";
  parm.spheroid->type = TYPE_STRING;
  parm.spheroid->required = NO;
  parm.spheroid->description = "Spheroid used for lat/lon (if applicable)";

  parm.res = G_define_option();
  parm.res->key = "resolution";
  parm.res->type = TYPE_DOUBLE;
  parm.res->required = NO;
  parm.res->multiple = YES;
  parm.res->description = "Output resolution as ew_res, ns_res";

  parm.size = G_define_option();
  parm.size->key = "size";
  parm.size->type = TYPE_INTEGER;
  parm.size->required = NO;
  parm.size->multiple = YES;
  parm.size->description = "Number of cells as rows, cols";

  flag.v = G_define_flag();
  flag.v->key = 'v';
  flag.v->description = "Verbose Output";

  if (G_parser(argc, argv)) exit(1);
  if (parm.spheroid->answer == NULL) parm.spheroid->answer = "wgs84"; 

/********* Convert DEM Header *********************************/
  if (!(dem_ascii = fopen(parm.input->answer, "r"))){
    fprintf(stderr, "Cannot open file %s for input", parm.input->answer);
    exit(-2);
  }
  read_dem_head(&dem_head, dem_ascii);

  if (dem_head.proj == PROJ_LL){
    if (VERBOSE) fprintf(stdout, "DEM data is in Lat/Lon\n");
    if (!(isok = CC_u2ll_spheroid(parm.spheroid->answer))){
      (isok == -1) ? 
	fprintf(stderr, "Unknown spheroid %s", parm.spheroid->answer) : 
	  fprintf(stderr, "CC_u2ll_spheroid(%s) internal error.", 
		  parm.spheroid->answer);
      exit (-1);
    }

    if (G_projection() == PROJECTION_UTM){
      if (VERBOSE) fprintf(stdout, "Cell data is in UTM\n");
      conv_head_ll2u(&dem_head, &cell_head);
    }
  }
/******** Set active region to new raster area and open output *********/
  G_set_window(&cell_head);
  if ((cell_fd = G_open_cell_new_random(parm.output->answer)) < 0) exit (-2);
  set_queue_fd(cell_fd); /* Set fd for the output buffer queue */

/******** Convert DEM data *********************************************/

  if (dem_head.proj == PROJ_LL){
    if (G_projection() == PROJECTION_UTM){
      if (VERBOSE) fprintf(stdout, "Converting DEM rows\n");
      read_dem_record(record[0], 1, 1, dem_ascii); /* preload first row */
      for (i=2; i <= dem_head.cols; i++){      
	if (VERBOSE) fprintf(stdout, "Read row %i of %i\n", i, dem_head.cols);
	read_dem_record(record[1], 1, i, dem_ascii);
	conv_row_ll2u(&dem_head, record, &cell_head);
	trans = record[0];
	record[0] = record[1];
	record[1] = trans;
	free(record[1]->data);
      }
      dump_ptree();                                  /* dump the data tree */
      cache((cell_row_t)-5, (cell_row_t)0, (cell_data_t)0);  /* and flush the cache */
    }
    else fprintf(stderr, "Unsupported GRASS projection (not UTM)");
  }
  else fprintf(stderr, "Unsupported DEM projection (not UTM)");

/******** Clean up ****************************************************/
  G_close_cell(cell_fd);
  return 1;
}








