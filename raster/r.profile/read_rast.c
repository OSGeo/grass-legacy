/*
* $Id$
*
* Copyright (C) 2000 by the GRASS Development Team
* Author: Bob Covill <bcovill@tekmap.ns.ca>
* 
* This Program is free software under the GPL (>=v2)
* Read the file COPYING coming with GRASS for details
*
*
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "display.h"

int read_rast 
( double east, 
double north, 
double dist, 
int fd, 
int coords, 
RASTER_MAP_TYPE data_type,
FILE *fp )
{ 
    int row, col, nrows, ncols;
    struct Cell_head window;
    CELL *cell;
    char buf[1024]="";
    FCELL *fcell;
    DCELL *dcell;
    float val_f;

G_get_window(&window);
nrows = window.rows;
ncols = window.cols;

row = (window.north - north) / window.ns_res ;
col = (east - window.west) / window.ew_res ;

if (row < 0 )
 G_fatal_error("Coordinate request outsite current region settings");

        if (data_type == CELL_TYPE) {
         cell = G_allocate_c_raster_buf();
         if (G_get_c_raster_row(fd, cell, row) < 0)
           exit(1);
         
         if(G_is_c_null_value(&cell[col]) )
           sprintf(buf, "Null");
         else
           sprintf(buf, "%d", cell[col]);
         
         if (coords ==1)
          fprintf(fp, "%f %f %f %s\n", east, north, dist, buf);
         else
          fprintf(fp, "%f %s\n", dist, buf);
        }

	if (data_type == FCELL_TYPE) {
	 fcell = G_allocate_f_raster_buf();
	 if (G_get_f_raster_row(fd, fcell, row) < 0)
	   exit(1);
         
         if(G_is_f_null_value(&fcell[col]) )
           sprintf(buf, "Null");
         else
           sprintf(buf, "%f", fcell[col]);
         
	 if (coords ==1)
	   fprintf(fp, "%f %f %f %s\n", east, north, dist, buf);
	 else
	   fprintf(fp, "%f %s\n", dist, buf);
	}
	
        if (data_type == DCELL_TYPE) {
         dcell = G_allocate_d_raster_buf();
         if (G_get_d_raster_row(fd, dcell, row) < 0)
           exit(1);
         if(G_is_d_null_value(&dcell[col]) )
           sprintf(buf, "Null");
         else
           sprintf(buf, "%f", dcell[col]);
         
         if (coords ==1)
           fprintf(fp, "%f %f %f %s\n", east, north, dist, buf);
         else
           fprintf(fp, "%f %s\n", dist, buf);
        }
} 
