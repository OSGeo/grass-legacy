/*======================================================================
   read_elev.c
======================================================================*/

#include "global.h"

static CELL read_raster_cell(int, int, int);

/*======================================================================
			     read_elev

Return elevation value at easting,northing.  
  Return -1 on error. 
  Returns 0 if requested (east,north) is outside of the elevation file.

Need to switch to the target environment and reset the current region to
read the elevation layer.
======================================================================*/
CELL read_elev(elevation, easting, northing)
int    *elevation;
double  easting, northing;
{
Auxillary_Photo  *auxil;

   CELL cell= -1;
   struct Cell_head old_region, new_region;
/**   GrassEnv old_env; **/
   int row, col;
   double G_easting_to_col(), G_northing_to_row();


   /* TODO -- only works for PHOTO and LANDSAT */
   if ((group.trans_type < PHOTO) ||
       (group.trans_type > LAND_TM)) {
     /** TODO - message **/
     elevation = 0;
     return (-1);
   }

   /* make visiable */
   auxil = (Auxillary_Photo *) group.auxil;
   
   G_get_set_window(&old_region);
/**   old_env = new_environment(TargetEnv); **/
   select_target_env();

   G_get_cellhd(auxil->elev.elev_map, auxil->elev.elev_mapset, &new_region);
   G_set_window(&new_region);

   if( auxil->elev.fd == 0) {
      auxil->elev.fd = G_open_cell_old(auxil->elev.elev_map, 
				 auxil->elev.elev_mapset);
      if(auxil->elev.fd == 0) goto end;
   }

   row = G_northing_to_row(northing, &new_region);
   if ((row < new_region.south) || (row > new_region.north)) {
     cell = 0;
     goto end;
   }

   col = G_easting_to_col(easting, &new_region);
   if ((col < new_region.west) || (col > new_region.east)) {
     cell = 0;
     goto end;
   }

   cell = read_raster_cell(auxil->elev.fd, row, col);


end:

   /* close the elevation file */
   G_close_cell (auxil->elev.fd);
   auxil->elev.fd = '\0';

   /** new_environment(old_env); **/
   select_current_env();
   G_set_window(&old_region);
   *elevation = (int) cell;

   return(1);
}



/*======================================================================
			   read_raster_cell

fd is an open file descriptor for a raster map.  Return cell value at
location row,col in that map.  Return -1 on error.
======================================================================*/
static CELL read_raster_cell(int fd, int row, int col)
{
   CELL *cell, c;
   cell = G_allocate_cell_buf();
   if(cell == NULL) return((CELL)-1);

   G_suppress_warnings(1);
   if (G_get_map_row(fd, cell, row)) {
     c = cell[col];
   }
   else c = 0;    
   G_suppress_warnings(1);
     
   G_free(cell);
   return(c);
}








