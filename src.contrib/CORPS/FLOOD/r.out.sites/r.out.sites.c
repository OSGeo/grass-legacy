#include "stdio.h"
#include "gis.h"
#include "Vect.h"

main (argc, argv)
	int argc ;
	char *argv[] ;
{
   int  rid;
   int row, col, nrows, ncols;
   double north, east;
   struct Cell_head window;
   CELL  *cell;
   struct Option *old;
   char *mapset;
   char errmsg[200], buf1[100], buf2[100];

   /*---------*/
   /* parsing */
   /*---------*/
   G_gisinit(argv[0]);

   old              = G_define_option();
   old->key         = "input";
   old->type        = TYPE_STRING;
   old->required    = YES;
   old->multiple    = NO;
   old->gisprompt   = "old,cell,raster" ;
   old->description = "raster map to be converted to sites";

   if (G_parser (argc, argv))
   exit(-1);

   /*--------------------------------*/
   /* find and open input raster map */
   /*--------------------------------*/
   if ((mapset = G_find_cell (old->answer, "")) == NULL)
   {
      sprintf (errmsg, "Could not find raster map <%s>\n", old->answer);
      G_fatal_error (errmsg);
   }

   if ( (rid = G_open_cell_old( old->answer, mapset) ) < 0 )
   {
      sprintf(errmsg, "Could not open raster map <%s>\n", old->answer);
      G_fatal_error (errmsg);
   }

   /*--------------------*/
   /* Get map resolution */
   /*--------------------*/
   G_get_window (&window);
   nrows = window.rows;
   ncols = window.cols;

   /*------------------------*/
   /* Convert cells to sites */
   /*------------------------*/
   cell = G_allocate_cell_buf();
   for( row = 0; row < nrows; row++ )
   {
      G_get_map_row ( rid, cell, row );
      for ( col = 0; col < ncols; col++ )
      {
         if ( cell[col] )
         {
            north = G_row_to_northing( row, window );
            east  = G_col_to_easting( col, window );
            G_format_northing( north, buf1, -1);
            G_format_easting( east, buf2, -1);
            printf(" %-12s|%-12s|#%d\n", buf2, buf1, cell[col]);
         }
      }
   }

   /*-----------*/
   /* finish up */
   /*-----------*/
   close( rid );
   exit(0);
}
