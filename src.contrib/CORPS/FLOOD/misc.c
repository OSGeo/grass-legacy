/*******************************************************************************
                    Floodplain Analysis Toolkit
               Mother Earth Systems, Boulder, Colorado


This software was been developed for the U.S. Army Corps of Engineers,
Omaha District under contract #DACW45-92-P-1301.

This code is in the public domain.  Permission to use, copy, modify, and
distribute this software and its documentation for any purpose and without
fee is granted.

*******************************************************************************/

#include <stdio.h>
#include <math.h>
#include "gis.h"
#include "voronoi.h"
#include "rsv.h"

/*==========================================================================*/
init_graphics( pwindow )
   C_HEAD *pwindow;
{
   /*-------------------------------*/
   /* Initialization for digitizing */
   /*-------------------------------*/
   R_open_driver();
   D_setup(0);
   G_get_set_window( pwindow );
   G_setup_plot ( D_get_d_north(), D_get_d_south(),
                  D_get_d_west(),  D_get_d_east(),
                  D_move_abs,      D_cont_abs );
}

/*==========================================================================*/
int readsites( input_rid )
   int input_rid;
{
   int nsites;
   int row, col;
   int nrows, ncols;
   CELL *row_buffer;
   double east, north;
   extern struct Cell_head wind;
   extern float **vsite;
   extern int    *elev;

   /*-----------------------------------*/
   /* allocate memory for voronoi sites */
   /*-----------------------------------*/
   vsite = (float **) G_malloc (MAXSITES * sizeof (float *));
   if (vsite == NULL)
      G_fatal_error ("Insufficent memory for voronoi points");

   for (nsites = 0; nsites < MAXSITES; nsites++)
   {
      vsite[nsites] = (float *) G_malloc (2 * sizeof (float));
      if (vsite[nsites] == NULL)
         G_fatal_error ("Insufficent memory for voronoi points");
   }

   elev = (int *) G_malloc (MAXSITES * sizeof (int));
   if (elev == NULL)
      G_fatal_error ("Insufficent memory for voronoi elevations");

   /*------------------------------------------*/
   /* get voronoi sites from cross section map */
   /*------------------------------------------*/
   nsites = 0;
   nrows  = wind.rows;
   ncols  = wind.cols;
   row_buffer = G_allocate_cell_buf();
 
   for( row=0; row < nrows; row++ )
   {
      G_get_map_row( input_rid, row_buffer, row );
      for ( col=0; col < ncols; col++ )
      {
         if ( row_buffer[col] )
         {
            /*---------------------------------------*/
            /* check for max number of voronoi sites */
            /*---------------------------------------*/
            if ( nsites == MAXSITES )
               G_fatal_error( "Maximum number of voronoi sites exceeded" );

            /*------------------*/
            /* add voronoi site */
            /*------------------*/
            cell_to_coord( &wind, row, col, &north, &east );
            elev[nsites]       = row_buffer[col];
            vsite[nsites][0]   = (float)east;
            vsite[nsites++][1] = (float)north;
         }
      }
   }

   return ( nsites );
}

/*==========================================================================*/
cell_to_coord( window, row, col, north, east )
   C_HEAD *window;
   int row;
   int col;
   double *north;
   double *east;
{
   *north = window->north - row * window->ns_res;
   *east  = window->west  + col * window->ew_res;
}

/*==========================================================================*/
