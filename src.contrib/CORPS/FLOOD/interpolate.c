/*******************************************************************************                         r.surf.voronoi
               Mother Earth Systems, Boulder, Colorado
 
 
This software was been developed for the U.S. Army Corps of Engineers,
Omaha District under contract #DACW45-92-P-1301. 
 
This code is in the public domain.  Permission to use, copy, modify, and 
distribute this software and its documentation for any purpose and without 
fee is granted. 
 
*******************************************************************************/

#include "G.h"
#include "rsv.h"

#define AUTO_MASKING G__.auto_mask
#define MASK_FD      G__.mask_fd
#define MASK_BUF     G__.mask_buf

interpolate( nsites )
   int nsites;
{
   extern int     input_rid;
   extern int     output_rid;
   extern int    *elev;
   extern float **vsite;
   extern double *area;
   extern C_HEAD  wind;

   int    i, nadj;
   int    nverts;
   int    adj_id, adj_list[256];
   int    line_color, erase_color;
   int    row, col;
   float  pverts[256][2];
   double orig_vol, mod_vol, stolen_vol;
   double new_area, adj_area;
   double east, north;
   CELL  *input_buffer;
   CELL  *output_buffer;
   CELL  *mask_buffer;

   /*-----------------*/
   /* initializations */
   /*-----------------*/
   line_color    = D_translate_color( "yellow" );
   erase_color   = D_translate_color( "black" );
   input_buffer  = G_allocate_cell_buf();
   output_buffer = G_allocate_cell_buf();
   mask_buffer   = MASK_BUF;

   for( row = 0; row < wind.rows; row++ )
   {
      G_get_map_row( input_rid, input_buffer, row );
      if ( AUTO_MASKING )
         G_get_map_row_nomask (MASK_FD, mask_buffer, row);

      for ( col = 0; col < wind.cols; col++ )
      {
         /*---------------------------*/
         /* see if cell is masked out */
         /*---------------------------*/
         if ( AUTO_MASKING  &&  mask_buffer[col] == 0 )
         {
            /*------------------------*/
            /* move on to next column */
            /*------------------------*/
            output_buffer[col] = 0;
            continue;
         }

         if ( input_buffer[col] )
            /*-----------------------------*/
            /* elevation is known, keep it */
            /*-----------------------------*/
            output_buffer[col] = input_buffer[col];
         else
         {
            /*-------------------------------*/
            /* add cell as new voronoi point */
            /*-------------------------------*/
            cell_to_coord( &wind, row, col, &north, &east );
            vsite[nsites][0] = (float)east;
            vsite[nsites][1] = (float)north;
            load_vsites( nsites+1, vsite, (float)wind.west, (float)wind.south,
                                          (float)wind.east, (float)wind.north);

            /*------------------------*/
            /* get new voronoi region */
            /*------------------------*/
            nverts = find_vregion( nsites, pverts );
            if ( nverts > MAX_PVERTS )
              G_fatal_error( "Maximum number of vertices exceeded." );

            polygon_area( pverts, nverts, &new_area );

            /*---------------------------------*/
            /* find adjacent sites to new cell */
            /*---------------------------------*/
            nadj = find_adjacent( nsites, adj_list );

#ifdef DEBUG
            /* plot new voronoi region */
            plot_polygon( "white", pverts, nverts );

            /* plot adjacent vectors */
            for ( i=0; i < nadj; i++ )
            {
               R_standard_color( line_color );
               G_plot_line( vsite[nsites][0], vsite[nsites][1],
                            vsite[adj_list[i]][0],  vsite[adj_list[i]][1] );
               R_flush();
            }

            /* erase adjacent vectors */
            printf("continue?");
            scanf("%*c");
            R_standard_color( erase_color );
            for ( i=0; i < nadj; i++ )
            {
               G_plot_line( vsite[nsites][0], vsite[nsites][1],
                            vsite[adj_list[i]][0],  vsite[adj_list[i]][1] );
               R_flush();
            }
            plot_polygon( "black", pverts, nverts );
#endif

            /*----------------------------------------------------*/
            /* process adjacent sites for elevation interpolation */
            /*----------------------------------------------------*/
            stolen_vol = 0.0;
            for ( i=0; i < nadj; i++ )
            {
               /*----------------------------------------*/
               /* get original volume of adjacent region */
               /*----------------------------------------*/
               adj_id = adj_list[i];
               orig_vol = area[adj_id] * (double)(elev[adj_id]);

               /*----------------------------------------*/
               /* get modified volume of adjacent region */
               /*----------------------------------------*/
               nverts = find_vregion( adj_id, pverts );
               if ( nverts > MAX_PVERTS )
                 G_fatal_error( "Maximum number of vertices exceeded." );
               polygon_area( pverts, nverts, &adj_area );
#ifdef DEBUG
               plot_polygon( "green", pverts, nverts );
#endif
               mod_vol = adj_area * (double)(elev[adj_id]);

               /*-------------------------*/
               /* cumulate stolen volumes */
               /*-------------------------*/
               stolen_vol += (orig_vol - mod_vol);
            }

            /*-----------------------------*/
            /* estimate elevation for cell */
            /*-----------------------------*/
            output_buffer[col] = (CELL)(stolen_vol / new_area);
         }
      }

      /*------------------------*/
      /* write interpolated row */
      /*------------------------*/
      G_percent( row, wind.rows, 1 );
      G_put_map_row( output_rid, output_buffer );
   }
   G_percent( 1, 1, 1 );
}
