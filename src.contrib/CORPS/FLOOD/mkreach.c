/*******************************************************************************
                    Floodplain Analysis Toolkit
               Mother Earth Systems, Boulder, Colorado


This software was been developed for the U.S. Army Corps of Engineers,
Ft. Worth District under contract #DACW63-91-M-1085 and for the Omaha District 
under contract #DACW45-92-P-1301.

This code is in the public domain.  Permission to use, copy, modify, and
distribute this software and its documentation for any purpose and without
fee is granted.

Mother Earth Systems disclaims all warranties with regard to this software,
including all implied warranties of merchantability and fitness. In no event
shall Mother Earth Systems be liable for any special, indirect or consequential
damages or any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this software.

*******************************************************************************/

#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "flood.h"

/*==========================================================================*/
char *make_reach( xsect_ptr )
   M_INFO *xsect_ptr;
{
   int     xsect_known;
   char   *mapset;
   char    xsect_fname[FNAMELEN];
   M_INFO  temp_vid;
   M_INFO *known_ptr;
   M_INFO  reach_vid;
   FILE   *reach_aid;

   /*------------------------------*/
   /* Check for existing reach map */
   /*------------------------------*/
   mapset = G_find_vector( reach_mconv, "" );
   if ( mapset )
      return( mapset ); 

   /*------------------------------------------------------*/
   /* See if we nee to identify and open cross section map */
   /*------------------------------------------------------*/
   if ( xsect_ptr )
   {
      xsect_known = TRUE;
      known_ptr   = xsect_ptr;
   }
   else
   {
      xsect_known = FALSE;

      /*----------------------------*/
      /* Get cross section map name */
      /*----------------------------*/
      mapset = get_xsect_name( xsect_fname );
      if ( !mapset )
      {   
          printf ( "    ERROR: cross section map '%s' not found.\n",
                        xsect_fname );
          return(0);
      }

      /*--------------------------------*/
      /* See what the sorting option is */
      /*--------------------------------*/
      sort = ask_sort();

      /*----------------------------------------------------*/
      /* Open cross section map, with sorting, if necessary */
      /*----------------------------------------------------*/
      if ( !get_xsect_map( xsect_fname, mapset, &temp_vid ) )
             return(0);
      known_ptr = &temp_vid;
   }

   /*---------------------------*/
   /* Find and open reach files */
   /*---------------------------*/
   printf("    Building reach map...\n");
   if ( !reach_input( known_ptr, &reach_vid, &reach_aid ) )
      return(NULL);
 
   /*--------------------*/
   /* Generate reach map */
   /*--------------------*/
   if ( !set_reach( known_ptr, &reach_vid, reach_aid ) )
      return(NULL);
      
   /*----------*/
   /* clean up */
   /*----------*/
   if ( !xsect_known )
      Vect_close( known_ptr );
   return( f_mapset );
}

/*==========================================================================*/
reach_input( xsect_ptr, reach_vid, reach_aid )
   M_INFO *xsect_ptr;
   M_INFO *reach_vid;
   FILE  **reach_aid;
{
   /*--------------------*/
   /* Open new reach map */
   /*--------------------*/
   if ( ( Vect_open_new( reach_vid, reach_mconv ) ) < 0 )
       return(0);
 
   /*-----------------------------*/
   /* Set up header for reach map */
   /*-----------------------------*/
   Vect_copy_head_data( &(xsect_ptr->head), &(reach_vid->head) );
   strcpy (reach_vid->head.map_name, "Output from mkreach");
   strcpy (reach_vid->head.your_name, G_whoami ());
 
   /*------------------------------------*/
   /* Open new reach attribute (id) file */
   /*------------------------------------*/
   if ((*reach_aid = G_fopen_new( "dig_att", reach_mconv )) == NULL)
      return(0);
 
   return(1);
}
/*==========================================================================*/
set_reach( xsect_ptr, reach_vid, reach_aid )
   M_INFO *xsect_ptr;
   M_INFO *reach_vid;
   FILE   *reach_aid;
{
   int     rc;
   int     intersect;
   int     itype;
   int     total_points;
   int     reach_id;
   char    buffer[BUFFLEN];
   double  d1, d2;
   double  x_right[2];
   double  y_right[2];
   double  x_left[2];
   double  y_left[2];
   double  x_rcross[2];
   double  y_rcross[2];
   double  x_lcross[2];
   double  y_lcross[2];
   double  int_x;
   double  int_y;
   L_PNTS *us_points;
   L_PNTS *ds_points;
   L_PNTS *rev_ds_points;
   L_PNTS *right_points;
   L_PNTS *left_points;
   L_PNTS *reach_points;
 
   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   reach_id      = 0;
   us_points     = Vect_new_line_struct();
   ds_points     = Vect_new_line_struct();
   rev_ds_points = Vect_new_line_struct();
   right_points  = Vect_new_line_struct();
   left_points   = Vect_new_line_struct();
   reach_points  = Vect_new_line_struct();
 
   /*-----------------------------------------*/
   /* Setup points for upstream cross section */
   /*-----------------------------------------*/
   Vect_rewind( xsect_ptr );
   itype = Vect_read_next_line( xsect_ptr, us_points );
   if ( itype <= 0 )
      return(0);
 
   /*-------------------------------------------*/
   /* Setup points for downstream cross section */
   /*-------------------------------------------*/
   itype = Vect_read_next_line( xsect_ptr, ds_points );
   if ( itype <= 0 )
      return(0);
 
   /*---------------------------------------------*/
   /* For subsequent downstream cross sections... */
   /*---------------------------------------------*/
   while ( itype > 0 )
   {
      /*------------------------------*/
      /* Setup points for reach sides */
      /*------------------------------*/
      set_sides( x_right, y_right, x_left, y_left,
                 us_points, ds_points );
 
      /*------------------------------*/
      /* Check for intersecting sides */
      /*------------------------------*/
      intersect = dig_find_intersection( x_right[0], y_right[0],
                                         x_right[1], y_right[1],
                                         x_left[0],  y_left[0],
                                         x_left[1],  y_left[1],
                                         &int_x,     &int_y );
      if ( intersect == TRUE )
      {
         /*--------------------*/
         /* Flip things around */
         /*--------------------*/
         reverse( rev_ds_points, ds_points );
         Vect_reset_line( ds_points );
         append_points( ds_points, rev_ds_points );
         set_sides( x_right, y_right, x_left, y_left,
                    us_points, ds_points );
      }
      else if ( intersect == FALSE )
      {
         /*------------------------------------------*/
         /* See if the two cross sections EVER cross */
         /*------------------------------------------*/
         reverse( rev_ds_points, ds_points );
         set_sides( x_rcross, y_rcross, x_lcross, y_lcross,
                    us_points, rev_ds_points );
         intersect = dig_find_intersection( x_rcross[0], y_rcross[0],
                                            x_rcross[1], y_rcross[1],
                                            x_lcross[0], y_lcross[0],
                                            x_lcross[1], y_lcross[1],
                                            &int_x,     &int_y );
         if ( intersect == FALSE )
         {
            /*-------------------------------------------------*/
            /* No intersection, use minimum distance criterion */
            /*-------------------------------------------------*/
            d1 = distance( x_rcross[0], y_rcross[0],
                           x_rcross[1], y_rcross[1] )
               + distance( x_lcross[0], y_lcross[0],
                           x_rcross[1], y_lcross[1] );
            d2 = distance( x_right[0], y_right[0],
                           x_right[1], y_right[1] )
               + distance( x_left[0], y_left[0],
                           x_left[1], y_left[1] );
            if ( d1 < d2 )
            {
               /*--------------------*/
               /* Flip things around */
               /*--------------------*/
               reverse( rev_ds_points, ds_points );
               Vect_reset_line( ds_points );
               append_points( ds_points, rev_ds_points );
               set_sides( x_right, y_right, x_left, y_left,
                          us_points, ds_points );
            }
         }
      }
 
      /*-------------------------------------*/
      /* Save the sides in a point structure */
      /*-------------------------------------*/
      Vect_copy_xy_to_pnts( right_points, x_right, y_right, 2 );
      Vect_copy_xy_to_pnts( left_points, x_left, y_left, 2 );
 
      /*--------------------------------------------------*/
      /* Connect the cross sections to form reach polygon */
      /*--------------------------------------------------*/
      Vect_reset_line( reach_points );
      append_points( reach_points, us_points );
      append_points( reach_points, right_points );
      append_points( reach_points, ds_points );
      append_points( reach_points, left_points );

      /*-------------------------------------------*/
      /* Write the reach polygon to the output map */
      /*-------------------------------------------*/
      if ( Vect_write_line( reach_vid, AREA, reach_points ) < 0 )
         return(0);
 
      /*-----------------------------------*/
      /* Find a reach identification point */
      /*-----------------------------------*/
      reach_id++;
      get_interior_pt( reach_points, reach_id, &int_x, &int_y );
          
      /*-----------------------------------------------*/
      /* Assign an attribute to the newly formed reach */
      /*-----------------------------------------------*/
      sprintf( buffer, "A %f %f %d", int_x, int_y, reach_id );
      fprintf( reach_aid, "%s\n", buffer );
 
      /*-------------------------------------------*/
      /* Set downstream xsect to new upstream xsect*/
      /*-------------------------------------------*/
      Vect_reset_line( us_points );
      append_points( us_points, ds_points );
       
      /*---------------------------*/
      /* Get next downstream xsect */
      /*---------------------------*/
      itype = Vect_read_next_line( xsect_ptr, ds_points );
   }
 
   /*----------*/
   /* Clean up */
   /*----------*/
   fclose( reach_aid );
   Vect_close( reach_vid );
   support_vector( reach_mconv );
   Vect_rewind( xsect_ptr );
 
   Vect_destroy_line_struct( us_points );
   Vect_destroy_line_struct( ds_points );
   Vect_destroy_line_struct( right_points );
   Vect_destroy_line_struct( left_points );
   Vect_destroy_line_struct( reach_points );
 
   /*---------------------*/
   /* Rasterize reach map */
   /*---------------------*/
   sprintf( buffer, "v.to.rast input=%s output=%s>> %s",
                     reach_mconv, reach_mconv, log_fname );
   system( buffer );
 
   /*---------------*/
   /* Check for EOF */
   /*---------------*/
   if ( itype != -2 )
      return (0);
   else
      return(1);
}      
 
/*==========================================================================*/
set_sides( x_right, y_right, x_left, y_left, us_points, ds_points )
   double *x_right;
   double *y_right;
   double *x_left;
   double *y_left;
   L_PNTS *us_points;
   L_PNTS *ds_points;
{
   int     last_us;
   int     last_ds;
    
   last_us = us_points->n_points - 1;
   last_ds = ds_points->n_points - 1;
 
   x_right[0] = us_points->x[last_us];
   y_right[0] = us_points->y[last_us];
 
   x_right[1] = ds_points->x[0];
   y_right[1] = ds_points->y[0];
 
   x_left[0]  = ds_points->x[last_ds];
   x_left[1]  = us_points->x[0];
 
   y_left[0]  = ds_points->y[last_ds];
   y_left[1]  = us_points->y[0];
}
    
/*==========================================================================*/
reverse( dest_line, src_line )
   L_PNTS *dest_line;
   L_PNTS *src_line;
{
   int i;
   int npoints;
 
   npoints = src_line->n_points;
   Vect_reset_line( dest_line );
 
   for ( i=npoints-1;   i >= 0;   i-- )
      Vect_append_point( dest_line, src_line->x[i], src_line->y[i] );
}
 
/*==========================================================================*/
append_points( dest, src )
   L_PNTS *dest;
   L_PNTS *src;
{
   int i, last;

   for ( i=0; i < src->n_points;  i++ )
   {
      /*---------------------------*/
      /* check for duplicate point */
      /*---------------------------*/
      last = dest->n_points-1;
      if ( last > 0  &&  dest->x[last] == src->x[i]  &&  dest->y[last] == src->y[i] )
         ;
      else
         Vect_append_point( dest, src->x[i], src->y[i] );
   }
}
 
/*==========================================================================*/
get_interior_pt( poly, reach_id, int_x, int_y )
   L_PNTS *poly;
   int     reach_id;
   double *int_x;
   double *int_y;
{
   /*-----------------------------------------------*/
   /* this function assumes that there are at least */
   /* three points in the polygon and that          */
   /* west coords are smaller than east coords.     */
   /*-----------------------------------------------*/

   int i, t1, t2, t3, last;
   double west;

   /*----------------------*/
   /* find west most point */
   /*----------------------*/
   for ( i=0; i < poly->n_points;  i++ )
   {
      if ( i == 0 )
      {
         west = poly->x[0];
         t1 = 0;
      }
      else if ( west > poly->x[i] )
      {
         west = poly->x[i];
         t1 = i;
      }
   }

   /*--------------------------------*/
   /* find previous point in polygon */
   /*--------------------------------*/
   last = poly->n_points - 1;
   if ( t1 )
      t2 = t1 - 1;
   else
      t2 = last - 1;

   /*----------------------------*/
   /* find next point in polygon */
   /*----------------------------*/
   if ( t1 == last )
      t3 = 1;
   else
      t3 = t1 + 1;

   /*------------------------------------*/
   /* calculate centroid of the triangle */
   /*------------------------------------*/
   *int_x = ( poly->x[t1] + poly->x[t2] + poly->x[t3] ) / 3.0;
   *int_y = ( poly->y[t1] + poly->y[t2] + poly->y[t3] ) / 3.0;


   /*--------------------------*/
   /* plot reach, if requested */
   /*--------------------------*/
#ifdef DEBUG
   printf("    Reach #%d...\n", reach_id);
   if ( sort == USER )
   {
      int line_color;
      char triangle;
      int ii;

      line_color = D_translate_color( "green" );
      R_standard_color( line_color );
      for(ii=0; ii<poly->n_points-1;ii++)
      {
         G_plot_line( poly->x[ii], poly->y[ii],
                      poly->x[ii+1], poly->y[ii+1] );
         R_flush();
      }

      printf("triangle?");
      scanf("%c", &triangle);
      fflush(stdin);

      if ( triangle == 'y' )
      {
         line_color = D_translate_color( "magenta" );
         R_standard_color( line_color );
         G_plot_line( poly->x[t1], poly->y[t1], poly->x[t2], poly->y[t2] );
         G_plot_line( poly->x[t2], poly->y[t2], poly->x[t3], poly->y[t3] );
         G_plot_line( poly->x[t3], poly->y[t3], poly->x[t1], poly->y[t1] );
         R_flush();
   
         put_box( *int_x, *int_y );
      }
      printf("continue?");
      scanf("%*c");
      fflush(stdin);
   }
#endif

}

/*==========================================================================*/
put_box( x_point, y_point )
   double  x_point;
   double  y_point;
{
   int     color;
   C_HEAD window;
   double x1, y1;
   double x2, y2;
   double x3, y3;
   double x4, y4;
   double delta;

   G_get_set_window( &window );
   color = D_translate_color( "blue" );
   R_standard_color( color );
   delta =  0.50 * dig_unit_conversion() * (window.north - window.south);
   x1 = x_point-delta;
   y1 = y_point+delta;
   x2 = x_point+delta;
   y2 = y_point+delta;
   x3 = x_point+delta;
   y3 = y_point-delta;
   x4 = x_point-delta;
   y4 = y_point-delta;
 
   G_plot_line( x1, y1, x2, y2 );
   G_plot_line( x2, y2, x3, y3 );
   G_plot_line( x3, y3, x4, y4 );
   G_plot_line( x4, y4, x1, y1 );
   R_flush();
}

