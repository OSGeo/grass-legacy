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
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "flood.h"

#define  FIELDWIDTH 20
/*------------------------*/
/* Global data structures */
/*------------------------*/
double elev_start;
double elev_end;
double elev_incr;
double max_vol;
int    brief;
int    end_on_vol;
int    elev_rid;
int    poly_rid;

/*==========================================================================*/
main( argc, argv )
   int    argc;
   char **argv;
{
   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   do_init( argv[0] );
   R_open_driver();
   D_setup(0);
 
   /*------------------------------*/
   /* Get input stuff for f.volume */
   /*------------------------------*/
   if ( !get_input() )
      exit(1);

   /*---------------------------------------------*/
   /* Interactive prompting for areas of interest */
   /*---------------------------------------------*/
   what();

   /*----------*/
   /* Clean up */
   /*----------*/
   close( poly_rid );
   close( elev_rid );
   R_close_driver();
   printf( "    Done.\n" );

}

/*==========================================================================*/
get_input()
{
   int    valid;
   int    dividend;
   double remainder;
   double difference;
   char   elev_rname[FNAMELEN];
   char   poly_name[FNAMELEN];
   char   token[WORDLEN];
   char  *mapset;

   /*---------------------*/
   /* Get start elevation */
   /*---------------------*/
   valid = FALSE;
   while( !valid )
   {
      printf("    Enter start elevation: ");
      if ( scanf( "%lf", &elev_start )  == 1 )
         valid = TRUE;
   }
   /*---------------------------------*/
   /* Get end elevation or max volume */
   /*---------------------------------*/
   valid = FALSE;
   while( !valid )
   {
      printf("    Enter end elevation: ");
      scanf( "%s", token );
      if ( token[0] == 'v' || token[0] == 'V' )
      {
         printf("    Enter maximum volume (acre-feet): ");
         if ( scanf( "%lf", &max_vol )  == 1 )
         {
            end_on_vol = TRUE;
            valid = TRUE;
         }
      }
      else
      {
         if ( sscanf( token, "%lf", &elev_end )  == 1  &&
                      elev_end > elev_start )
         {
            end_on_vol = FALSE;
            valid = TRUE;
         }
         else if ( elev_end < elev_start )
            printf("    %s %s\n", 
                   "ERROR: end elevation must be >= start elevation.",
                   "Try again." );
      }
   }
   /*-------------------*/
   /* Get end elevation */
   /*-------------------*/
   valid = FALSE;
   while( !valid )
   {
      printf("    Enter elevation increment: ");
      if ( scanf( "%lf", &elev_incr )  == 1 )
         valid = TRUE;
   }

   /*-------------------------------*/
   /* Make sure increment will work */
   /*-------------------------------*/
   if ( !end_on_vol )
   {
      difference = elev_end - elev_start;
      dividend   = (int)(difference / elev_incr);
      remainder  = fmod( difference, elev_incr );
      if ( remainder > 0.0 )
         elev_end = ((dividend+1)*elev_incr) + elev_start;
   }

   /*-------------------*/
   /* Get elevation map */
   /*-------------------*/
   mapset = G_find_file( "cell", elev_mconv, "" );
   if ( mapset )
      strcpy( elev_rname, elev_mconv );
   else
   {
      printf("    Enter elevation (raster) map name: ");
      scanf( "%s", elev_rname );
      mapset = G_find_file( "cell", elev_rname, "" );
      if ( mapset == NULL )
      {
          printf ( "    ERROR: elevation map '%s' not found.\n",
                        elev_rname );
          return(0);
      }
   }
 
   /*--------------------*/
   /* Open elevation map */
   /*--------------------*/
   elev_rid = G_open_cell_old( elev_rname, mapset );
   if ( elev_rid < 0 )
      {
          printf ( "    ERROR: Cannot open map '%s'.\n", elev_rname );
          return(0);
      }

   /*-------------------------------*/
   /* Get area-of-interest map name */
   /*-------------------------------*/
   printf("    Enter area-of-interest (raster) map name: ");
   scanf( "%s", poly_name );

   /*---------------------*/
   /* Open raster AOI map */
   /*---------------------*/
   poly_rid = G_open_cell_old( poly_name, mapset );
   if ( poly_rid < 0 )
   {
       printf ( "    ERROR: Cannot open area-of-interest raster map '%s'.\n",
                     poly_name );
       return(0);
   }

   return(1);
}

/*==========================================================================*/
what()
{
   int     row, col;
   int     button;
   int     loop_flag;
   int     screen_x, screen_y;
   double  elev;
   double  area;
   double  vol;
   double  east, north;
   CELL    aoi_id;
   CELL   *row_buffer;
   C_HEAD  window;

   /*----------------*/
   /* Initialization */
   /*----------------*/
   G_get_set_window (&window);
   row_buffer = G_allocate_cell_buf();

   while( TRUE )
   {
      /*------------------*/
      /* Get button click */
      /*------------------*/
      show_buttons();
      R_get_location_with_pointer( &screen_x, &screen_y, &button );
      if (button == 2) continue;
      if (button == 3) break;

      /*----------------------------*/
      /* Determine selected polygon */
      /*-----------------------------*/
      screen_to_utm( window, screen_x, screen_y, &east, &north );
      coord_to_cell( window, north, east, &row, &col );
      G_get_map_row( poly_rid, row_buffer, row );
      aoi_id = row_buffer[col];
      if ( aoi_id > 0 )
      {
         /*--------------------------*/
         /* Print header information */
         /*--------------------------*/
         printf ("\n    Area of interest id %d:\n", aoi_id );
         brief = FALSE;
         post_header();

         if ( end_on_vol )
         {
            /*-------------------------------------------*/
            /* Iterate until specified volume is reached */
            /*-------------------------------------------*/
            vol = 0.0;
            loop_flag = TRUE;
            for ( elev=elev_start; loop_flag;  elev+=elev_incr )
               loop_flag = do_stat( aoi_id, elev, &area, &vol );
         }
         else
         {
            /*-----------------------------------*/
            /* Iterate over specified elevations */
            /*-----------------------------------*/
            for ( elev=elev_start; elev <= elev_end;  elev+=elev_incr )
               do_stat( aoi_id, elev, &area, &vol );
         }
         if ( brief )
            printf( "    (*) in thousands\n" );
      }
      else
         printf( "    No area of interest identified.\n" );
   }
}

/*==========================================================================*/
do_stat( poly_att, elev, area, volume )
   int     poly_att;
   double  elev;
   double *area;
   double *volume;
{
   double  hectares;
   double  acre_ft;

   /*-------------------------------------*/
   /* Calculate statistics in AOI-polygon */
   /*-------------------------------------*/
   calc_stat( poly_att, elev, area, volume );

   /*-------------------------*/
   /* Format and print values */
   /*-------------------------*/
   hectares = cvt_area( *area, PROJECTION, HECTARES );
   acre_ft  = cvt_volume( *volume, FEET, ACRE_FEET );
   post_stat( elev, hectares, acre_ft );

   if ( end_on_vol  &&  acre_ft >= max_vol )
      return( FALSE );
   else
      return( TRUE );
}

/*==========================================================================*/
calc_stat( poly_att, elev, area, volume )
   int     poly_att;
   double  elev;
   double *area;
   double *volume;
{
   int    row;
   int    col;
   int    nrows;
   int    ncols;
   int    ncells;
   CELL  *elev_buffer;
   CELL  *aoi_buffer;
   double sum;
   double cell_area;
   C_HEAD window;

   /*----------------*/
   /* Initialization */
   /*----------------*/
   G_get_set_window( &window );
   elev_buffer = G_allocate_cell_buf();
   aoi_buffer  = G_allocate_cell_buf();
   nrows       = window.rows;
   ncols       = window.cols;
   *area       = 0.0;
   *volume     = 0.0;
   sum         = 0.0;
   ncells      = 0;

   /*-------------------------------------------*/
   /* Calculate area volume in area of interest */
   /*-------------------------------------------*/
   for ( row=0; row < nrows; row++ )
   {
      G_get_map_row( poly_rid, aoi_buffer, row );
      for ( col=0; col < ncols; col++ )
      {
         if ( aoi_buffer[col] == poly_att )
         {
            G_get_map_row( elev_rid, elev_buffer, row );
            if ( elev > elev_buffer[col] )
            {
               sum += elev - elev_buffer[col];
               ncells++;
            }
         }
      }
   }
   if ( ncells )
   {
      cell_area = window.ew_res * window.ns_res;
      *area     = ncells * cell_area;
      *volume   = sum * cvt_area( cell_area, PROJECTION, SQ_FEET );
   }

   /*----------*/
   /* Clean up */
   /*----------*/
   free( elev_buffer );
   free( aoi_buffer );
}

/*==========================================================================*/
post_header()
{
   char    buffer[BUFFLEN];
   char    token[WORDLEN];
   char    rjust_str[WORDLEN];

   blank_pad( buffer, BUFFLEN );
   strcpy( token, "elevation (feet)" );
   r_justify( rjust_str, token, FIELDWIDTH );
   strcpy( buffer, rjust_str );

   strcpy( token, "area (hectares)" );
   r_justify( rjust_str, token, FIELDWIDTH );
   strcat( buffer, rjust_str );

   strcpy( token, "volume (acre-feet)" );
   r_justify( rjust_str, token, FIELDWIDTH );
   strcat( buffer, rjust_str );

   printf("%s\n", buffer );
}

/*==========================================================================*/
post_stat( elev, area, vol )
   double elev;
   double area;
   double vol;
{
   int     fwidth;
   char    buffer[BUFFLEN];
   char    val_str[WORDLEN];
   char    rjust_str[WORDLEN];

   /*-----------*/
   /* elevation */
   /*-----------*/
   blank_pad( buffer, BUFFLEN );
   value_fmt( elev, val_str, COUNT, FALSE );
   r_justify( rjust_str, val_str, FIELDWIDTH );
   strcpy( buffer, rjust_str );

   /*------*/
   /* area */
   /*------*/
   value_fmt( area, val_str, ONEDEC, FALSE );
   if ( brief || strlen( val_str ) > FIELDWIDTH-2 )
   {
      value_fmt( area/1000., val_str, ONEDEC, FALSE );
      brief = TRUE;
   }
   r_justify( rjust_str, val_str, FIELDWIDTH );
   if ( brief )
      strcat( rjust_str, "*" );
   strcat( buffer, rjust_str );

   /*--------*/
   /* volume */
   /*--------*/
   value_fmt( vol, val_str, ONEDEC, FALSE );
   if ( brief || strlen( val_str ) > FIELDWIDTH-2 )
   {
      value_fmt( vol/1000., val_str, ONEDEC, FALSE );
      brief = TRUE;
   }
   if ( brief )
      fwidth = FIELDWIDTH - 1;
   else
      fwidth = FIELDWIDTH;
   r_justify( rjust_str, val_str, fwidth );
   if ( brief )
      strcat( rjust_str, "*" );
   strcat( buffer, rjust_str );

   printf("%s\n", buffer );
}

/*==========================================================================*/
show_buttons ()
{
   fprintf (stderr, "\n");
   fprintf (stderr, "    Buttons\n");
   fprintf (stderr, "       Left:  what's here?\n");
   fprintf (stderr, "       Right: quit\n\n");
}

