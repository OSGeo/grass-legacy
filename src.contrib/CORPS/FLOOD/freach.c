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

#include "gis.h"
#include "Vect.h"
#include "flood.h"

/*------------------------*/
/* Global data structures */
/*------------------------*/
#define  FIELDWIDTH 25

static int nlines = 50;
C_HEAD     window;

int    reach_rid;
int    depth_rid;
M_INFO reach_vid;

/*==========================================================================*/
main( argc, argv )
   int    argc;
   char **argv;
{
   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   do_init( argv[0] );
   init_graphics( &window );
 
   /*-----------------------------*/
   /* Get input stuff for f.reach */
   /*-----------------------------*/
   if ( !get_input() )
      exit(1);

   /*-----------------------------------*/
   /* Interactive prompting for reaches */
   /*-----------------------------------*/
   what();

   /*----------*/
   /* Clean up */
   /*----------*/
   Vect_close( &reach_vid );
   close( reach_rid );
   close( depth_rid );
   R_close_driver();
   printf( "    Done.\n" );

}

/*==========================================================================*/
get_input()
{
   char   rast_name[FNAMELEN];
   char   xsect_fname[FNAMELEN];
   char   sorted_fname[FNAMELEN];
   char  *mapset;
   M_INFO xsect_vid;

   /*-------------------------------*/
   /* Get displayed raster map name */
   /*-------------------------------*/
   if ( D_get_cell_name( rast_name ) )
   {
      printf("    Enter underlying raster map name e.g. 'depth.<event>': ");
      scanf( "%s", rast_name );
   }
   else if ( strncmp( rast_name, depth_mconv, strlen(depth_mconv)  ) )
      printf("%s\n%s\n",
             "    WARNING: You are not displaying a flood depth map.",
             "             Calculated statistics may be unpredictable." );
      
   /*-----------------*/
   /* Get mapset name */
   /*-----------------*/
   mapset = G_find_file( "cell", rast_name, "" );
   if ( mapset == NULL )
   {
       printf ( "    ERROR: Displayed map '%s' not found.\n", rast_name );
       return(0);
   }
   
   /*---------------------------*/
   /* Open displayed raster map */
   /*---------------------------*/
   depth_rid = G_open_cell_old( rast_name, mapset );
   if ( depth_rid < 0 )
   {
       printf ( "    ERROR: Cannot open displayed map '%s'.\n", rast_name );
       return(0);
   }

   /*-------------------------------------------------*/
   /* Create reach map from unknown cross section map */
   /*-------------------------------------------------*/
   if ( !make_reach( NULL ) )
   {
       printf ("    ERROR: Cannot identify river reaches.\n" );
       return(0);
   }

   /*-----------------------*/
   /* Open raster reach map */
   /*-----------------------*/
   reach_rid = G_open_cell_old( reach_mconv, mapset );
   if ( reach_rid < 0 )
   {
       printf ( "    ERROR: Cannot open raster reach map '%s'.\n", 
                     reach_mconv );
       return(0);
   }
 
   /*-----------------------*/
   /* Open vector reach map */
   /*-----------------------*/
   if ( Vect_open_old (&reach_vid, reach_mconv, mapset ) < 2 )
   {
       printf ("    ERROR: Cannot open vector reach map '%s'.\n",
                    reach_mconv );
       return(0);
   }


   return(1);
}

/*==========================================================================*/
what()
{
   int button;
   int screen_x, screen_y;
   double  east, north;
   double  avg, area, vol;
   double  feet, meters;
   double  sq_mi, sq_meters;
   double  acres, hectares;
   double  cu_ft, cu_meters, acre_ft;
   P_AREA *Reach ;
   plus_t  reach ;

   while( TRUE )
   {
      /*------------------*/
      /* Get button click */
      /*------------------*/
      show_buttons();
      R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
      if (button == 2) continue;
      if (button == 3) break;

      /*-----------------------------*/
      /* Determine selected location */
      /*-----------------------------*/
      screen_to_utm( window, screen_x, screen_y, &east, &north );

      /*--------------------------*/
      /* Determine selected reach */
      /*--------------------------*/
      reach = dig_point_to_area( &reach_vid, east, north );
      if ( reach > 0 )
      {
         Reach = &(reach_vid.Area[reach]);
         if ( Reach->att )
            printf ("\n    Reach %d\n", Reach->att );
         else
            printf("\n    INTERNAL ERROR: Reach not identified.\n" );

         /*-------------------------------------------------*/
         /* Calculate area, avg. depth, and volume in reach */
         /*-------------------------------------------------*/
         calc_stat( Reach->att, &avg, &area, &vol );
         feet      = avg;
         meters    = cvt_dist( avg, FEET, METERS );

         sq_mi     = cvt_area( area, PROJECTION, SQ_MILES );
         acres     = cvt_area( area, PROJECTION, ACRES );
         sq_meters = cvt_area( area, PROJECTION, SQ_METERS );
         hectares  = cvt_area( area, PROJECTION, HECTARES );

         cu_meters = cvt_volume( vol, FEET, CU_METERS );
         cu_ft     = cvt_volume( vol, FEET, CU_FEET );
         acre_ft   = cvt_volume( vol, FEET, ACRE_FEET );

         /*-------------------------*/
         /* Format and print values */
         /*-------------------------*/
         printf("       Flooded area:\n" );
         post( sq_mi,     "sq.miles" );
         post( acres,     "acres" );
         post( sq_meters, "sq.meters" );
         post( hectares,   "hectares" );

         printf("\n       Average flood depth:\n" );
         post( feet,      "feet" );
         post( meters,    "meters" );

         printf("\n       Volume of floodwater:\n" );
         post( cu_meters, "cu.meters" );
         post( cu_ft,     "cu.feet" );
         post( acre_ft,   "acre ft." );
      }
      else
         printf( "    No reach identified.\n" );
   }
}

/*==========================================================================*/
calc_stat( area_att, avg, area, volume )
   int     area_att;
   double *avg;
   double *area;
   double *volume;
{
   int    row;
   int    col;
   int    nrows;
   int    ncols;
   int    ncells;
   CELL  *depth_buffer;
   CELL  *reach_buffer;
   double cell_area;
   double sum;
   C_HEAD window;

   /*----------------*/
   /* Initialization */
   /*----------------*/
   G_get_set_window( &window );
   depth_buffer = G_allocate_cell_buf();
   reach_buffer = G_allocate_cell_buf();
   nrows        = window.rows;
   ncols        = window.cols;
   *avg         = 0.0;
   *area        = 0.0;
   *volume      = 0.0;
   sum          = 0.0;
   ncells       = 0;

   /*------------------------------------------------------*/
   /* Calculate avg. water depth and water volume in reach */
   /*------------------------------------------------------*/
   for ( row=0; row < nrows; row++ )
   {
      G_get_map_row( depth_rid, depth_buffer, row );
      G_get_map_row( reach_rid, reach_buffer, row );
      for ( col=0; col < ncols; col++ )
      {
         if ( reach_buffer[col] == area_att  &&  depth_buffer[col]  > 0 )
         {
            sum += depth_buffer[col] / 10.0;
            ncells++;
         }
      }
   }
   if ( ncells )
   {
      cell_area = window.ew_res * window.ns_res;
      *avg      = sum / (double)ncells;
      *area     = ncells * cell_area;
      *volume   = sum * cvt_area( cell_area, PROJECTION, SQ_FEET );

   }

   /*----------*/
   /* Clean up */
   /*----------*/
   free( depth_buffer );
   free( reach_buffer );
}

/*==========================================================================*/
post( value, units )
   double value;
   char  *units;
{
   char    val_str[WORDLEN];
   char    rjust_str[WORDLEN];

   value_fmt( value, val_str, ONEDEC, FALSE );
   r_justify( rjust_str, val_str, FIELDWIDTH );
   printf("%s %s\n", rjust_str, units );
}

/*==========================================================================*/
show_buttons ()
{
   if (nlines >= 18)      /* display prompt every screen full */
   {
      fprintf (stderr, "\n");
      fprintf (stderr, "    Buttons\n");
      fprintf (stderr, "       Left:  what's here?\n");
      fprintf (stderr, "       Right: quit\n\n");
      nlines = 4;
   }
}

