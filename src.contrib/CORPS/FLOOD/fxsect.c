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

/*-----------------------*/
/* CROSS SECTION globals */
/*-----------------------*/
char     secno_str[WORDLEN];
int      numst;
int      stchl;
int      stchr;
int      xlobl;
int      xlobr;
int      xlch;
int      xlobl_mod;
int      xlobr_mod;
int      xlch_mod;
int      pxsecr;
int      pxsece;
int      iplot;

int      modified;
int      max_gr;
int     *sta;
double  *elev;
X_INFO  *listptr;

/*-----------------------*/
/* Map and file globals  */
/*-----------------------*/
int     elev_rid;
FILE   *descrip_fid;
FILE   *orig_att; 
FILE   *mod_att; 
M_INFO  orig_vid; 
M_INFO  mod_vid; 
char    mod_xsect_vname[FNAMELEN];

/*---------------*/
/* Misc. globals */
/*---------------*/
int     use_orig;
int     xsect_color;
int     flow_color;
int     box_color;
int     erase_color;
C_HEAD  window;


main( argc, argv )
   int argc;
   char **argv;
{
   char fname[FNAMELEN];

   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   do_init( argv[0] );
   init_graphics( &window );
   box_color   = D_translate_color( "yellow" );
   xsect_color = D_translate_color( "red" );
   flow_color  = D_translate_color( "blue" );
   erase_color = D_translate_color( "black" );

   /*---------------------------*/
   /* Find and open input files */
   /*---------------------------*/
   if ( !get_input() )
      exit(1);

   /*--------------------------------------------------------*/
   /* Set up memory for linked lists and cross section stuff */
   /*--------------------------------------------------------*/
   if ( !get_memory() )
      exit(1);

   /*-------------------------------*/
   /* Let the user digitize changes */
   /*-------------------------------*/
   if ( !digit_xsect() )
      exit(1);

   /*-----------------------------*/
   /* Write new cross section map */
   /*-----------------------------*/
   if ( !map_xsect() )
      exit(1);

   /*----------*/
   /* Clean up */
   /*----------*/
   if ( use_orig )
   {
      Vect_close( &orig_vid );
      fclose( orig_att );
   }
   Vect_close( &mod_vid );
   fclose( mod_att );
   if  ( modified )
   {
      printf("    NOTE: Modified cross section map is '%s'.\n",
                  mod_xsect_vname );
      support_vector( mod_xsect_vname );
   }
   else
   {
      G__file_name( fname, "dig", mod_xsect_vname, f_mapset );
      unlink( fname );
      G__file_name( fname, "dig_att", mod_xsect_vname, f_mapset );
      unlink( fname );
   }
   R_close_driver();
   printf("    Done.\n" );
}

/*==========================================================================*/
get_input()
{
   int     offset;
   char   *mapset;
   char    title[WORDLEN];
   char    buffer[WORDLEN];
   char    orig_xsect_vname[FNAMELEN];
   char    elev_rname[FNAMELEN];

   /*-----------------------------*/
   /* open HEC-2 description file */
   /*-----------------------------*/
   if ( (descrip_fid = fopen( xsect_hec2_fconv, "w" )) == NULL )
   {
      printf("    ERROR: cannot open HEC2 description file '%s'.\n", 
                  xsect_hec2_fconv );
      return(0);
   }

   /*--------------------------------------*/
   /* Check for original cross section map */
   /*--------------------------------------*/
   mapset = get_xsect_name( orig_xsect_vname );
   if ( mapset )
   {
      /*-----------------------------*/
      /* Process original source map */
      /*-----------------------------*/
      use_orig = TRUE;
      sort = ask_sort();
 
      /*-------------------------------------------------------------*/
      /* Open original cross section map, with sorting, if necessary */
      /*-------------------------------------------------------------*/
      if ( !get_xsect_map( orig_xsect_vname, mapset, &orig_vid ) )
             return(0);

      /*-------------------------------------------------*/
      /* Open original cross section attribute (id )file */
      /*-------------------------------------------------*/
      orig_att = G_fopen_old( "dig_att", orig_xsect_vname, mapset );
      if ( !orig_att )
      {
         printf ("    ERROR: No attribute file for vector map '%s'.\n", 
                      orig_xsect_vname);
         return(0);
      }
   }
   else
      /*----------------------------------------*/
      /* User is starting new cross section map */
      /*----------------------------------------*/
      use_orig = FALSE;

   /*----------------------------*/
   /* Open new cross section map */
   /*----------------------------*/
   if ( use_orig )
      sprintf( mod_xsect_vname, "%s.mod", orig_xsect_vname );
   else
      strcpy( mod_xsect_vname, orig_xsect_vname );
   
   if ( ( Vect_open_new( &mod_vid, mod_xsect_vname ) ) < 0 )
   {
       printf ("    ERROR: Cannot open new cross section map '%s'.\n", 
                    mod_xsect_vname );
       return(0);
   }

   /*--------------------------------------------*/
   /* Open new cross section attribute (id )file */
   /*--------------------------------------------*/
   if ((mod_att = G_fopen_new( "dig_att", mod_xsect_vname, f_mapset )) == NULL)
   {
      printf ("    ERROR: Cannot open attribute file for vector map '%s'.\n", 
                   mod_xsect_vname);
      return(0);
   }

   /*------------------------*/
   /* Get elevation map name */
   /*------------------------*/
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
       printf ( "   ERROR: cannot open elevation map '%s'.\n",
                           elev_rname);
       return(0);
   }

   /*------------------------------*/
   /* Set up header for output map */
   /*------------------------------*/
   if ( use_orig )
   {
      Vect_copy_head_data( &orig_vid.head, &mod_vid.head );
      strcpy (mod_vid.head.map_name, "Output from f.xsection");
      strcpy (mod_vid.head.your_name, G_whoami ());
   }
   else
      get_head_info( FALSE, &(mod_vid.head) );

   /*----------------------------------------------*/
   /* Set up header info for HEC2 description file */
   /*----------------------------------------------*/
   blank_pad( title, WORDLEN );
   sprintf( buffer, "HEC-2 CROSS SECTION DESCRIPTIONS -- %s (%s)\n\n\n",
                     G_location(), f_mapset );
   offset = (WORDLEN - strlen(buffer) ) / 2;
   strcpy( title+offset, buffer );
   fprintf( descrip_fid, title );

   if ( use_orig )
   {
      fprintf( descrip_fid, "Original Cross Section Map: %s \n", 
                             orig_xsect_vname );
      fprintf( descrip_fid, "Modified Cross Section Map: %s \n", 
                             mod_xsect_vname );
   }
   else
      fprintf( descrip_fid, "New Cross Section Map: %s \n", 
                             mod_xsect_vname );

   return(1);
}
/*==========================================================================*/
get_memory()
{
   int     i;
   char    buffer[LINELEN];
   X_INFO *newptr;
   X_INFO *oldptr;

   listptr = NULL;
   if ( use_orig )
   {
      /*-----------------------------------------------*/
      /* Set up original cross sections in linked list */
      /*-----------------------------------------------*/
      for ( i=0; i<orig_vid.n_lines; i++ )
      {
         /*--------------------*/
         /* allocate structure */
         /*--------------------*/
         newptr = (X_INFO *) malloc( sizeof(X_INFO) );
         if ( !newptr )
            mem_exit();

         /*---------------*/
         /* assign fields */
         /*---------------*/
         newptr->source = ORIG;
         newptr->index  = i;
         newptr->points = Vect_new_line_struct();
         newptr->next   = NULL;

         Vect_read_next_line( &orig_vid, newptr->points );
         if ( !fgets( buffer, LINELEN, orig_att ) )
         {
            printf("    ERROR: reading original cross section attributes.\n" );
            return(0);
         }
         sscanf( buffer, " %*s %*f %*f %d", &(newptr->secno) );

         /*-----------------*/
         /* establish links */
         /*-----------------*/
         if ( !listptr )
         {
            newptr->prev = NULL;
            listptr = newptr;
         }
         else
         {
            newptr->prev = oldptr;
            oldptr->next = newptr;
         }
         oldptr = newptr;
      }
      Vect_rewind( &orig_vid );
      rewind( orig_att );
   }

   /*------------------------------------------------*/
   /* Allocate memory for GR elevations and stations */
   /*------------------------------------------------*/
   max_gr = GR_INCREMENT;
   elev = (double *) calloc( max_gr, sizeof(double) );
   sta  = (int *) calloc( max_gr, sizeof(int) );
   if ( !elev  ||  !sta )
      mem_exit();

   return(1);
}

/*==========================================================================*/
digit_xsect()
{
   int     valid;
   int     tailend;
   int     secno_int;
   int     accepted;
   int     only_one;
   int     button;
   int     screen_x, screen_y;
   double  x_pair[2];
   double  y_pair[2];
   char    usr_buffer[WORDLEN];
   CELL   *row_buffer;
   X_INFO *us_ptr;
   X_INFO *ds_ptr;
   X_INFO *new_ptr;
   L_PNTS *xs_points;

   /*----------------*/
   /* Initialization */
   /*----------------*/
   row_buffer = G_allocate_cell_buf();
   us_ptr     = NULL;
   ds_ptr     = NULL;
   only_one   = TRUE;
   modified   = FALSE;
   xlobl = xlobl_mod = 0.0;
   xlch  = xlch_mod  = 0.0;
   xlobr = xlobr_mod = 0.0;

   /*------------------------*/
   /* Add new cross sections */
   /*------------------------*/
   while ( TRUE )
   {
      /*--------------------------*/
      /* Initialize for new xsect */
      /*--------------------------*/
      numst   = 0;
      tailend = FALSE;
      R_standard_color( xsect_color );

      /*------------------------*/
      /* Get xsect id from user */
      /*------------------------*/
      valid = FALSE;
      while ( !valid )
      {
         printf("\n    Enter new cross section id (0 to end): " );
         scanf( "%s", secno_str );
         if ( sscanf( secno_str, "%d", &secno_int ) == 1 )
         {
            if ( strlen( secno_str ) <= 6 )
               valid = TRUE;
            else
               printf( "    Id limited to field width of 6. Try again.\n" );
         }
         else
            printf( "    Invalid cross section id. Try again.\n" );
      }
      if ( secno_int == 0 )
         break;

      /*---------------------------*/
      /* Get plot option from user */
      /*---------------------------*/
      iplot = ask_yesno("    Do you want this cross section plotted in HEC2?" );
      if ( iplot )
      valid = FALSE;
      while ( !valid )
      {
         printf( "Plot all points [A] or only points up to WSEL [W]? ");
         scanf( " %s", usr_buffer );

         if ( !strcmp( usr_buffer, "A" )  ||  !strcmp( usr_buffer, "a" ) )
         {
              valid = TRUE;
              iplot = 1;
         }
         else if ( !strcmp( usr_buffer, "W" )  ||  !strcmp( usr_buffer, "w" ) )
         {
              valid = TRUE;
              iplot = 10;
         }
      }

      /*----------------------------*/
      /* Begin digitizing new xsect */
      /*----------------------------*/
      xs_points = Vect_new_line_struct();
      printf("\n    Begin digitizing cross section %d...\n", secno_int );
      xsect_buttons();

      while( !tailend )
      {  
         /*--------------------------------------------*/
         /* Check for HEC2 limit on number of stations */
         /*--------------------------------------------*/
         if ( numst == 89 )
            printf("    WARNING: approaching HEC2 upper limit for number of stations.\n" );
         else if ( numst == 99 )
         {
            printf("    WARNING: station limit reached. Identify end of cross section.\n" );
            tailend = TRUE;
         }

         /*-----------------*/
         /* Get new station */
         /*-----------------*/
         R_get_location_with_pointer( &screen_x, &screen_y, &button ) ;
         if ( button == 3 ) 
            tailend = TRUE;

         if ( !add_station( row_buffer, screen_x, screen_y, 
                            x_pair, y_pair ) )
         {
            printf("    ERROR: cannot add cross section station.\n" );
            return(0);
         }
         Vect_append_point( xs_points, x_pair[1], y_pair[1] );
         numst++;

         /*-------------------------------------------------------*/
         /* See if we need to allocate more space for GR stations */
         /*-------------------------------------------------------*/
         if ( !tailend  &&  numst == max_gr )
         {
            max_gr += GR_INCREMENT;
            elev = (double *) realloc( (char *) elev,
                             max_gr * sizeof(double) );
            sta  = (int *) realloc( (char *) sta,
                           max_gr * sizeof(int) );
            if ( !elev  ||  !sta )
               mem_exit();
         } 
      } 

      /*-----------------------------------------*/
      /* Ask user for cross section verification */
      /*-----------------------------------------*/
      accepted = point_yesno("    Do you accept this cross section?" );
      if ( !accepted )
      {
         strcpy( usr_buffer, "    NOTE: cross section UN-drawn. Try again." );
         put_line( usr_buffer, xs_points->n_points,
                   xs_points->x, xs_points->y, erase_color );
      }
      else
      {
         /*-----------------------------------------------------*/
         /* These should always be zero for a new cross section */
         /* unless numst=0 due to copy/paste request            */
         /*-----------------------------------------------------*/
         pxsecr = 0;
         pxsece = 0;

         /*------------------------------------*/
         /* Set up for new node in linked list */
         /*------------------------------------*/
         modified = TRUE;
         new_ptr = (X_INFO *) malloc( sizeof(X_INFO) );
         if ( !new_ptr )
            mem_exit();
         new_ptr->source = NEW;
         new_ptr->index  = -1;
         new_ptr->secno  = secno_int;
         new_ptr->points = xs_points;

         /*------------------------------------------------------*/
         /* Have the user identify left and right station points */
         /*------------------------------------------------------*/
         printf( "\n    Identify LEFT bank station (any button):\n" );
         fflush( stdout );
         stchl = which_station( xs_points );

         printf( "\n    Identify RIGHT bank station (any button):\n" );
         fflush( stdout );
         stchr = which_station( xs_points );

         if ( !use_orig  &&  only_one )
            only_one = FALSE;
         else
         {
            /*-------------------------------------------------------------*/
            /* Have user identify leftbank, rightbank, and channel lengths */
            /*-------------------------------------------------------------*/
            printf( "\n    LEFT overbank flow:\n" );
            fflush( stdout );
            us_ptr = NULL;
            ds_ptr = NULL;
            valid = which_flow( new_ptr, FALSE, &xlobl, &xlobl_mod, 
                                &us_ptr, &ds_ptr );

            if ( valid )
            {
               printf( "\n    CHANNEL flow:\n" );
               fflush( stdout );
               valid = which_flow( new_ptr, TRUE, &xlch, &xlch_mod, 
                                   &us_ptr, &ds_ptr );
            }

            if ( valid )
            {
               printf( "\n    RIGHT overbank flow:\n" );
               fflush( stdout );
               valid = which_flow( new_ptr, TRUE, &xlobr, &xlobr_mod, 
                                   &us_ptr, &ds_ptr );
            }

            if ( !valid )
            {
               printf("    ERROR: cannot process flow lines.\n" );
               return(0);
            }
         }

         /*--------------------------------------*/
         /* Add new cross section to linked list */
         /*--------------------------------------------------------------*/
         /* NOTE:  This is where the ds->us assumption is made           */
         /* in this section of code, prev == ds and next == us           */
         /* to be more generic, the user should be able to specify       */
         /* sub-critical (ds->us) or super-critical(us->ds) flow regimes */
         /*--------------------------------------------------------------*/
         if ( us_ptr )
         {
            new_ptr->next = us_ptr;
            us_ptr->prev  = new_ptr;
         }
         else
            new_ptr->next = NULL;

         if ( ds_ptr )
         {
            new_ptr->prev = ds_ptr;
            ds_ptr->next  = new_ptr;
         }
         else
         {
            new_ptr->prev = NULL;
            listptr = new_ptr;
         }

         /*--------------------------------------*/
         /* Write HEC2 cross section description */
         /*--------------------------------------*/
         hec2_xsect( us_ptr );
      }
   }
   return(1);
}

/*==========================================================================*/
add_station( row_buffer, screen_x, screen_y, x_pair, y_pair )
   CELL   *row_buffer;
   int     screen_x;
   int     screen_y;
   double *x_pair;
   double *y_pair;
{
   int     row, col;
   double  east, north;
   double  dist;

   /*----------------------------*/
   /* Determine station location */
   /*----------------------------*/
   if ( !screen_to_utm( window, screen_x, screen_y, &east, &north ) )
   {
      printf("    ERROR: cannot convert screen coordinates.\n");
      return(0);
   }
   coord_to_cell( window, north, east, &row, &col );

   /*--------------------------------*/
   /* Store elevation and station id */
   /*--------------------------------*/
   x_pair[1] = east;
   y_pair[1] = north;

   G_get_map_row( elev_rid, row_buffer, row );
   elev[numst] = row_buffer[col];
   if ( numst == 0 )
   {
      /*---------------*/
      /* first station */
      /*---------------*/
      sta[numst] = 0;
   }
   else
   {
      /*---------------------*/
      /* subsequent stations */
      /*---------------------*/
      dist = distance( x_pair[0], y_pair[0],
                       x_pair[1], y_pair[1] );
      dist = cvt_dist( dist, PROJECTION, FEET );
      sta[numst] = sta[numst-1] + (int)dist;
      if ( sta[numst] > FWIDTH_I8 )
      {
         printf("    ERROR: station numbers have exceeded field width.\n");
         return(0);
      }
      G_plot_line( x_pair[0], y_pair[0],
                   x_pair[1], y_pair[1] );
      R_flush();
   }

   /*--------------------------------------------------*/
   /* Save station's x,y location for next time around */
   /*--------------------------------------------------*/
   x_pair[0] = x_pair[1];
   y_pair[0] = y_pair[1];

   return(1);
}

/*==========================================================================*/
which_station( points )
   L_PNTS *points;
{
   int    accepted;
   int    button;
   int    point_indx;
   int    screen_x;
   int    screen_y;
   double utm_x;
   double utm_y;

   /*------------*/
   /* Initialize */
   /*------------*/
   accepted = FALSE;

   while( !accepted )
   {
      /*----------------------*/
      /* user selects a point */
      /*----------------------*/
      R_get_location_with_pointer( &screen_x, &screen_y, &button ) ;
      screen_to_utm( window, screen_x, screen_y, &utm_x, &utm_y );

      /*----------------------*/
      /* find closest station */
      /*----------------------*/
      point_indx = snap_station( points, utm_x, utm_y );

      /*-----------------------------------*/
      /* Ask user for station verification */
      /*-----------------------------------*/
      put_box( points->x[point_indx], points->y[point_indx], box_color );

      accepted = point_yesno("    Do you accept this station?" );
      if ( !accepted )
      {
         put_box( points->x[point_indx], points->y[point_indx], erase_color );
         printf("    NOTE: station UN-selected. Try again.\n" );
      }
   }

   return( point_indx );
}

/*==========================================================================*/
snap_station( points, utm_x, utm_y )
   L_PNTS *points;
   double  utm_x;
   double  utm_y;
{
   int    i;
   int    first;
   int    winner;
   double dist2;
   double least_dist2;

   first = TRUE;
   for ( i=0; i<numst; i++ )
   {
      dist2 = distance_sq( points->x[i], points->y[i], utm_x, utm_y );
      if ( first )
      {
         least_dist2 = dist2;
         winner      = i;
         first       = FALSE;
      }
      else if ( dist2 < least_dist2 )
      {
         /*--------------*/
         /* closer point */
         /*--------------*/
         least_dist2 = dist2;
         winner      = i;
      }
   }

   return( winner );
}

/*==========================================================================*/
which_flow( new_ptr, c_check, ds_flow_len, us_flow_len, us_ptr, ds_ptr )
   X_INFO  *new_ptr;
   int      c_check;
   int     *ds_flow_len;
   int     *us_flow_len;
   X_INFO **us_ptr;
   X_INFO **ds_ptr;
{
   int     fpts;
   int     erased;
   int     accepted;
   int     valid;
   char    message[BUFFLEN];
   X_INFO *old_us;
   X_INFO *old_ds;
   X_INFO *temp_ptr;
   double  flow_len;
   double  us_x,  us_y;
   double  new_x, new_y;
   double  ds_x,  ds_y;
   double  flow_x[3];
   double  flow_y[3];

   /*------------*/
   /* Initialize */
   /*------------*/
   accepted = FALSE;
   old_us   = *us_ptr;
   old_ds   = *ds_ptr;

   while( !accepted )
   {
      /*--------------------------------------------------------------*/
      /* user selects flow lines by identifying center-of-mass points */
      /*--------------------------------------------------------------*/
      fpts   = 0;
      erased = FALSE;

      /*---------------------------*/
      /* downstream center-of-mass */
      /*---------------------------*/
      printf( "       Identify DOWNSTREAM center-of-mass point.\n" );
      flow_buttons( "DOWNSTREAM" );
      which_xsect( new_ptr, &ds_x,  &ds_y, ds_ptr );
      if ( *ds_ptr )
      {
         flow_x[fpts] = ds_x;
         flow_y[fpts] = ds_y;
         fpts++;
         put_box( ds_x, ds_y, flow_color );
      }
      
      /*--------------------*/
      /* new center-of-mass */
      /*--------------------*/
      valid = FALSE;
      while ( !valid )
      {
         printf( "       Identify NEW center-of-mass point.\n\n" );
         which_xsect( new_ptr, &new_x, &new_y, &temp_ptr );
         if ( temp_ptr )
         {
            flow_x[fpts] = new_x;
            flow_y[fpts] = new_y;
            fpts++;
            put_box( new_x, new_y, flow_color );
            put_line( NULL, fpts, flow_x, flow_y, flow_color );
            valid = TRUE;
         }
         else
         {
            sprintf( message, "    %s %s\n",
                    "ERROR: New cross section must be identified.",
                    "Try again." );
            printf( message );
         }
      }

      /*-------------------------*/
      /* upstream center-of-mass */
      /*-------------------------*/
      printf( "       Identify UPSTREAM center-of-mass point.\n" );
      flow_buttons( "UPSTREAM" );
      which_xsect( new_ptr, &us_x,  &us_y, us_ptr );
      if ( *us_ptr )
      {
         flow_x[fpts] = us_x;
         flow_y[fpts] = us_y;
         fpts++;
         put_box( us_x, us_y, flow_color );
         put_line( NULL, fpts, flow_x, flow_y, flow_color );
      }

      /*----------------------------*/
      /* Verify new xsect selection */
      /*----------------------------*/
      if ( !erased  &&  new_ptr != temp_ptr )
      {
         sprintf( message, "    %s %s",
                 "ERROR: New cross section not identified correctly",
                 "in flow lines. Try again." );
         rm_flow( message, fpts, flow_x, flow_y );
         erased = TRUE;
      }

      /*------------------------*/
      /* Verify us/ds adjacency */
      /*------------------------*/
      if ( !erased  &&  *us_ptr  &&  *ds_ptr  &&  (*us_ptr)->prev != *ds_ptr )
      {
         sprintf( message, "    %s %s",
                 "ERROR: Upstream and downstream cross sections",
                 "must be adjacent. Try again." );
         rm_flow( message, fpts, flow_x, flow_y );
         erased = TRUE;
      }

      /*--------------------------*/
      /* Verify us/ds consistency */
      /*--------------------------*/
      if ( !erased  &&  c_check )
      {
         if (  old_us != *us_ptr  )
         {
            sprintf( message, "    %s %s %s",
                    "ERROR: Upstream cross section does not match",
                    "that of previously drawn flow line.",
                    "Try again." );
            rm_flow( message, fpts, flow_x, flow_y );
            erased = TRUE;
         }
         if (  old_ds != *ds_ptr  )
         {
            sprintf( message, "    %s %s %s",
                    "ERROR: Downstream cross section does not match",
                    "that of previously drawn flow line.",
                    "Try again." );
            rm_flow( message, fpts, flow_x, flow_y );
            erased = TRUE;
         }
      }

      /*--------------------------------*/
      /* Ask user for flow verification */
      /*--------------------------------*/
      if ( !erased )
      {
         accepted = point_yesno("    Do you accept this line of flow?" );
         if ( !accepted )
         {
            strcpy( message, "    NOTE: flow UN-drawn. Try again." );
            rm_flow( message, fpts, flow_x, flow_y );
         }
         else
         {
            /*------------------------*/
            /* Calculate flow lengths */
            /*------------------------*/
            if ( *ds_ptr )
            {
               flow_len = distance( ds_x, ds_y, new_x, new_y );
               flow_len = cvt_dist( flow_len, PROJECTION, FEET );
               *ds_flow_len = (int)flow_len;
            }
            else
               *ds_flow_len = 0.0;

            if ( *us_ptr )
            {
               flow_len = distance( us_x, us_y, new_x, new_y );
               flow_len = cvt_dist( flow_len, PROJECTION, FEET );
               *us_flow_len = (int)flow_len;
            }
            else
               *us_flow_len = 0.0;

            if ( *ds_flow_len > FWIDTH_I8  ||  *us_flow_len > FWIDTH_I8 )
            {
               printf("    ERROR: flow lengths have exceeded field width.\n");
               return(0);
            }
         }
      }
   }
   return(1);
}
/*==========================================================================*/
which_xsect( new_ptr, xsect_x, xsect_y, which_ptr )
   X_INFO  *new_ptr;
   double  *xsect_x;
   double  *xsect_y;
   X_INFO **which_ptr;
{
   int     i;
   int     first;
   int     button;
   int     screen_x;
   int     screen_y;
   double  utm_x;
   double  utm_y;
   double  snap_x;
   double  snap_y;
   double  dist;
   double  min_dist;
   X_INFO *xptr;
   X_INFO *winner;
   L_PNTS *points;

   /*------------*/
   /* Initialize */
   /*------------*/
   first = TRUE;
   xptr  = listptr;

   /*----------------------*/
   /* user selects a point */
   /*----------------------*/
   R_get_location_with_pointer( &screen_x, &screen_y, &button ) ;
   if ( button == 3 )
   {
      /*-----------------------------*/
      /* no cross section applicable */
      /*-----------------------------*/
      *which_ptr = NULL;
      *xsect_x   = 0.0;
      *xsect_y   = 0.0;
   }
   else
   {
      /*-------------------------------------*/
      /* Check cross sections in linked list */
      /*-------------------------------------*/
      screen_to_utm( window, screen_x, screen_y, &utm_x, &utm_y );
      while( xptr )
      {
         points = xptr->points;
         for ( i=0;  i < points->n_points-1;  i++ )
         {
            dist = point_line_snap( utm_x, utm_y, 
                                    points->x[i], points->y[i], 
                                    points->x[i+1], points->y[i+1], 
                                    &snap_x, &snap_y );
            if ( first  ||  dist < min_dist )
            {
               min_dist = dist;
               winner   = xptr;
               *xsect_x = snap_x;
               *xsect_y = snap_y;
   
               if ( first )
                  first = FALSE;
            }
         }
         xptr = xptr->next;
      }   

      /*---------------------------------*/
      /* Check newly added cross section */
      /*---------------------------------*/
      points = new_ptr->points;
      for ( i=0;  i < points->n_points-1;  i++ )
      {
         dist = point_line_snap( utm_x, utm_y,
                                 points->x[i], points->y[i],
                                 points->x[i+1], points->y[i+1],
                                 &snap_x, &snap_y );
         if ( first  ||  dist < min_dist )
         {
            min_dist = dist;
            winner   = new_ptr;
            *xsect_x = snap_x;
            *xsect_y = snap_y;
 
            if ( first )
               first = FALSE;
         }
      }
      *which_ptr = winner;
   }
}

/*==========================================================================*/
double point_line_snap( utm_x, utm_y, x1, y1, x2, y2, snap_x, snap_y )
   double  utm_x,   utm_y;
   double  x1, y1,  x2, y2;
   double *snap_x, *snap_y;
{
   int    snap_dir;
   double delta_x, delta_y;
   double line_b;
   double line_slope;
   double orth_b;
   double orth_slope;
   double orth_x, orth_y;
   double dx1, dx2;
   double dy1, dy2;
   double e1_dist, e2_dist;
   double snap_dist;

   /*-----------------------------*/
   /* Determine direction of snap */
   /*-----------------------------*/
   delta_x = x2 - x1;
   delta_y = y2 - y1;
   if ( delta_x == 0 )
   {
      /*------------------------------------*/
      /* find intersection of the two lines */
      /*------------------------------------*/
      snap_dir = HORIZONTAL;
      orth_x   = x1;
      orth_y   = utm_y;
   }
   else if ( delta_y == 0 )
   {
      /*------------------------------------*/
      /* find intersection of the two lines */
      /*------------------------------------*/
      snap_dir = VERTICAL;
      orth_x   = utm_x;
      orth_y   = y1;
   }
   else
   {
      /*------------------------------*/
      /* get parameters of known line */
      /*------------------------------*/
      snap_dir   = SLOPED;
      line_slope = delta_y / delta_x;
      line_b     = y1 - (line_slope*x1);

      /*-----------------------------------*/
      /* get parameters of orthagonal line */
      /*-----------------------------------*/
      orth_slope = -1.0 / line_slope;
      orth_b     = utm_y - (orth_slope*utm_x);

      /*------------------------------------*/
      /* find intersection of the two lines */
      /*------------------------------------*/
      orth_x = (orth_b - line_b) / 
               (line_slope - orth_slope);
      orth_y = (line_slope*orth_b - orth_slope*line_b) / 
               (line_slope - orth_slope);
   }

   /*-------------------------------------------------*/
   /* See if intersection is on original line segment */
   /*-------------------------------------------------*/
   dx1 = x1 - orth_x;
   dx2 = x2 - orth_x;
   dy1 = y1 - orth_y;
   dy2 = y2 - orth_y;
   if ( (snap_dir == HORIZONTAL  &&  
        ((dy1 > 0  && dy2 > 0)  ||  (dy1 < 0  && dy2 < 0))) ||
        (snap_dir != HORIZONTAL  &&  
        ((dx1 > 0  && dx2 > 0)  ||  (dx1 < 0  && dx2 < 0))) )
   {
      /*-------------------------------------------------------*/
      /* snap direction is not orthagonal, snap to an endpoint */
      /*-------------------------------------------------------*/
      e1_dist = distance( utm_x, utm_y, x1, y1 );
      e2_dist = distance( utm_x, utm_y, x2, y2 );
      if ( e1_dist < e2_dist )
      {
         snap_dist = e1_dist;
         *snap_x   = x1;
         *snap_y   = y1;
      }
      else
      {
         snap_dist = e2_dist;
         *snap_x   = x2;
         *snap_y   = y2;
      }
   }
   else
   {
      /*------------------------------------------------------------------*/
      /* snap direction is orthagonal, snap to solved intersection points */
      /*------------------------------------------------------------------*/
      snap_dist = distance( utm_x, utm_y, orth_x, orth_y );
      *snap_x   = orth_x;
      *snap_y   = orth_y;
   }
   return( snap_dist );
}

/*==========================================================================*/
rm_flow( message, fpts, flow_x, flow_y )
   char *message;
   int   fpts;
   double *flow_x;
   double *flow_y;
{
   int i;

   for( i=0; i < fpts; i++)
      put_box( flow_x[i], flow_y[i], erase_color );
   put_line( message, fpts, flow_x, flow_y, erase_color );
}

/*==========================================================================*/
put_line( msg, npts, x, y, color )
   char   *msg;
   int     npts;
   double *x;
   double *y;
   int     color;
{
   int i;

   R_standard_color( color );

   for( i=0; i < npts - 1; i++ )
      G_plot_line( x[i], y[i], x[i+1], y[i+1] );
   R_flush();

   if ( msg )
      printf( "%s\n", msg );
}

/*==========================================================================*/
put_box( x_point, y_point, color )
   double  x_point;
   double  y_point;
   int     color;
{
   double x1, y1;
   double x2, y2;
   double x3, y3;
   double x4, y4;
   double delta;

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

/*==========================================================================*/
map_xsect()
{
   char    att_buffer[BUFFLEN];
   X_INFO *xptr;

   xptr = listptr;
   while( xptr )
   {
      if ( Vect_write_line( mod_vid, LINE, xptr->points ) < 0 )
      {
         printf("    ERROR: cannot write out new cross section map.\n");
         return(0);
      }
      sprintf( att_buffer, "L %f %f %d", 
               xptr->points->x[0], xptr->points->y[0], xptr->secno );
      fprintf( mod_att, "%s\n", att_buffer );

      xptr = xptr->next;
   }
   return(1);
}
/*==========================================================================*/
hec2_xsect( us_ptr )
   X_INFO  *us_ptr;
{
   int   recno;
   int   nrecords;
   int   remainder;
   char  record[H2_RECLEN];

   /*----------------*/
   /* Write comments */
   /*----------------*/
   fprintf( descrip_fid, "\n\nC---------------------------------\n" );
   fprintf( descrip_fid, "C   New Cross Section %s \n", secno_str );
   fprintf( descrip_fid, "C---------------------------------\n" );

   /*-----------------*/
   /* Write X1 record */
   /*-----------------*/
   get_X1( record );
   fprintf( descrip_fid, "%s\n", record );

   /*------------------*/
   /* Write GR records */
   /*------------------*/
   nrecords  = numst / 5;
   remainder = numst % 5;
   if ( remainder )
      nrecords++;
   for ( recno=0; recno < nrecords; recno++ )
   {
      get_GR( record, recno );
      fprintf( descrip_fid, "%s\n", record );
   }

   /*------------------------*/
   /* Write modified lengths */
   /*------------------------*/
   if ( us_ptr )
   {
      fprintf( descrip_fid, "\n\nC---------------------------------\n" );
      fprintf( descrip_fid, "C   Corrected Cross Section %d \n", us_ptr->secno );
      fprintf( descrip_fid, "C---------------------------------\n" );
      fprintf( descrip_fid, "XLOBL = %ld\n", xlobl_mod );
      fprintf( descrip_fid, "XLOBR = %ld\n", xlobr_mod );
      fprintf( descrip_fid, "XLCH  = %ld\n", xlch_mod );
   }

}

/*==========================================================================*/
get_X1( record )
   char *record;
{
   int   field;
   char  token[WORDLEN];

   /*-----------*/
   /* Record ID */
   /*-----------*/
   field = 0;
   strcpy( token, "X1" );
   put_field( record, field++, token );

   /*----------------*/
   /* field 1: SECNO */
   /*----------------*/
   strcpy( token, secno_str );
   put_field( record, field++, token );

   /*----------------*/
   /* field 2: NUMST */
   /*----------------*/
   sprintf( token, "%d", numst );
   put_field( record, field++, token );

   /*----------------*/
   /* field 3: STCHL */
   /*----------------*/
   sprintf( token, "%d", sta[stchl] );
   put_field( record, field++, token );

   /*----------------*/
   /* field 4: STCHR */
   /*----------------*/
   sprintf( token, "%d", sta[stchr] );
   put_field( record, field++, token );

   /*----------------*/
   /* field 5: XLOBL */
   /*----------------*/
   sprintf( token, "%d", xlobl );
   put_field( record, field++, token );

   /*----------------*/
   /* field 6: XLOBR */
   /*----------------*/
   sprintf( token, "%d", xlobr );
   put_field( record, field++, token );

   /*----------------*/
   /* field 7: XLCH  */
   /*----------------*/
   sprintf( token, "%d", xlch );
   put_field( record, field++, token );

   /*------------------*/
   /* field 8: PXSECR  */
   /*------------------*/
   sprintf( token, "%d", pxsecr );
   put_field( record, field++, token );

   /*------------------*/
   /* field 9: PXSECE  */
   /*------------------*/
   sprintf( token, "%d", pxsece );
   put_field( record, field++, token );

   /*------------------*/
   /* field 10: IPLOT  */
   /*------------------*/
   sprintf( token, "%d", iplot );
   put_field( record, field++, token );

}
/*==========================================================================*/
get_GR( record, recno )
   char *record;
   int   recno;
{
   int    pair;
   int    count;
   int    field;
   char   token[WORDLEN];

   /*-----------*/
   /* Record ID */
   /*-----------*/
   field = 0;
   strcpy( token, "GR" );
   put_field( record, field++, token );

   /*-----------------------------------*/
   /* One GR record holds five stations */
   /*-----------------------------------*/
   count = recno * 5;
   for ( pair=0; pair<5 && count<numst; pair++, count++ )
   {
      /*----------------*/
      /* EL - elevation */
      /*----------------*/
      sprintf( token, "%6.1f", elev[count] );
      put_field( record, field++, token );

      /*---------------*/
      /* STA - station */
      /*---------------*/
      sprintf( token, "%d", sta[count] );
      put_field( record, field++, token );
   }
}

/*==========================================================================*/
put_field( buffer, field, token )
   char *buffer;
   int   field;
   char *token;
{
   char  just_token[WORDLEN];

   if ( field == 0 )
      strcpy( buffer, token );
   else if ( field == 1 )
   {
      r_justify( just_token, token, H2_FIELDLEN - H2_IDLEN );
      strcat( buffer, just_token );
   }
   else
   {
      r_justify( just_token, token, H2_FIELDLEN );
      strcat( buffer, just_token );
   }
}

/*==========================================================================*/
xsect_buttons ()
{
   fprintf (stderr, "    Buttons\n");
   fprintf (stderr, "       Left:   add a station\n");
   fprintf (stderr, "       Right:  add the LAST station\n\n");
}

/*==========================================================================*/
flow_buttons ( token )
   char *token;
{
   char msg[BUFFLEN];

   fprintf (stderr, "       Buttons\n");
   fprintf (stderr, "          Left:   add a flow point\n");
   sprintf( msg, "%s %s %s\n\n",
                    "          Right:  NO", token, "cross section" );
   fprintf( stderr, msg );
}

/*==========================================================================*/
