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
#include "gis.h"
#include "Vect.h"
#include "flood.h"

/*------------------*/
/* Global variables */
/*------------------*/
int    connect;
int    algorithm;
int    nevents;
char **event_list;

char   xsect_vname[FNAMELEN];
char   xsect_rname[FNAMELEN];
char   xsect_sloc[FNAMELEN];
char   wsurf_rname[FNAMELEN];
char   wsurf_tname[FNAMELEN];
char   depth_rname[FNAMELEN];
char   depth_tname[FNAMELEN];
char   mask_rname[FNAMELEN];
char   inun_rname[FNAMELEN];
char   inun_tname[FNAMELEN];
char   clump_rname[FNAMELEN];
char   elev_rname[FNAMELEN];

main( argc, argv )
   int    argc;
   char **argv;
{
   int    event;

   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   do_init( argv[0] );

   /*----------------------------------*/
   /* Get flood events and input files */
   /*----------------------------------*/
   if ( !get_input( argc, argv ) )
      exit(1);

   /*------------------------------------*/
   /* Set up for floodplain calculations */
   /*------------------------------------*/
   setup_floodplain();

   /*----------------------------------------------*/
   /* Determine floodplain for each selected event */
   /*----------------------------------------------*/
   for( event=0; event < nevents; event++ )
   {
      if ( !calc_floodplain( event ) )
      {
         printf("    ERROR: abnormal termination.\n" );
         exit(1);
      }
   }

   /*-----------*/
   /* Finish up */
   /*-----------*/
   printf("    Done.\n" );
}

/*==========================================================================*/
get_input( argc, argv )
   int    argc;
   char **argv;
{
   char  *mapset;

   /*----------------------------*/
   /* Get events to be processed */
   /*----------------------------*/
   mapset = G_find_file( felement, fwsurf_ctrl, "" );
   if ( mapset )
      nevents = ctrl_events( mapset );
   else
#ifdef PARSE
      /*-------------------------------------------------*/
      /* use GRASS's command line and interactive parser */
      /*-------------------------------------------------*/
      nevents = cl_parse( argc, argv );
#else
      /*-------------------------------------*/
      /* use old style interactive prompting */
      /*-------------------------------------*/
      nevents = ask_events();
#endif

   if ( !nevents )
   {  
      printf("    ERROR: no flood events available for processing.\n" );
      return(0);
   }
   ssort( nevents, event_list );
   list_events( nevents, event_list );

   /*--------------------------------------*/
   /* Make sure elevation map is available */
   /*--------------------------------------*/
   mapset = G_find_file( "cell", elev_mconv, "" );
   if ( mapset )
      strcpy( elev_rname, elev_mconv );
   else
   {
      printf("    Enter elevation (raster) map name: ");
      scanf( " %s", elev_rname );
      mapset = G_find_file( "cell", elev_rname, "" );
      if ( mapset == NULL )
      {
          printf ( "    ERROR: elevation map '%s' not found.\n",
                        elev_rname );
          return(0);
      }
   }

   return(1);
}
/*==========================================================================*/
ctrl_events( mapset )
   char *mapset;
{
   int   count;
   FILE *ctrl_fid;
   char *gotline;
   char  token[WORDLEN];
   char  answer[WORDLEN];
   char  buffer[LINELEN];

   /*-------------------*/
   /* Read flood events */
   /*-------------------*/
   count = read_events( mapset, fwsurf_ctrl, &event_list, &ctrl_fid );
   if ( !count )
      return(0);

   /*-------------------------------------*/
   /* See what the processing options are */
   /*-------------------------------------*/
   connect   = FALSE;
   gotline = fgets( buffer, LINELEN, ctrl_fid );
   while ( gotline )
   {
      /*--------------------------------------------*/
      /* keep reading until we get to an assignment */
      /*--------------------------------------------*/
      while ( sscanf( buffer, " %s = %s", token, answer ) != 2   
              &&  gotline )
         gotline = fgets( buffer, LINELEN, ctrl_fid );

      /*-----------------------------*/
      /* process keyword assignments */
      /*-----------------------------*/
      if ( gotline )
      {
         /*---------------------------------------------------*/
         /* see if it is a hydraulic connectivity designation */
         /*---------------------------------------------------*/
         if( !strcmp( token, "CONNECT" )  ||  !strcmp( token, "connect" ) )
         {
            if( answer[0] == 'Y'  ||  answer[0] == 'y' )
            {
               connect  = TRUE;
               printf( "    NOTE: Hydraulic connectivity will be enforced.\n" );
            }
            else if( answer[0] == 'N'  ||  answer[0] == 'n' )
               connect  = FALSE;
         }

         /*-------------------------------------------*/
         /* see if it is an interpolation designation */
         /*-------------------------------------------*/
         else if( !strcmp( token, "INTERPOLATE" )  ||  
             !strcmp( token, "interpolate" ) )
         {
            algorithm = check_algorithm( answer );

            if ( algorithm )
            {
               sprintf( buffer, "    NOTE: %s %s\n", answer,
                        " interpolation algorithm will be used." );
               printf( buffer );
            }
         }

         /*-------------------------*/
         /* get the next input line */
         /*-------------------------*/
         gotline = fgets( buffer, LINELEN, ctrl_fid );
      }
   }

   /*-----------------------*/
   /* set default algorithm */
   /*-----------------------*/
   if ( !algorithm )
   {
      algorithm = IDW;
      printf("    NOTE: IDW interpolation algorithm will be used.\n" );
   }

   fclose( ctrl_fid );
   return( count );

}

/*==========================================================================*/
ask_events()
{
   int count;

   /*-----------------------*/
   /* prompt user for input */
   /*-----------------------*/
   count = prompt_events( "dig", xsect_wsel_mconv, &event_list );

   /*------------------------------------------------*/
   /* See what the interpolation algorithm option is */
   /*------------------------------------------------*/
   algorithm = ask_algorithm();

   /*------------------------------------------------*/
   /* See what the hydraulic connectivity  option is */
   /*------------------------------------------------*/
   connect = ask_yesno("    Force hydraulic connectivity?");

   return( count );
}

/*==========================================================================*/
cl_parse( argc, argv )
   int    argc;
   char **argv;
{
   int valid, count;
   struct { struct Flag *connect; } flag;
   struct { struct Option *event, *alg; } parm;

   parm.event = G_define_option();
   parm.event->key = "event";
   parm.event->key_desc = "interval";
   parm.event->type = TYPE_STRING;
   parm.event->description = "Flood events to process";
   parm.event->required = YES;
   parm.event->multiple = YES;

   parm.alg = G_define_option();
   parm.alg->key = "interpolate";
   parm.alg->key_desc = "option";
   parm.alg->type = TYPE_STRING;
   parm.alg->description = "Interpolation algorithm";
   parm.alg->options = "voronoi,tps,idw,idw2,contour";
   parm.alg->answer = "idw";

   flag.connect = G_define_flag ();
   flag.connect->key = 'c';
   flag.connect->description = "Enforce hydraulic connectivity";
 
   if ( G_parser( argc,argv ) )
      exit(1);

   /*------------------------------*/
   /* get the command line answers */
   /*------------------------------*/
   connect = flag.connect->answer;
   algorithm = check_algorithm( parm.alg->answer );
 
   /*-----------------------*/
   /* set up the event list */
   /*-----------------------*/
   valid = TRUE;
   for( count=0; parm.event->answers[count] != NULL; count++ );
   event_list = (char **) malloc( count *  sizeof( char *) );
   if ( !event_list )
      mem_exit();
   for( count=0; parm.event->answers[count] != NULL; count++ )
   {
      if ( !set_event( &(event_list[count]), parm.event->answers[count] ) )
         valid = FALSE;
   }
   if ( !valid )
   {
      printf( "    ERROR: Invalid flood event specification.\n" );
      return( 0 );
   }
   return( count );
}   
 
/*==========================================================================*/
setup_floodplain()
{
   char   command[BUFFLEN];

   /*-----------------------------*/
   /* Construct generic map names */
   /*-----------------------------*/
   sprintf( wsurf_tname, "%s.%s", wsurf_mconv, "temp" );
   sprintf( depth_tname, "%s.%s", depth_mconv, "temp" );
   sprintf( inun_tname, "%s.%s", inun_mconv, "temp" );
   strcpy( mask_rname, "MASK" );
}

/*==========================================================================*/
calc_floodplain( event )
   int    event;
{
   int    nflows;
   char   command[BUFFLEN];
   char   suffix[SUFFIXLEN];

   /*------------------------------------*/
   /* Construct event-specific map names */
   /*------------------------------------*/
   map_suffix( event_list[event], suffix );
   sprintf( xsect_vname, "%s.%s", xsect_wsel_mconv, suffix );
   strcpy( xsect_rname, xsect_vname );
   sprintf( wsurf_rname, "%s.%s", wsurf_mconv, suffix );
   sprintf( depth_rname, "%s.%s", depth_mconv, suffix );
   sprintf( inun_rname, "%s.%s", inun_mconv, suffix );
   sprintf( clump_rname, "%s.%s", clump_mconv, suffix );

   /*------------------------------------------------------*/
   /* Set up cross section map for interpolation algorithm */
   /*------------------------------------------------------*/
   switch( algorithm )
   {
      case TPS:
         /*-----------------------------------------------------------------*/
         /* Create sites from cross section map of water surface elevations */
         /*-----------------------------------------------------------------*/
         G__file_name( xsect_sloc, "dig_ascii", xsect_vname, f_mapset );
         sprintf( command, "v.out.point input=%s > %s", 
                            xsect_vname, xsect_sloc );
         system( command );

         sprintf( command, "s.in.ascii sites=%s input=%s fs='|' >> %s", 
                            xsect_vname, xsect_sloc, log_fname );
         system( command );

         /*----------------------------------*/
         /* use event-specific map names to  */
         /* construct interpolation cmd      */
         /*----------------------------------*/
         sprintf( command, "s.surf.tps input=%s elev=%s >> %s", 
                       xsect_vname, wsurf_tname, log_fname ); 
         break;

      case VORONOI:
         /*---------------------------------------------------------------*/
         /* Create cell map cross section map of water surface elevations */
         /*---------------------------------------------------------------*/
         sprintf( command, "v.to.rast input=%s output=%s>> %s", 
                            xsect_vname, xsect_vname, log_fname );
         system( command );

         /*----------------------------------*/
         /* use event-specific map names to  */
         /* construct interpolation cmd      */
         /*----------------------------------*/
         sprintf( command, "r.surf.voronoi input=%s output=%s >> %s", 
                           xsect_vname, wsurf_tname, log_fname ); 
         break;

      case IDW:
         /*---------------------------------------------------------------*/
         /* Create cell map cross section map of water surface elevations */
         /*---------------------------------------------------------------*/
         sprintf( command, "v.to.rast input=%s output=%s>> %s", 
                            xsect_vname, xsect_vname, log_fname );
         system( command );

         /*----------------------------------*/
         /* use event-specific map names to  */
         /* construct interpolation cmd      */
         /*----------------------------------*/
         sprintf( command, "r.surf.idw input=%s output=%s >> %s", 
                           xsect_vname, wsurf_tname, log_fname ); 
         break;

      case IDW2:
         /*---------------------------------------------------------------*/
         /* Create cell map cross section map of water surface elevations */
         /*---------------------------------------------------------------*/
         sprintf( command, "v.to.rast input=%s output=%s>> %s", 
                            xsect_vname, xsect_vname, log_fname );
         system( command );

         /*----------------------------------*/
         /* use event-specific map names to  */
         /* construct interpolation cmd      */
         /*----------------------------------*/
         sprintf( command, "r.surf.idw2 input=%s output=%s >> %s", 
                           xsect_vname, wsurf_tname, log_fname ); 
         break;

      case CONTOUR:
         /*---------------------------------------------------------------*/
         /* Create cell map cross section map of water surface elevations */
         /*---------------------------------------------------------------*/
         sprintf( command, "v.to.rast input=%s output=%s>> %s", 
                            xsect_vname, xsect_vname, log_fname );
         system( command );

         /*----------------------------------*/
         /* use event-specific map names to  */
         /* construct interpolation cmd      */
         /*----------------------------------*/
         sprintf( command, "r.surf.contour input=%s output=%s >> %s", 
                           xsect_vname, wsurf_tname, log_fname ); 
         break;
   }

   /*---------------------------*/
   /* Interpolate water surface */
   /*---------------------------*/
   printf("    %s %s-year event\n    %s\n\n",
               "Delimiting floodplain for", event_list[event],
               "(be patient, this may take a while)..." );
   system( command );

   /*---------------------------------------------------------------*/
   /* Calculate flood depths from water surface and terrain surface */
   /*---------------------------------------------------------------*/
   sprintf( command, "r.mapcalc %s = '%s-(%s * 10)' >> %s", 
                      temp_mconv, wsurf_tname, elev_rname, log_fname );
   system( command );

   sprintf( command, "r.mapcalc %s = 'if( %s, %s, 0, 0 )' >> %s", 
                      depth_tname, temp_mconv, temp_mconv, log_fname );
   system( command );

   /*----------------------------------------------*/
   /* Enforce hydraulic connectivity, if requested */
   /*----------------------------------------------*/
   if ( connect )
      nflows = do_connect( suffix );

   /*----------------------------------------------------------*/
   /* Set permanent maps, if not already done via connectivity */
   /*----------------------------------------------------------*/
   if ( !connect || (connect && nflows == 1) )
   {
      /*------------------*/
      /* rename depth map */
      /*------------------*/
      sprintf( command, "g.rename rast=%s,%s >> %s",
                         depth_tname, depth_rname, log_fname );
      system( command );

      /*----------------------------*/
      /* resample water surface map */
      /*----------------------------*/
      sprintf( command, "g.copy rast=%s,%s >> %s",
                         depth_rname, mask_rname, log_fname );
      system( command );
      sprintf( command, "r.resample -q input=%s output=%s >> %s",
                         wsurf_tname, wsurf_rname, log_fname );
      system( command );
   }

   /*--------------------------*/
   /* Remove intermediate maps */
   /*--------------------------*/
   sprintf( command, "g.remove rast=%s,%s,%s,%s,%s,%s,%s >> %s", 
                      xsect_rname, temp_mconv, mask_rname,
                      inun_tname, wsurf_tname,
                      clump_rname, inun_rname, log_fname );
   system( command );
   return(1);
}

/*==========================================================================*/
ask_algorithm()
{
   int   alg_opt;
   int   valid;
   char  token[WORDLEN];

   valid = FALSE;
   while ( !valid )
   {
      printf("    %s %s : " ,
             "Enter interpolation algorithm option",
             "[VORONOI, IDW, IDW2, CONTOUR]" );
      scanf( " %s", token );


      alg_opt = check_algorithm( token );
      if ( alg_opt )
         valid = TRUE;
    }

   return( alg_opt );
}
/*==========================================================================*/
check_algorithm( answer )
   char *answer;
{

   int alg_opt;

   if( !strcmp( answer, "VORONOI" )  ||  !strcmp( answer, "voronoi" ) )
      alg_opt = VORONOI;

   else if( !strcmp( answer, "TPS" )  ||  !strcmp( answer, "tps" ) )
      alg_opt = TPS;

   else if( !strcmp( answer, "IDW" )  ||  !strcmp( answer, "idw" ) )
      alg_opt = IDW;

   else if( !strcmp( answer, "IDW2" )  ||  !strcmp( answer, "idw2" ) )
      alg_opt = IDW2;

   else if( !strcmp( answer, "CONTOUR" )  ||  !strcmp( answer, "contour" ) )
      alg_opt = CONTOUR;

   else
      alg_opt = IGNORE;

   return( alg_opt );
}

/*==========================================================================*/
do_connect( suffix )
   char  *suffix;
{
   char   command[BUFFLEN];
   int    display;
   int    clump_mid, mask_mid;
   int    screen_x, screen_y, button;
   int    row, col;
   double east, north;
   struct Categories pcats;
   CELL  *row_buffer, area_id;
   C_HEAD window;

   /*------------------------------*/
   /* create simple inundation map */
   /*------------------------------*/
   printf("    Checking for hydraulic connectivity...\n" );
   sprintf( command, "r.mapcalc %s = 'if(%s,1,0,0)' >> %s",
                      inun_tname, depth_tname, log_fname );
   system( command );

   /*----------------------*/
   /* clump inundation map */
   /*----------------------*/
   sprintf( command, "r.clump -q input=%s output=%s >> %s",
                      inun_tname, clump_rname, log_fname );
   system( command );

   /*------------------------------------------------*/
   /* See if there is more than one area of flooding */
   /*------------------------------------------------*/
   if ( G_read_cats( clump_rname, f_mapset, &pcats ) )
   {
      printf("    ERROR: Cannot read categories for map %s.\n", 
                  clump_rname );
      return(0);
   }

   if ( pcats.num > 1 )
   {
      /*------------------------------------------------------*/
      /* Open clump map of flood areas and temporary MASK map */
      /*------------------------------------------------------*/
      clump_mid = G_open_cell_old( clump_rname, f_mapset );
      if ( clump_mid < 0 )
      {
          printf ( "   ERROR: cannot open map '%s'.\n", clump_rname);
          return(0);
      }

      mask_mid = G_open_cell_new( mask_rname );
      if ( mask_mid < 0 )
      {
          printf ( "   ERROR: cannot open map '%s'.\n", mask_rname);
          return(0);
      }

      /*----------------------------------------*/
      /* Ask user to identify area of main flow */
      /*----------------------------------------*/
      init_graphics( &window );
      printf( "    NOTE: There are areas of disconnected flows.\n" );
      display = ask_yesno("    Do you want to see the flood delineation?");
      if ( display )
         Dcell( depth_tname, f_mapset, 0 );
      area_id = 0;
      while ( area_id == 0 )
      {
         printf( "    Locate a point within the area of main flow.\n" );
         printf( "    (Any mouse button)\n" );
         R_get_location_with_pointer( &screen_x, &screen_y, &button );
         if ( !screen_to_utm( window, screen_x, screen_y, &east, &north ) )
         {
            printf("    ERROR: cannot convert screen coordinates.\n");
            return(0);
         }
         coord_to_cell( window, north, east, &row, &col );

         /*------------------------------------*/
         /* See which area the user identified */
         /*------------------------------------*/
         row_buffer = G_allocate_cell_buf();
         G_get_map_row( clump_mid, row_buffer, row );
         area_id = row_buffer[col];
      }

      /*----------------------------------------*/
      /* Reclass the clump map to create a mask */
      /*----------------------------------------*/
      for( row = 0; row < window.rows; row++ )
      {
         G_get_map_row( clump_mid, row_buffer, row );
         for ( col = 0; col < window.cols; col++ )
         {
            if ( row_buffer[col] != area_id )
               row_buffer[col] = 0;
         }
         G_put_map_row( mask_mid, row_buffer );
      }

      /*------------------------------------------------------*/
      /* Resample the depth and wsurf maps with mask in place */
      /*------------------------------------------------------*/
      G_close_cell( clump_mid );
      G_close_cell( mask_mid );
      sprintf( command, "r.resample -q input=%s output=%s >> %s",
                         depth_tname, depth_rname, log_fname );
      system( command );
      sprintf( command, "r.resample -q input=%s output=%s >> %s",
                         wsurf_tname, wsurf_rname, log_fname );
      system( command );
      if ( display )
         Dcell( depth_rname, f_mapset, 0 );
   }

   return( pcats.num );
}
/*==========================================================================*/
