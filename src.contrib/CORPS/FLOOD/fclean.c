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
#include "flood.h"

main( argc, argv )
   int    argc;
   char **argv;
{
   int    i;
   int    valid;
   int    nevents;
   char   mapname[FNAMELEN];
   char   buffer[BUFFLEN];
   char   suffix[SUFFIXLEN];
   char   token[WORDLEN];
   char  *mapset;
   char **event_list;
   FILE  *ctrl_fid;

   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   printf("*** FCLEAN ***\n");
   G_gisinit( argv[0] );

   /*--------------------------------------------------------------*/
   /* Get flood events to be processed -- either from control file */
   /*                                     or from user response    */
   /*--------------------------------------------------------------*/
   mapset = G_find_file( felement, fclean_ctrl, "" );
   if ( mapset )
      nevents = read_events( mapset, fwsurf_ctrl, 
                             &event_list, &ctrl_fid );
   else
   {
      /*----------------------------*/
      /* Get number of flood events */
      /*----------------------------*/
      valid = FALSE;
      while ( !valid )
      {
         printf("    How many flood events to be cleaned? : ");
         scanf( " %s", token );
         if ( sscanf( token, "%d", &nevents ) == 1 )
         {
            valid = TRUE;
            event_list = (char **) malloc( nevents *  sizeof( char *) );
            if ( !event_list )
               mem_exit();
         }
      }
 
      /*------------*/
      /* Get events */
      /*------------*/
      for ( i=0; i < nevents; i++ )
      {
         valid = FALSE;
         while( !valid )
         {
            sprintf( buffer, "    Enter flood event #%d (e.g., 1, 50, spf): ",
                                  i+1 );
            printf( buffer );
            scanf( " %s", token );
            if ( set_event( &(event_list[i]), token ) )
               valid = TRUE;
            else
               printf( "    Invalid flood event. Try again.\n" );
         }      
      }
   }

   if ( !nevents )
   {  
      printf("    ERROR: no flood events available for processing.\n" );
      exit(1);
   }
   ssort( nevents, event_list );
   list_events( nevents, event_list );

   /*----------------------------*/
   /* For each selected event... */
   /*----------------------------*/
   for( i=0; i < nevents; i++ )
   {
      map_suffix( event_list[i], suffix );
      /*-----------------------------------------------*/
      /* water surface elevation vector cross sections */
      /*-----------------------------------------------*/
      sprintf( mapname, "%s.%s", xsect_wsel_mconv, suffix );
      sprintf( buffer, "g.remove vect=%s >> %s", 
                         mapname, log_fconv );
      system( buffer );
      
      /*--------------------------*/
      /* water surface raster map */
      /*--------------------------*/
      sprintf( mapname, "%s.%s", wsurf_mconv, suffix );
      sprintf( buffer, "g.remove rast=%s >> %s", 
                         mapname, log_fconv );
      system( buffer );

      /*------------------------*/
      /* flood depth raster map */
      /*------------------------*/
      sprintf( mapname, "%s.%s", depth_mconv, suffix );
      sprintf( buffer, "g.remove rast=%s >> %s", 
                         mapname, log_fconv );
      system( buffer );

      /*-------------------*/
      /* damage vector map */
      /*-------------------*/
      sprintf( mapname, "%s.%s", damage_mconv, suffix );
      sprintf( buffer, "g.remove vect=%s >> %s", 
                         mapname, log_fconv );
      system( buffer );

   }

   printf("    Fclean finished.\n" );
}
