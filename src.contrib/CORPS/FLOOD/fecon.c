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

/*------------------------*/
/* Global data structures */
/*------------------------*/
int      count_brief;
int      dollar_brief;
int      area_brief[NAREAUNITS];

int      nevents;
char   **event_list;
double  *prob;
int      spf_period;
 
int      ncrv;
char   **crv;

int      ndepth;
int     *depth_orig;
double  *depth_cats;

double **pct;
double **cnt_damage;
double **str_damage;
double **con_damage;
double **area_damage;

FILE    *damcrv_fid;
FILE    *summary_fid;

FILE    *build_fid;
FILE    *build_aid;

M_INFO   build_mid;
M_INFO   damage_mid;
L_PNTS  *build_points;

/*==========================================================================*/
main( argc, argv )
   int    argc;
   char **argv;
{
   int event;

   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   do_init( argv[0] );
 
   /*---------------------------*/
   /* Find and open input files */
   /*---------------------------*/
   if ( !get_input() )
      exit(1);

   /*-----------------------------*/
   /* Load depth-damage curves... */
   /*-----------------------------*/
   ndepth = load_depths( damcrv_fid );
   if ( !ndepth )
      exit(1);

   ncrv = load_data( damcrv_fid );
   if ( !ncrv )
      exit(1);

   /*-----------------------------------------*/
   /* Allocate memory for flood damage tables */
   /*-----------------------------------------*/
   if ( !get_memory() )
      exit(1);

   /*--------------------------------------------------*/
   /* Calculate flood damage assessment for each event */
   /*--------------------------------------------------*/
   for ( event=0; event < nevents; event++ )
      if ( calc_damage( event ) )
         /*----------------------------------------*/
         /* See if our numbers have gotten too big */
         /* to fit into the summary report         */
         /*----------------------------------------*/
         check_values( event );
      else
         exit(1);
   
   /*-----------------------------------*/
   /* Calculate expected annual damages */
   /*-----------------------------------*/
   calc_ead();

   /*--------------------------------*/
   /* Report flood damage assessment */
   /*--------------------------------*/
   summary_damage();

   /*----------*/
   /* Clean up */
   /*----------*/
   Vect_destroy_line_struct( build_points );
   printf( "    Done.\n" );

}

/*==========================================================================*/
get_input()
{
   int   i;
   int   nprob;
   int   valid;
   int   found;
   int   moredata;
   int   index;
   int   period;
   int   answer;
   char  period_str[SUFFIXLEN];
   char  build_map[FNAMELEN];
   char  build_fname[FNAMELEN];
   char  damcrv_fname[FNAMELEN];
   char  summary_fname[FNAMELEN];
   char  token[WORDLEN];
   char  buffer[LINELEN];
   char *mapset;
   char *gotline;
   FILE *ctrl_fid = NULL;

   /*-------------------------------*/
   /* Get map of building locations */
   /*-------------------------------*/
   mapset = G_find_vector( build_loc_mconv, "" );
   if ( mapset )
      strcpy( build_map, build_loc_mconv );
   else
   {
      printf("    Enter vector mapname of building locations: ");
      scanf( "%s", build_map );
      mapset = G_find_vector( build_map, "" );
      if ( mapset == NULL )
      {
          printf ( "    ERROR: building map '%s' not found.\n", build_map);
          return(0);
      }    
   }
   if ( ( Vect_open_old( &build_mid, build_map, mapset ) ) < 0 )
   {  
       printf ( "   ERROR: cannot open building map '%s'.\n", build_map);
       return(0);
   }    
   Vect_set_constraint_type( &build_mid, DOT );
   build_points = Vect_new_line_struct();

   /*----------------------------------*/
   /* Open building map attribute file */
   /*----------------------------------*/
   if ((build_aid = G_fopen_old( "dig_att", build_map, mapset )) == NULL)
   {
      printf ("    ERROR: No attribute file for building map '%s'.\n",
                   build_map );
      return(0);
   }

   /*---------------------------------*/
   /* Get ascii file of building data */
   /*---------------------------------*/
   mapset = G_find_file( felement, build_info_fconv, "" );
   if ( mapset )
      G__file_name( build_fname, felement, build_info_fconv, mapset );
   else
   {
      printf("    %s\n    %s: ", 
             "Enter filename (full path) containing building data",
             "(id's, values, curve numbers, etc.)" );
      scanf( "%s", build_fname );
   }
   build_fid = fopen( build_fname, "r" );
   if ( !build_fid )
   {
      printf("    ERROR: cannot open structure data file '%s'.\n", 
                         build_fname );
      return(0);
   }

   /*---------------------------------------*/
   /* Get ascii file of depth-damage curves */
   /*---------------------------------------*/
   mapset = G_find_file( felement, damcrv_fconv, "" );
   if ( mapset )
      G__file_name( damcrv_fname, felement, damcrv_fconv, mapset );
   else
   {
      printf("    Enter filename (full path) for depth-damage curves: ");
      scanf( "%s", damcrv_fname );
   }
   damcrv_fid  = fopen( damcrv_fname, "r" );
   if ( !damcrv_fid ) 
   {
      printf("    ERROR: cannot open depth-damage file '%s'.\n", 
                         damcrv_fname );
      return(0);
   }

   /*--------------------------------------------------------------*/
   /* Get flood events to be processed -- either from control file */
   /*                                     or from user response    */
   /*--------------------------------------------------------------*/
   mapset = G_find_file( felement, fecon_ctrl, "" );
   if ( mapset )
      nevents = read_events( mapset, fecon_ctrl,
                             &event_list, &ctrl_fid );
   else
      nevents = prompt_events( "cell", depth_mconv, &event_list );

   if ( !nevents )
   {
      printf("    ERROR: no flood events available for processing.\n" );
      return(0);
   }
   ssort( nevents, event_list );
   list_events( nevents, event_list );

   /*-------------------------------*/
   /* Determine event probabilities */
   /*-------------------------------*/
   nprob = nevents;
   prob  = (double *) malloc( nprob * sizeof(double) );
   index = get_sindex( nevents, event_list, "spf" );
   if ( index == -1 )
      spf_period = 0;
   else
   {
      /*---------------------------------------*/
      /* Get recurrence interval for SPF event */
      /*---------------------------------------*/
      found = FALSE;
      if ( ctrl_fid )
      {
         /*--------------------------------------------*/
         /* Read recurrence interval from control file */
         /*--------------------------------------------*/
         moredata = TRUE;
         while ( moredata  &&  !found )
         {
            /*--------------------------------------------*/
            /* keep reading until we get to an assignment */
            /*--------------------------------------------*/
            gotline = fgets( buffer, LINELEN, ctrl_fid );
            while ( sscanf( buffer, " %s = %d", token, &answer ) != 2   &&  gotline )
               gotline = fgets( buffer, LINELEN, ctrl_fid );
 
            /*--------------------------------------------*/
            /* process assignment  -- see if it is an SPF */
            /* recurrence interval designation            */
            /*--------------------------------------------*/
            if ( gotline )
            {
               if( !strcmp( token, "SPF" )  ||  !strcmp( token, "spf" ) )
               {
                  spf_period = answer;
                  found  = TRUE;
                  printf("    NOTE: spf recurrence interval = %d.\n", 
                                    spf_period );
               }
            }
            else
               moredata = FALSE;
         }
         fclose( ctrl_fid );
      }

      if ( !found )
      {
         /*---------------------------*/
         /* Prompt user interactively */
         /*---------------------------*/
         valid = FALSE;
         while ( !valid )
         {
            printf("    Enter recurrence interval (years) for SPF flood event: ");
            scanf( "%s", period_str );
            if ( sscanf( period_str, "%d", &spf_period ) == 1 )
               valid = TRUE;
            else
               printf( "    Invalid period specification. Try again.\n" );
         }
      }
      prob[index] = 1.0/spf_period;
      nprob--;
   }

   for( i=0; i < nprob; i++ )
   {
      /*-------------------------------------------------------------*/
      /* Note that this loop assumes that the event list has been    */
      /* sorted such that the spf event will appear last in the list */
      /*-------------------------------------------------------------*/
      sscanf( event_list[i], "%d", &period );
      prob[i] = 1.0/period;
   }

   /*--------------------------*/
   /* Open summary output file */
   /*--------------------------*/
   strcpy( summary_fname, summary_report_fconv );
   summary_fid  = fopen( summary_fname, "w" );
   if ( !summary_fid )
   {
      printf("    WARNING: cannot open summary output file.\n" );
      printf("             Damage summary will be written to stdout.\n" );
      summary_fid = stdout;
   }

   /*----------------------------------------*/
   /* Set up header for building damage maps */
   /*----------------------------------------*/
   Vect_copy_head_data( &build_mid.head, &damage_mid.head );
   strcpy (damage_mid.head.map_name, "Output from f.econ");
   strcpy (damage_mid.head.your_name, G_whoami ());

   return(1);
}

/*==========================================================================*/
get_memory()
{
   int i;
   int mem_err = 0;

   if ( !(cnt_damage = (double **) calloc( NPROPCATS, sizeof(double *) ) ) ) 
      mem_err++;
   if ( !(str_damage = (double **) calloc( NPROPCATS, sizeof(double *) ) ) ) 
      mem_err++;
   if ( !(con_damage = (double **) calloc( NPROPCATS, sizeof(double *) ) ) ) 
      mem_err++;
   if ( !(area_damage = (double **) calloc( NAREAUNITS, sizeof(double *) ) ) ) 
      mem_err++;

   for ( i=0; i < NPROPCATS  &&  !mem_err; i++ )
   {
      if ( !(cnt_damage[i] = (double *) calloc( nevents, sizeof(double) ) ) ) 
         mem_err++;
      if ( !(str_damage[i] = (double *) calloc( nevents+1, sizeof(double) ) ) ) 
         mem_err++;
      if ( !(con_damage[i] = (double *) calloc( nevents+1, sizeof(double) ) ) ) 
         mem_err++;
   }

   for ( i=0; i < NAREAUNITS  &&  !mem_err; i++ )
   {
      if ( !(area_damage[i] = (double *) calloc( nevents+1, sizeof(double) ) ) ) 
         mem_err++;
   }

   if ( mem_err )
       mem_exit();
   return(1);
}

/*==========================================================================*/
int load_depths( fid )
   FILE  *fid;
{
   int    i;
   int    count;
   int    norig;
   int    max_depth;
   double depth;
   double curr_depth;
   double next_depth;
   char   buffer[BUFFLEN];
   char   answer[1];

   printf("    Scanning depth-damage file...\n" );

#ifdef VERBOSE
   printf("    scanning for depth categories...\n");
#endif

   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   count = 0;
   norig = 0;

   /*--------------------------------------*/
   /* Allocate memory for depth categories */
   /*--------------------------------------*/
   max_depth = DEPTH_INCREMENT;
   depth_cats = (double *) calloc( max_depth, sizeof(double) );
   depth_orig = (int *) calloc( max_depth, sizeof(int) );

   if ( !depth_cats  ||  !depth_orig )
      mem_exit();

   /*-------------------------------------------------------------*/
   /* Read depth-damage file until we get to the depth categories */
   /*-------------------------------------------------------------*/
   if ( !non_blank_line( fid, buffer ) )
      fmt_exit();

   /*------------------------------------------------*/
   /* For every adjacent pair of depth categories... */
   /*------------------------------------------------*/
   next_value( buffer, &curr_depth );
   while ( next_value( NULL, &next_depth ) )
   {
      /*----------------------------------------------*/
      /* ...interpolate to depth of 0.1 ft accurracy */
      /*----------------------------------------------*/
      for ( depth=curr_depth; depth < next_depth; depth+=0.1 )
      {
	 /*???????????????????????????????????*/
	 /* !!! round off can be a problem!!! */
	 /*???????????????????????????????????*/
	 if ( next_depth - depth < 0.01 ) continue;

	 depth_cats[count] = depth;

	 /*------------------------------------------------------*/
	 /* make note if depth is original or interpolated value */
	 /*------------------------------------------------------*/
	 if ( depth == curr_depth )
	 {
	    depth_orig[count] = TRUE;
	    norig++;
	 }
	 else
	    depth_orig[count] = FALSE;

	 count++;

	 /*------------------------------------------------------------*/
	 /* See if we need to allocate more space for depth categories */
	 /*------------------------------------------------------------*/
	 if ( count == max_depth )
	 {
	    max_depth += DEPTH_INCREMENT;
	    depth_cats = (double *) realloc( (char *) depth_cats,
					    max_depth * sizeof(double) );
	    depth_orig = (int *) realloc( (char *) depth_orig,
					  max_depth * sizeof(int) );

#ifdef VERBOSE
	    printf("    reallocated depth arrays to %d elements \n",
			max_depth );
#endif
	    if ( !depth_cats  ||  !depth_orig )
	       mem_exit();
	 }
      }
      curr_depth = next_depth;
   }
   /*----------------------------------*/
   /* Don't forget last depth category */
   /*----------------------------------*/
   depth_cats[count] = curr_depth;
   depth_orig[count] = TRUE;
   count++;
   norig++;

#ifdef VERBOSE
   printf( "    Do you want to see the depth categories? " );
   scanf("%1s", answer );
   if ( answer[0] == 'y'  ||  answer[0] == 'Y' )
   {
      for ( i=0; i<count; i++ )
	 printf("    depth[%d] = %5.1f  \torig = %d\n",
		     i, depth_cats[i], depth_orig[i] );
      printf("    no. original = %d\n", norig );
   }
#endif

   return( count );
}

/*==========================================================================*/
int load_data( fid )
   FILE    *fid;
{
   char   buffer[BUFFLEN];
   char   answer[1];
   int    moredata;
   int    i;
   int    index;
   int    count;
   int    max_crv;
   int    d_cat;
   double curr_pct;
   double next_pct;
   double delta;

   /*-------------------------------------*/
   /* Allocate memory for curve numbers   */
   /* and depth-damage percentage table   */
   /*-------------------------------------*/
   max_crv   = CRV_INCREMENT;
   crv = (char **) calloc( max_crv, sizeof(char*) );
   pct       = (double **) calloc( max_crv, sizeof(double*) );
   if ( !crv  ||  !pct )
      mem_exit();

   count = 0;
   moredata = non_blank_line( fid, buffer );
   while( moredata )
   {
      /*-------------------*/
      /* Load curve number */
      /*-------------------*/
      crv[count] = (char *) malloc( CRVLEN );
      if ( !crv[count] )
	 mem_exit();

      get_token( buffer, STRING, 1, 3, crv[count], NULL );
#ifdef VERBOSE
      printf("    curve number = %s\n", crv[count] );
#endif

      /*-------------------------------------------------------*/
      /* Allocate memory for curve number's damage percentages */
      /*-------------------------------------------------------*/
      pct[count] = (double *) calloc( ndepth, sizeof(double) );
      if ( !pct[count] )
	 mem_exit();

      /*------------------------------------------------*/
      /* For every adjacent pair of depth categories... */
      /*------------------------------------------------*/
      d_cat = 0;
      next_value( buffer+DD_OFFSET, &curr_pct );
      curr_pct = curr_pct / 100.0;
      while ( next_value( NULL, &next_pct ) )
      {
	 /*---------------------------------*/
	 /* Store current damage percentage */
	 /*---------------------------------*/
         next_pct = next_pct / 100.0;
	 pct[count][d_cat] = curr_pct;
	 d_cat++;

	 /*-------------------------------------------------------*/
	 /* See if we need to interpolate next damage percentages */
	 /*-------------------------------------------------------*/
	 if ( !depth_orig[d_cat] )
	 {
	    /*-----------------------------------------*/
	    /* interpolate to damage of 0.1% accurracy */
	    /*-----------------------------------------*/
	    for( index=0; !depth_orig[d_cat+index]; index++ );
	    delta = (next_pct - curr_pct) / (index + 1);
	    for( i=0; i<index; i++ )
	    {
	       pct[count][d_cat] = pct[count][d_cat-1] + delta;
	       d_cat++;
	    }
	 }
#ifdef VERBOSE
	 else
	    printf("    not interpolating for depth category = %d\n",
			d_cat );
#endif
	 curr_pct = next_pct;
      }

      /*----------------------------------*/
      /* Don't forget last depth category */
      /*----------------------------------*/
      pct[count][d_cat] = curr_pct / 100.0;

/*
#ifdef VERBOSE
      printf( "    Do you want to see the damage percentages? " );
      scanf("%1s", answer );
      if ( answer[0] == 'y'  ||  answer[0] == 'Y' )
      {
	 for ( i=0; i<d_cat; i++ )
	    printf("    depth[%d] = %5.1f  \torig = %d  \tpct = %5.3f\n",
			i, depth_cats[i], depth_orig[i], pct[count][i] );
      }
#endif
*/

      /*----------------------------------*/
      /* Get record for next curve number */
      /*----------------------------------*/
      moredata = non_blank_line( fid, buffer );
      count++;

      /*---------------------------------------------------------*/
      /* See if we need to allocate more space for curve numbers */
      /*---------------------------------------------------------*/
      if ( moredata  &&  count == max_crv )
      {
	 max_crv += CRV_INCREMENT;
	 crv = (char **) realloc( (char **) crv,
                                   max_crv * sizeof(char*) );
	 pct = (double **) realloc( (char *) pct,
                                    max_crv * sizeof(double*) );

#ifdef VERBOSE
	 printf("    reallocated curve number arrays to %d elements \n", max_crv );
#endif
	 if ( !crv  ||  !pct )
	    mem_exit();
      }
   }
   return( count );
}

/*==========================================================================*/
calc_damage( event )
   int     event;
{
   int      rc;
   int      nvect;
   int      flooding;

   int      prop_cat;
   char     str_crv[CRVLEN];
   char     con_crv[CRVLEN];
   double   str_val;
   double   con_val;
   double   str_pct;
   double   con_pct;
   double   first_floor;
   double   sdamage;
   double   cdamage;

   int      build_row;
   int      build_col;
   int      build_num;
   char     build_num_str[WORDLEN];
   double   build_damage;
   char     build_damage_str[WORDLEN];

   char     depth_map[FNAMELEN];
   char     wsurf_map[FNAMELEN];
   char     damage_map[FNAMELEN];
   char     damage_att[FNAMELEN];

   int      depth_mid;
   int      wsurf_mid;
   FILE    *damage_aid;

   double  *north;
   double  *east;

   double   wsurf_elev;
   double   flood_depth;
   double   damage_depth;

   CELL    *row_buffer;
   C_HEAD   window;

   char     suffix[SUFFIXLEN];
   char     path[PATHLEN];
   char     att_buffer[BUFFLEN];
   char    *buff_ptr;

   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   printf("    Calculating damages for %s-year event...\n",
               event_list[event] );
   G_get_window( &window );
   row_buffer = G_allocate_cell_buf();
 
   /*----------------------*/
   /* Open flood depth map */
   /*----------------------*/
   map_suffix( event_list[event], suffix );
   sprintf( depth_map, "%s.%s", depth_mconv, suffix );
   depth_mid = G_open_cell_old( depth_map, f_mapset );
   if ( depth_mid < 0 )
   {   
       printf ( "   ERROR: cannot open depth map '%s'.\n", depth_map);
       return(0);
   }  

   /*----------------------------------*/
   /* Open water surface elevation map */
   /*----------------------------------*/
   sprintf( wsurf_map, "%s.%s", wsurf_mconv, suffix );
   wsurf_mid = G_open_cell_old( wsurf_map, f_mapset );
   if ( wsurf_mid < 0 )
   {   
       printf ( "   ERROR: cannot open water surface map '%s'.\n", 
                           wsurf_map);
       return(0);
   }  

   /*----------------------------*/
   /* Create building damage map */
   /*----------------------------*/
   sprintf( damage_map, "%s.%s", damage_mconv, suffix );
   if ( ( Vect_open_new( &damage_mid, damage_map ) ) < 0 )
   {  
       printf ( "   ERROR: cannot create building damage map '%s'.\n", 
                           damage_map);
       return(0);
   }    
   if ( !copy_vector( &build_mid, &damage_mid, &nvect ) )
      return(0);

   /*-------------------------------------*/
   /* Open building damage attribute file */
   /*-------------------------------------*/
   G__file_name( path, "", "", f_mapset );
   sprintf (damage_att, "%s/dig_att/%s", path, damage_map);
   if ((damage_aid = fopen (damage_att, "w")) == NULL)
   {
      printf ("    %s '%s'.\n",
              "ERROR: cannot open attribute file for building damage map",
               damage_map );
      return(0);
   }

   /*--------------------------------*/
   /* For every building location... */
   /*--------------------------------*/
   rewind( build_aid );
   while( Vect_read_next_line( &build_mid, build_points ) >= 0 ) 
   {
      /*---------------------------------*/
      /* Get building id i.e., attribute */
      /*---------------------------------*/
      east  = build_points->x;
      north = build_points->y;
      if ( !fgets( att_buffer, BUFFLEN,  build_aid ) )
      {  
         printf( "    %s %s =%12.2f  %s =%12.2f\n",
                      "WARNING: cannot read building id at",
                      "NORTH", north[0], "EAST", east[0] );
         continue;
      }
      sscanf( att_buffer, " %*s %*f %*f %d", &build_num );

      /*-------------------------------*/
      /* Get economic info on building */
      /*-------------------------------*/
      rc = get_econ_info( build_num, &prop_cat, &first_floor,
                          str_crv, &str_val, con_crv, &con_val );
      if ( !rc ) 
      {
         printf( "    WARNING: cannot read economic data for building %d.\n",
                               build_num );
         continue;
      }

      /*-----------------------------*/
      /* Determine building location */
      /*-----------------------------*/
      coord_to_cell( window, north[0], east[0], &build_row, &build_col );

      /*--------------------------------------*/
      /* Get flood depth at building location */
      /* to see if building is in floodplain  */
      /*--------------------------------------*/
/*-----------
      G_get_map_row( depth_mid, row_buffer, build_row );
      flood_depth = row_buffer[build_col] / 10.0;
      if ( flood_depth > 0 )
-----------*/
      {
         /*---------------------------------------------------*/
         /* Calculate damage depth based on first floor elev. */
         /*---------------------------------------------------*/
         G_get_map_row( wsurf_mid, row_buffer, build_row );
         wsurf_elev   = row_buffer[build_col] / 10.0;
         damage_depth = wsurf_elev - first_floor;

         /*--------------------------------------------------*/
         /* Get depth damage percentages on flooded building */
         /*--------------------------------------------------*/
         rc = get_ddcrv( damage_depth, str_crv, &str_pct, 
                                       con_crv, &con_pct );
         switch ( rc ) 
         {
            case -2:
               printf( "    %s %6.1f %s %d.\n",
                            "WARNING: cannot locate depth category",
                            damage_depth, "for building", build_num );
               flooding = FALSE;
               break;

            case -1:
               printf( "    %s %s %d.\n",
                            "WARNING: cannot locate depth-damage curve",
                            "for building", build_num );
               flooding = FALSE;
               break;

            case 0:
               flooding = FALSE;
               break;

            case 1:
               flooding = TRUE;
               break;
         }

         /*------------------------------------------------*/
         /* Cumulate damage estimates for flooded building */
         /*------------------------------------------------*/
         if ( flooding )
         {
            cnt_damage[prop_cat][event]++;
            cnt_damage[CAT_TOT][event]++;

            sdamage = str_pct * str_val;
            cdamage = con_pct * con_val;
            build_damage = sdamage + cdamage;

            str_damage[prop_cat][event] += sdamage;
            str_damage[CAT_TOT][event]  += sdamage;

            con_damage[prop_cat][event] += cdamage;
            con_damage[CAT_TOT][event]  += cdamage;
         }
         else
            build_damage = 0.0;
      }

      /*---------------------------------------------*/
      /* Write build damage to damage attribute file */
      /*---------------------------------------------*/
      sprintf( build_damage_str, "%d", round( build_damage ) );
      sprintf( build_num_str, "%d", build_num );
      buff_ptr = att_buffer + rscan( att_buffer, build_num_str );
      strcpy( buff_ptr, build_damage_str );
      fprintf( damage_aid, "%s\n", att_buffer );
   }

   /*---------------------------------------------------*/
   /* Calculate areal extent of flooding for each event */
   /*---------------------------------------------------*/
   if ( !calc_area( event, depth_mid ) )
      return(0);
   
   /*---------------------------------*/
   /* Close maps/files for this event */
   /*---------------------------------*/
   close( depth_mid );
   close( wsurf_mid );
   fclose( damage_aid );
   Vect_close( &damage_mid );
   support_vector( damage_map );

   return(1);
}

/*==========================================================================*/
calc_ead()
{
   int    i;
   int    ead_indx;
   int    prop_cat;
   double ead;

   /*------------------------------------------------------*/
   /* Calculate expected annual for each property category */
   /*------------------------------------------------------*/
   ead_indx = nevents;
   for( prop_cat=0; prop_cat < NPROPCATS; prop_cat++ )
   {
      for( i=0; i < nevents-1; i++ )
      {
         /*----------------------------*/
         /* EAD for structural damages */
         /*----------------------------*/
         ead = ((str_damage[prop_cat][i] + 
                 str_damage[prop_cat][i+1])/2) * 
                (prob[i]-prob[i+1]);
         str_damage[prop_cat][ead_indx] += ead;

         /*-------------------------*/
         /* EAD for content damages */
         /*-------------------------*/
         ead = ((con_damage[prop_cat][i] + 
                 con_damage[prop_cat][i+1])/2) * 
                (prob[i]-prob[i+1]);
         con_damage[prop_cat][ead_indx] += ead;
      }
      /*------------------------------------------------*/
      /* The last event is handled a little differently */
      /*------------------------------------------------*/
      ead = str_damage[prop_cat][i] * prob[i];
      str_damage[prop_cat][ead_indx] += ead;
      ead = con_damage[prop_cat][i] * prob[i];
      con_damage[prop_cat][ead_indx] += ead;
   }
}

/*==========================================================================*/
get_econ_info( build_num, prop_cat, first_floor, 
               str_crv, str_val, con_crv, con_val )
   int     build_num;
   int    *prop_cat;
   double *first_floor;
   char   *str_crv;
   double *str_val;
   char   *con_crv;
   double *con_val;
{
   char   buffer[BUFFLEN];
   char   token[WORDLEN];
   int    id;
   int    err;
   int    found;
   int    col;
   int    len;
   double gnd_elev;
   double value;

   /*----------------*/
   /* Initialization */
   /*----------------*/
   err   = 0;
   found = 0;
   rewind( build_fid );

   while ( !found )
   {
      if ( !non_blank_line( build_fid, buffer ) )
         return(0);

      /*-----------------------*/
      /* Get property category */
      /*-----------------------*/
      col  = 11;
      len  =  3;
      err += !get_token( buffer, STRING, col, len, token, NULL );
      if ( !err )
         *prop_cat = catindex( token );
  
      /*-----------------------*/
      /* Get ground elevation  */
      /*-----------------------*/
      col  = 14;
      len  =  8;
      err += !get_token( buffer, NUMBER, col, len, NULL, &value );
      if ( !err )
         gnd_elev = value;
  
      /*-----------------------------------------*/
      /* Get finished floor height above ground  */
      /*-----------------------------------------*/
      col  = 22;
      len  =  6;
      err += !get_token( buffer, NUMBER, col, len, NULL, &value );
      if ( !err )
         *first_floor = value;
  
      /*----------------------------------------*/
      /* Adjust first floor to ground elevation */
      /*----------------------------------------*/
      *first_floor += gnd_elev;

      /*----------------------------*/
      /* Get structure damage curve */
      /*----------------------------*/
      col  = 42;
      len  =  3;
      err += !get_token( buffer, STRING, col, len, token, NULL );
      if ( !err )
         strcpy( str_crv, token );
  
      /*---------------------*/
      /* Get structure value */
      /*---------------------*/
      col  = 49;
      len  = 10;
      err += !get_token( buffer, NUMBER, col, len, NULL, &value );
      if ( !err )
         *str_val = value;
  
      /*--------------------------*/
      /* Get content damage curve */
      /*--------------------------*/
      col  = 59;
      len  =  3;
      err += !get_token( buffer, STRING, col, len, token, NULL );
      if ( !err )
         strcpy( con_crv, token );
  
      /*-------------------*/
      /* Get content value */
      /*-------------------*/
      col  = 62;
      len  = 10;
      err += !get_token( buffer, NUMBER, col, len, NULL, &value );
      if ( !err )
         *con_val = value;
  
      /*------------------------*/
      /* Get building id number */
      /*------------------------*/
      col  = 77;
      len  =  4;
      err += !get_token( buffer, NUMBER, col, len, NULL, &value );
      if ( !err )
         id = (int) value;

      /*------------------*/
      /* Check for errors */
      /*------------------*/
      if ( err )
         return(0);

      /*----------------------------------------*/
      /* Check for matching building id numbers */
      /*----------------------------------------*/
      if ( id == build_num )
         found = TRUE;
   }
   return(1);
}

/*==========================================================================*/
get_ddcrv( damage_depth, str_crv, str_pct, con_crv, con_pct )
   double  damage_depth;
   char   *str_crv;
   double *str_pct;
   char   *con_crv;
   double *con_pct;
{
   int     crv_indx;
   int     depth_indx;

   /*-----------------------*/
   /* Get depth table index */
   /*-----------------------*/
   depth_indx = get_dindex( ndepth, depth_cats, damage_depth );
   if ( depth_indx < 0 )
   {
      if ( damage_depth < depth_cats[0] )
         /*--------------------*/
         /* No flooding damage */
         /*--------------------*/
         return(0);

      else if ( damage_depth > depth_cats[ndepth-1] )
         /*-----------------------------------------------------*/
         /* NOTE:  What about depth categories that are greater */
         /*        than those found in the depth-damage curve?  */
         /*-----------------------------------------------------*/
         return(-2);

      else
         /*--------------------------------------------------------*/
         /* This should not happen.  But weird round-off behavior  */
         /* with the depth categories makes this check a good idea */
         /*--------------------------------------------------------*/
         return(-2);
   }

   /*---------------------------------------*/
   /* Get structure depth-damage percentage */
   /*---------------------------------------*/
   crv_indx = get_sindex( ncrv, crv, str_crv );
   if ( crv_indx < 0 )
      return(-1);
   *str_pct = pct[crv_indx][depth_indx];

   /*-------------------------------------*/
   /* Get content depth-damage percentage */
   /*-------------------------------------*/
   crv_indx = get_sindex( ncrv, crv, con_crv );
   if ( crv_indx < 0 )
      return(-1);
   *con_pct = pct[crv_indx][depth_indx];

   /*---------------------------*/
   /* Check for flooding damage */
   /*---------------------------*/
   if ( *str_pct == 0.0  &&  *con_pct == 0.0 )
      return(0);
   else
      return(1);
}

/*==========================================================================*/
calc_area( event, fid )
   int     event;
   int     fid;
{
   int     row;
   int     col;
   int     nrows;
   int     ncols;
   int     cell_count;
   double  area;
   CELL   *cell;
   C_HEAD  window;

   /*--------------------*/
   /* Get map resolution */
   /*--------------------*/
   G_get_window (&window);
   nrows = window.rows;
   ncols = window.cols;

   /*---------------------------*/
   /* Count cells in floodplain */
   /*---------------------------*/
   cell_count = 0;
   cell = G_allocate_cell_buf();    
   for( row = 0; row < nrows; row++ )
   {
      if ( G_get_map_row ( fid, cell, row ) < 0 )
         return(0);
      for ( col = 0; col < ncols; col++ )
         if ( cell[col] )
            cell_count++;
   }

   /*-----------------*/
   /* Calculate areas */
   /*-----------------*/
   area = cell_count * window.ew_res  * window.ns_res;
   area_damage[AREA_ACRE][event] = cvt_area( area, PROJECTION, ACRES );
   area_damage[AREA_SQMI][event] = cvt_area( area, PROJECTION, SQ_MILES );
   area_damage[AREA_SQMT][event] = cvt_area( area, PROJECTION, SQ_METERS );
   area_damage[AREA_HECT][event] = cvt_area( area, PROJECTION, HECTARES );
   return(1);
}

/*==========================================================================*/
summary_damage()
{
   int   offset;
   int   fmt_type;
   char *prop_cat_str[NPROPCATS]; 
   char *area_str[NAREAUNITS]; 
   char  title[LINELEN];
   char  buffer[LINELEN];

   /*----------------*/
   /* Initialization */
   /*----------------*/
   printf( "    Printing damage reports...\n" );
   prop_cat_str[0] = "Residential, Single-Family";
   prop_cat_str[1] = "Residential, Multi-Family";
   prop_cat_str[2] = "Residential, Mobile Home";
   prop_cat_str[3] = "Commercial & Industrial";
   prop_cat_str[4] = "Public";
   prop_cat_str[5] = "Privately Owned Vehicle";
   prop_cat_str[6] = "TOTAL";
   area_str[AREA_ACRE] = "Acres";
   area_str[AREA_SQMI] = "Square Miles";
   area_str[AREA_SQMT] = "Square Meters";
   area_str[AREA_HECT] = "Hectares";

   /*---------------------*/
   /* Print summary title */
   /*---------------------*/
   blank_pad( title, LINELEN );
   sprintf( buffer, "ECONOMIC DAMAGE ESTIMATES -- %s (%s)\n", 
                     G_location(), f_mapset );
   offset = (LINELEN - strlen(buffer) ) / 2;
   strcpy( title+offset, buffer );
   fprintf( summary_fid, title );

   /*----------------------------*/
   /* Print building count table */
   /*----------------------------*/
   fprintf( summary_fid, "\n\n\n" );
   sprintf( title, "NUMBER OF STRUCTURES" );
   if ( count_brief )
      fmt_type = ONEDEC;
   else
      fmt_type = COUNT;
   table_fmt( title, prop_cat_str, NPROPCATS, fmt_type, 
              &count_brief, TBL_FMT, cnt_damage, NULL );

   /*------------------------------*/
   /* Print structure damage table */
   /*------------------------------*/
   fprintf( summary_fid, "\n\n\n" );
   sprintf( title, "DAMAGE TO STRUCTURES" );
   table_fmt( title, prop_cat_str, NPROPCATS, DOLLAR, 
              &dollar_brief, TBL_FMT, str_damage, NULL );

   /*----------------------------*/
   /* Print content damage table */
   /*----------------------------*/
   fprintf( summary_fid, "\n\n\n" );
   sprintf( title, "DAMAGE TO CONTENTS" );
   table_fmt( title, prop_cat_str, NPROPCATS, DOLLAR, 
              &dollar_brief, TBL_FMT, con_damage, NULL );

   /*--------------------------*/
   /* Print total damage table */
   /*--------------------------*/
   fprintf( summary_fid, "\n\n\n" );
   sprintf( title, "TOTAL FLOOD DAMAGE" );
   table_fmt( title, prop_cat_str, NPROPCATS, DOLLAR, 
              &dollar_brief, TBL_FMT, str_damage, con_damage );

   /*------------------------*/
   /* Print area of flooding */
   /*------------------------*/
   fprintf( summary_fid, "\n\n\n" );
   sprintf( title, "AREAL EXTENT OF FLOODING" );
   table_fmt( title, area_str,  NAREAUNITS, ONEDEC, 
              area_brief, ROW_FMT, area_damage, NULL );

   /*---------------------*/
   /* Print special notes */
   /*---------------------*/
   fprintf( summary_fid, "\n\n\n\n*** NOTES ***\n\n" ); 
   if ( spf_period )
      fprintf( summary_fid, "SPF recurrence interval = %d years\n", spf_period );
   fclose( summary_fid );
   print_date( summary_report_fconv );
}

/*==========================================================================*/
check_values( event )
   int event;
{
   int unit;

   if ( str_damage[CAT_TOT][event] + 
        con_damage[CAT_TOT][event]  >= BIG_VALUE )
      dollar_brief = TRUE;
   else
      dollar_brief = FALSE;

   if ( cnt_damage[CAT_TOT][event]  >= BIG_VALUE )
      count_brief = TRUE;
   else
      count_brief = FALSE;

   for( unit=0; unit < NAREAUNITS; unit++ )
      if ( area_damage[unit][event] >= BIG_VALUE )
         area_brief[unit] = TRUE;
      else
         area_brief[unit] = FALSE;
}

/*==========================================================================*/
table_fmt( title, row_title, nrows, fmt_type, brief, fmt_unit, table1, table2 )
   char    *title;
   char   **row_title;
   int      nrows;
   int      fmt_type;
   int     *brief;
   int      fmt_unit;
   double **table1;
   double **table2;
{
   int   i;
   int   row;
   int   row_brief;
   int   event;
   int   offset;
   int   ncolumns;
   int   first_col = 30;
   char  token[WORDLEN];
   char  heading[WORDLEN];
   char  amount[WORDLEN];
   char  buffer[BUFFLEN];

   /*------------------------------------------------------*/
   /* Differentiate between count and damage table formats */
   /* Damage tables have an extra column for EAD values    */
   /*------------------------------------------------------*/
   if ( fmt_type == DOLLAR )
      ncolumns = nevents + 1;
   else
      ncolumns = nevents;

   /*------------------------*/
   /* Format column headings */
   /*------------------------*/
   buffer[0] = '\n';
   strcpy( buffer+1, title );
   offset = first_col - strlen( title );
   blank_pad( token, WORDLEN );
   token[offset] = '\0';
   strcat( buffer, token );
   for( i=0; i < ncolumns; i++ )
   {
      if ( i == nevents )
         strcpy( heading, "EAD" );
      else if ( !strcmp( event_list[i], "spf" ) )
         strcpy( heading, "SPF EVENT" );
      else
         sprintf( heading, "%s YEAR", event_list[i] );

      r_justify( token, heading, TAB_INCREMENT );
      strcat( buffer, token );
   }
   fprintf( summary_fid, buffer );
   fprintf( summary_fid, "\n" );
   if ( fmt_unit == TBL_FMT  && *brief )
      fprintf( summary_fid, "  (in thousands)\n" );

   /*------------*/
   /* Table rows */
   /*------------*/
   for ( row=0; row < nrows; row++ )
   {
      /*-----------------------------------*/
      /* Check for row-specific formatting */
      /*-----------------------------------*/
      if ( fmt_unit == ROW_FMT )
         row_brief = brief[row];
      else
         row_brief = *brief;

      /*---------------------*/
      /* Format row headings */
      /*---------------------*/
      buffer[0] = '\n';
      strcpy( buffer+1, row_title[row] );
      if ( fmt_unit == ROW_FMT  &&  row_brief )
         strcat( buffer, "(in thousands)" );
      offset = first_col - strlen( buffer ) + 1;
      blank_pad( token, WORDLEN );
      token[offset] = '\0';
      strcat( buffer, token );

      /*-------------------------------*/
      /* Flood events as table entries */
      /*-------------------------------*/
      for ( event=0;  event < ncolumns;  event++ )
      {
         /*------------------------------*/
         /* Format value to comma-string */
         /*------------------------------*/
         blank_pad( amount, WORDLEN );
         if ( table2 == NULL )
            value_fmt( table1[row][event], 
                       amount, fmt_type, row_brief );
         else
            value_fmt( table1[row][event]+table2[row][event], 
                       amount, fmt_type, row_brief );

         /*--------------------------------*/
         /* Right-justify formatted string */
         /*--------------------------------*/
         r_justify( token, amount, TAB_INCREMENT );
         strcat( buffer, token );
      }
      fprintf( summary_fid, buffer );
   }
}

/*==========================================================================*/
/* NOTE: this function is set up for scanning an UNKNOWN number of values   */
/*       from a data line.  It must scan past the last data value to reset  */
/*       itself for the next data line!                                     */
/*==========================================================================*/
int next_value( buffer, value )
   char   *buffer;
   double *value;
{
   int          rc;
   char         token[WORDLEN];
   static char *buff_ptr = NULL;

   /*-----------------------------------*/
   /* See if we are starting a new line */
   /*-----------------------------------*/
   if ( !buff_ptr )
      buff_ptr = buffer;

   /*-----------------*/
   /* grab next token */
   /*-----------------*/
   if ( sscanf( buff_ptr, " %s", token ) == 1 )
   {
      /*-----------------------------*/
      /* Make sure token is a number */
      /*-----------------------------*/
      rc = sscanf( token, "%lf", value );
      if ( rc != 1 )
         fmt_exit();
      else
         /*----------------------------------------*/
         /* Move pointer beyond just-grabbed token */
         /*----------------------------------------*/
         buff_ptr += lscan( buff_ptr, token) + strlen( token );
   }
   else
   {
      /*-------------------------------*/
      /* no more data values in buffer */
      /*-------------------------------*/
      buff_ptr = NULL;
      rc = 0;
   }
   return( rc );
}

/*==========================================================================*/
fmt_exit()
{
   printf("    ERROR: unexpected depth-damage table format.\n" );
   exit(1);
}

/*==========================================================================*/
/* NOTE: this function is set up for scanning a token of KNOWN location     */
/*       and length from a buffer.                                          */
/*==========================================================================*/
int get_token( buffer, type, col, len, alphanum, numeric )
   char   *buffer;
   int     type;
   int     col;
   int     len;
   char   *alphanum;
   double *numeric;
{
   char token[WORDLEN];

   /*----------------------*/
   /* grab specified token */
   /*----------------------*/
   strncpy( token, buffer+col-1, len );
   token[len] = '\0';
   
   /*----------------------------------*/
   /* See if numeric token is expected */
   /*----------------------------------*/
   if ( type == NUMBER )
   {
      if ( sscanf( token, "%lf", numeric ) != 1 )
         return(0);
   }
   else
      strcpy( alphanum, token );

   return( 1 );
}

/*==========================================================================*/
