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
int      dollar_brief;

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
double **str_damage;
double **con_damage;
double **dam_depth;

FILE    *damcrv_fid;
FILE    *damage_report_fid;
FILE    *depth_report_fid;

FILE    *build_fid;
FILE    *build_aid;

M_INFO   build_mid;
L_PNTS  *build_points;
/*==========================================================================*/
main( argc, argv )
   int    argc;
   char **argv;
{

   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   do_init( argv[0] );
   dollar_brief = FALSE;
 
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

   /*------------------------------------*/
   /* Calculate and report flood details */
   /*------------------------------------*/
   if ( !do_detail() )
      exit(1);
   
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
   char  damage_report_fname[FNAMELEN];
   char  depth_report_fname[FNAMELEN];
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
      printf("    Enter mapname of building sites: ");
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
   mapset = G_find_file( felement, fdetail_ctrl, "" );
   if ( mapset )
      nevents = read_events( mapset, fdetail_ctrl,
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
   /* Open output report files */
   /*--------------------------*/
   strcpy( damage_report_fname, damage_report_fconv );
   damage_report_fid  = fopen( damage_report_fname, "w" );
   if ( !damage_report_fid )
   {
      printf("    WARNING: cannot open output report file.\n" );
      printf("             Detailed damage report will be written to the screen.\n" );
      damage_report_fid = stdout;
   }

   strcpy( depth_report_fname, depth_report_fconv );
   depth_report_fid  = fopen( depth_report_fname, "w" );
   if ( !depth_report_fid )
   {
      printf("    WARNING: cannot open output report file.\n" );
      printf("             Detailed depth report will be written to the screen.\n" );
      depth_report_fid = stdout;
   }

   return(1);
}

/*==========================================================================*/
get_memory()
{
   int mem_err = 0;

   if ( !(str_damage = (double **) calloc( 1, sizeof(double *) ) ) ) 
      mem_err++;
   if ( !(con_damage = (double **) calloc( 1, sizeof(double *) ) ) ) 
      mem_err++;
   if ( !(dam_depth = (double **) calloc( 1, sizeof(double *) ) ) ) 
      mem_err++;

   if ( !(str_damage[0] = (double *) calloc( nevents+1, sizeof(double) ) ) ) 
      mem_err++;
   if ( !(con_damage[0] = (double *) calloc( nevents+1, sizeof(double) ) ) ) 
      mem_err++;
   if ( !(dam_depth[0] = (double *) calloc( nevents, sizeof(double) ) ) ) 
      mem_err++;

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
   int    offset;
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
do_detail()
{
   int     rc;
   int     event;
   int     flooding;

   int     prop_cat;
   char    str_crv[CRVLEN];
   char    con_crv[CRVLEN];
   double  str_val;
   double  con_val;
   double  str_pct;
   double  con_pct;
   double  first_floor;
   double  sdamage;
   double  cdamage;

   int     build_row;
   int     build_col;
   int     build_num;

   char    wsurf_map[FNAMELEN];
   int     wsurf_mid;
   char    depth_map[FNAMELEN];
   int     depth_mid;

   double *north;
   double *east;

   double  wsurf_elev;
   double  flood_depth;
   double  damage_depth;

   CELL   *row_buffer;
   C_HEAD  window;

   char    report_title[LINELEN];
   char    table_title[LINELEN];
   char    suffix[SUFFIXLEN];
   char    att_buffer[BUFFLEN];
   char   *mapset;

   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   G_get_window( &window );
   row_buffer = G_allocate_cell_buf();

   /*-----------------------------*/
   /* Setup detailed report files */
   /*-----------------------------*/
   strcpy( report_title, "DETAILED DAMAGE ESTIMATES" ); 
   strcpy( table_title,  "BUILDING" );
   report_setup( damage_report_fid, report_title, table_title, DOLLAR );

   strcpy( report_title,  "RELATIVE FLOOD DEPTHS" ); 
   report_setup( depth_report_fid, report_title, table_title, ONEDEC );

   /*--------------------------------*/
   /* For every building location... */
   /*--------------------------------*/
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

      /*-----------------------*/
      /* For every flood event */
      /*-----------------------*/
      for ( event=0; event < nevents; event++ )
      {
         /*---------------------------------------------*/
         /* Open water surface elevation and depth maps */
         /*---------------------------------------------*/
         map_suffix( event_list[event], suffix );
         sprintf( wsurf_map, "%s.%s", wsurf_mconv, suffix );
         mapset = G_find_cell( wsurf_map, "" );
         if ( !mapset ) 
         { 
             printf ( "    ERROR: cannot locate water surface map '%s'.\n", 
                                  wsurf_map);
             return(0); 
         } 
 
         wsurf_mid = G_open_cell_old( wsurf_map, mapset );
         if ( wsurf_mid < 0 )
         {   
             printf ( "   ERROR: cannot open water surface map '%s'.\n", 
                                 wsurf_map);
             return(0);
         }  

         sprintf( depth_map, "%s.%s", depth_mconv, suffix );
         depth_mid = G_open_cell_old( depth_map, mapset );
         if ( depth_mid < 0 )
         {   
             printf ( "   ERROR: cannot open depth map '%s'.\n", 
                                 depth_map);
             return(0);
         }  

         /*--------------------------------------*/
         /* See if building is in the floodplain */
         /*--------------------------------------*/
         G_get_map_row( depth_mid, row_buffer, build_row );
         flood_depth  = row_buffer[build_col] / 10.0;
         if ( flood_depth <= 0.0 )
         {
            flooding = FALSE;
            damage_depth = NOVALUE;
         }
         else
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
         }

         /*-------------------------------------*/
         /* Store damage estimates for building */
         /*-------------------------------------*/
         if ( flooding )
         {
            sdamage = str_pct * str_val;
            cdamage = con_pct * con_val;
         }
         else
         {
            sdamage = 0.0; 
            cdamage = 0.0; 
         }

         str_damage[0][event] = sdamage;
         con_damage[0][event] = cdamage;
         dam_depth[0][event]  = damage_depth;

         /*---------------------------------*/
         /* Close maps/files for this event */
         /*---------------------------------*/
         close( wsurf_mid );
         close( depth_mid );
      }

      /*----------------------------------------------*/
      /* Write building calculations to detail report */
      /*----------------------------------------------*/
      calc_ead();
      report_detail( build_num, prop_cat, first_floor );
   }

   /*-----------*/
   /* Finish up */
   /*-----------*/
   report_notes();

   return(1);
}

/*==========================================================================*/
calc_ead()
{
   int    i;
   int    ead_indx;
   double ead;
 
   /*---------------------------------------------*/
   /* Calculate expected annual for this building */
   /*---------------------------------------------*/
   ead_indx = nevents;
   str_damage[0][ead_indx] = 0.0;
   con_damage[0][ead_indx] = 0.0;

   for( i=0; i < nevents-1; i++ )
   {
      /*----------------------------*/
      /* EAD for structural damages */
      /*----------------------------*/
      ead = ((str_damage[0][i] +
              str_damage[0][i+1])/2) *
             (prob[i]-prob[i+1]);
      str_damage[0][ead_indx] += ead;
 
      /*-------------------------*/
      /* EAD for content damages */
      /*-------------------------*/
      ead = ((str_damage[0][i] +
              str_damage[0][i+1])/2) *
             (prob[i]-prob[i+1]);
      str_damage[0][ead_indx] += ead;

      /*-------------------------*/
      /* EAD for content damages */
      /*-------------------------*/
      ead = ((con_damage[0][i] +
              con_damage[0][i+1])/2) *
             (prob[i]-prob[i+1]);
      con_damage[0][ead_indx] += ead;
   }
   /*------------------------------------------------*/
   /* The last event is handled a little differently */
   /*------------------------------------------------*/
   ead = str_damage[0][i] * prob[i];
   str_damage[0][ead_indx] += ead;
   ead = con_damage[0][i] * prob[i];
   con_damage[0][ead_indx] += ead;
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

      /*-------------------------------*/
      /* Get finished floor elevation  */
      /*-------------------------------*/
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
report_setup( report_fid, report_title, table_title, fmt_type )
   FILE *report_fid;
   char *report_title;
   char *table_title;
   int   fmt_type;
{
   int     i;
   int     ncolumns;
   int     offset;
   char    buffer[BUFFLEN];
   char    title[BUFFLEN];
   char    heading[BUFFLEN];
   char    token[WORDLEN];

   /*--------------------*/
   /* Print report title */
   /*--------------------*/
   blank_pad( title, BUFFLEN );
   sprintf( buffer, "%s -- %s (%s)\n", 
                     report_title, G_location(), f_mapset );
   offset = (LINELEN - strlen(buffer) ) / 2;
   strcpy( title+offset, buffer );
   fprintf( report_fid, title );

   /*------------------------------------------------------*/
   /* Differentiate between count and damage table formats */
   /* Damage tables have an extra column for EAD values    */
   /*------------------------------------------------------*/
   if ( fmt_type == DOLLAR )
      ncolumns = nevents + 1;
   else
      ncolumns = nevents;

   /*----------------------------------------*/
   /* Format table title and column headings */
   /*----------------------------------------*/
   fprintf( report_fid, "\n\n\n" );
   r_justify( token, table_title, TAB_INCREMENT );
   strcpy( buffer, token );

   if ( report_fid == damage_report_fid )
      strcpy( heading, "CATEGORY" );
   else if ( report_fid == depth_report_fid )
      strcpy( heading, "F. FLOOR" );

   r_justify( token, heading, TAB_INCREMENT );
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
   fprintf( report_fid, buffer );
   fprintf( report_fid, "\n" );
}

/*==========================================================================*/
report_detail( build_id, prop_cat, first_floor )
   int     build_id;
   int     prop_cat;
   double  first_floor;
{
   char  row_title[WORDLEN];

   /*----------------------------------------*/
   /* See if our numbers have gotten too big */
   /* to fit into the reports                */
   /*----------------------------------------*/
   check_values();

   /*------------------------------*/
   /* Print structure damage table */
   /*------------------------------*/
   sprintf( row_title, "%d", build_id );
   row_fmt( damage_report_fid, row_title, DOLLAR, dollar_brief, 
            prop_cat, first_floor, str_damage, con_damage );

   row_fmt( depth_report_fid, row_title, ONEDEC, FALSE, 
            prop_cat, first_floor, dam_depth, NULL );
}

/*==========================================================================*/
report_notes()
{
   /*---------------------*/
   /* Print special notes */
   /*---------------------*/
   fprintf( damage_report_fid, "\n\n\n\n*** NOTES ***\n\n" ); 
   if ( spf_period )
      fprintf( damage_report_fid, "SPF recurrence interval = %d years\n", 
               spf_period );
   fclose( damage_report_fid );
   print_date( damage_report_fconv );

   fprintf( depth_report_fid, "\n\n\n\n*** NOTES ***\n\n" ); 
   if ( spf_period )
      fprintf( depth_report_fid, "SPF recurrence interval = %d years\n", 
               spf_period );
   fclose( depth_report_fid );
   print_date( depth_report_fconv );
}

/*==========================================================================*/
check_values()
{
   int event;

   for ( event=0; event < nevents; event++ )
   {
      if ( str_damage[0][event] + 
           con_damage[0][event]  >= BIG_VALUE )
         dollar_brief = TRUE;
   }
}

/*==========================================================================*/
row_fmt( report_fid, row_title, fmt_type, brief, prop_cat, first_floor, 
         table1, table2 )
   FILE    *report_fid;
   char    *row_title;
   int      fmt_type;
   int      brief;
   int      prop_cat;
   double   first_floor;
   double **table1;
   double **table2;
{
   int   event;
   int   ncolumns;
   char  token[WORDLEN];
   char  amount[WORDLEN];
   char  buffer[BUFFLEN];
   char *prop_cat_str[NPROPCATS]; 
   char *ffloor_str[WORDLEN]; 

   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   prop_cat_str[0] = "SFR";
   prop_cat_str[1] = "MFR";
   prop_cat_str[2] = "MOB";
   prop_cat_str[3] = "C";
   prop_cat_str[4] = "P";
   prop_cat_str[5] = "POV";

   /*------------------------------------------------------*/
   /* Differentiate between count and damage table formats */
   /* Damage tables have an extra column for EAD values    */
   /*------------------------------------------------------*/
   if ( fmt_type == DOLLAR )
      ncolumns = nevents + 1;
   else
      ncolumns = nevents;

      /*---------------------*/
      /* Format row headings */
      /*---------------------*/
      fprintf( report_fid, "\n" );
      r_justify( token, row_title, TAB_INCREMENT );
      strcpy( buffer, token );

      if ( report_fid == damage_report_fid )
      {
         /*--------------------------------*/
         /* Right-justify property category*/
         /*--------------------------------*/
         r_justify( token, prop_cat_str[prop_cat], TAB_INCREMENT );
         strcat( buffer, token );
      }

      else if ( report_fid == depth_report_fid )
      {
         /*--------------------------------*/
         /* Right-justify ffloor elevation */
         /*--------------------------------*/
         sprintf( ffloor_str, "%8.2f", first_floor );
         r_justify( token, ffloor_str, TAB_INCREMENT );
         strcat( buffer, token );
      }

/*-----
      if ( brief )
         fprintf( report_fid, "  (in thousands)\n" );
-----*/

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
            value_fmt( table1[0][event], amount, fmt_type, brief );
         else
            value_fmt( table1[0][event]+table2[0][event], 
                       amount, fmt_type, brief );

         /*--------------------------------*/
         /* Rigth-justify formatted string */
         /*--------------------------------*/
         r_justify( token, amount, TAB_INCREMENT );
         strcat( buffer, token );
      }
      fprintf( report_fid, buffer );
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
