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
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "flood.h"

/*---------*/
/* Globals */
/*---------*/
int      nevents;
char   **event;
int      nsplit;
int      nsecno;
int      nxsect;
int      max_secno;

int      itruncate;
int      splitflow;
FILE    *hec2_fid;
FILE    *split_fid;

int      cwsel_col;
double **cwsel_val;
int      secno_col;
int     *secno_match;
double  *secno_val;

FILE   *In_att; 
FILE   *Out_att; 
M_INFO  In_dig; 
M_INFO  Out_dig; 


main( argc, argv )
   int argc;
   char **argv;
{
   int     i;
   int     nmatches;
   int     extra_secno;
   int     extra_xsect;

   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   do_init( argv[0] );

   /*---------------------------*/
   /* Find and open input files */
   /*---------------------------*/
   if ( !get_input( argc, argv ) )
      exit(1);

   /*--------------------------*/
   /* Process hec2 output file */
   /*--------------------------*/
   if ( !read_hec2( hec2_fid, FALSE ) )
      exit(1);

   /*----------------------------------------------------------*/
   /* Process second hec2 output file for split flow condition */
   /*----------------------------------------------------------*/
   if ( splitflow )
      if ( !read_hec2( split_fid, TRUE ) )
         exit(1);

   /*--------------------------------------*/
   /* Create WSEL map for each flood event */
   /*--------------------------------------*/
   fprintf (stdout,"    Creating cross section maps (%d) of water surface elevations...\n",
               nevents );

   for ( i=0; i < nevents; i++ )
   {
      /*----------------------------------------------*/
      /* Assign CWSEL values to mapped cross sections */
      /*----------------------------------------------*/
      if ( !xsect_map( i, &extra_xsect ) )
         fprintf (stdout, "    ERROR: bad results in processing %s flood event.\n",
                      event[i] );
   }

   /*----------------------------------------------------------*/
   /* Check matches between HEC2 secno's and vector map xsects */
   /*----------------------------------------------------------*/
   nmatches     = 0;
   extra_secno  = 0;
   for ( i=0; secno_val[i]  &&  i < max_secno; i++ )
   {
      if ( secno_match[i] )
         nmatches++;
      else
         extra_secno++;
   }
   if ( splitflow )
      fprintf (stdout,"    NOTE: Number of split flow cross sections   = %d.\n", 
                        nsplit );

   fprintf (stdout,"    NOTE: Total number of HEC2 cross sections   = %d (SECNO).\n", 
                     nsecno );
   fprintf (stdout,"    NOTE: Total number of vector cross sections = %d (XSECT).\n", 
                     nxsect );
   fprintf (stdout,"    NOTE: Number of SECNO/XSECT matches = %d\n", nmatches );
   fprintf (stdout,"    NOTE: Number of extra SECNO's       = %d\n", extra_secno );
   fprintf (stdout,"    NOTE: Number of extra XSECT's       = %d\n", extra_xsect );
   if ( (nmatches+extra_secno!=nsecno) || (nmatches+extra_xsect!=nxsect) )
      fprintf (stdout,"\n    WARNING: %s.\n",
             "Data mismatch between cross section map and HEC2 output" );
   if ( nmatches == 0 )
      fprintf (stdout,"\n    ERROR: %s.\n    %s\n",
             "Severe mismatch!",
             "Check vector cross section map against Hec-2 output." );

   /*----------*/
   /* Clean up */
   /*----------*/
   Vect_close( &In_dig );
   fclose( In_att );
   fprintf (stdout,"    Done.\n" );
}

/*==========================================================================*/
get_input( argc, argv )
   int     argc;
   char  **argv;
{
   char   *mapset;
   char   *xsect_mapset;
   char    xsect_fname[FNAMELEN];
   char    hec2_fname[FNAMELEN];
   char    hec2_fname2[FNAMELEN];

   /*---------------------------*/
   /* Get HEC2 output file name */
   /*---------------------------*/
   mapset = G_find_file( felement, hec2_out_fconv, "" );
   if ( mapset )
      G__file_name( hec2_fname, felement, hec2_out_fconv, mapset );
   else
   { 
      fprintf (stdout,"    Enter HEC2 output file name (full path): ");
      scanf( " %s", hec2_fname );
   }

   /*----------------*/
   /* open HEC2 file */
   /*----------------*/
   if ( (hec2_fid = fopen( hec2_fname, "r" )) == NULL )
   {
      fprintf (stdout,"    ERROR: cannot open HEC2 file '%s'.\n", hec2_fname );
      return(0);
   }

   /*----------------------------*/
   /* Get cross section map name */
   /*----------------------------*/
   xsect_mapset = get_xsect_name( xsect_fname );
   if ( !xsect_mapset )
   {   
       fprintf (stdout, "    ERROR: cross section map '%s' not found.\n",
                     xsect_fname );
       return(0);
   }

   /*-------------------------*/
   /* Get finput control data */
   /*-------------------------*/
   mapset = G_find_file( felement, finput_ctrl, "" );
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
      fprintf (stdout,"    ERROR: no flood events available for processing.\n" );
      return(0);
   }

   /*----------------------------------------------------*/
   /* Open cross section map, with sorting, if necessary */
   /*----------------------------------------------------*/
   if ( !get_xsect_map( xsect_fname, xsect_mapset, &In_dig ) )
          return(0);

   /*----------------------------------------*/
   /* Open cross section attribute (id )file */
   /*----------------------------------------*/
   if ((In_att = G_fopen_old( "dig_att", xsect_fname, xsect_mapset )) == NULL)
   {
      fprintf (stdout,"    ERROR: No attribute file for vector map '%s'.\n", 
                   xsect_fname);
      return(0);
   }

   /*-----------------------------------------------*/
   /* Create reach map from known cross section map */
   /*-----------------------------------------------*/
   if ( !make_reach( &In_dig ) )
   {
       fprintf (stdout,"    ERROR: Cannot identify river reaches.\n" );
       return(0);
   }

   /*---------------------*/
   /* Get split flow file */
   /*---------------------*/
   if ( splitflow )
   {
      mapset = G_find_file( felement, hec2_split_fconv, "" );
      if ( mapset )
         G__file_name( hec2_fname2, felement, hec2_split_fconv, mapset );
      else
      { 
         fprintf (stdout,"    Enter HEC2 split flow file name (full path): ");
         scanf( " %s", hec2_fname2 );
      }
      /*-------------------------------*/
      /* open 2nd split flow HEC2 file */
      /*-------------------------------*/
      if ( (split_fid = fopen( hec2_fname2, "r" )) == NULL )
      {
         fprintf (stdout,"    ERROR: cannot open HEC2 split flow file '%s'.\n", 
                            hec2_fname2 );
         return(0);
      }
   }

   /*-------------------------------*/
   /* Notify user of modeled events */
   /*-------------------------------*/
   list_events( nevents, event );
            
   return(1);
}
/*==========================================================================*/
cl_parse( argc, argv )
   int    argc;
   char **argv;
{
   int valid, count;
   struct { struct Flag *extend, *split; } flag;
   struct { struct Option *event, *sort; } parm;

   parm.event = G_define_option();
   parm.event->key = "event";
   parm.event->key_desc = "interval";
   parm.event->type = TYPE_STRING;
   parm.event->description = "Flood events modeled in HEC-2";
   parm.event->required = YES;
   parm.event->multiple = YES;
   
   parm.sort = G_define_option();
   parm.sort->key = "sort";
   parm.sort->key_desc = "option";
   parm.sort->type = TYPE_STRING;
   parm.sort->description = "Cross section sorting option";
   parm.sort->options = "none,value,vector,user"; 
   parm.sort->answer = "none"; 
   
   flag.extend = G_define_flag ();
   flag.extend->key = 'x';
   flag.extend->description = "Extend HEC-2 SECNO's beyond decimal point";

   flag.split = G_define_flag ();
   flag.split->key = 's';
   flag.split->description = "Process split flow condition";

   if ( G_parser( argc,argv ) )
      exit(1);

   /*------------------------------*/
   /* get the command line answers */
   /*------------------------------*/
   splitflow = flag.split->answer;
   itruncate  = !(flag.extend->answer);
   if ( sort != IGNORE )
      sort = check_sort( parm.sort->answer );

   /*-----------------------*/
   /* set up the event list */
   /*-----------------------*/
   valid = TRUE;
   for( count=0; parm.event->answers[count] != NULL; count++ );
   event = (char **) malloc( count *  sizeof( char *) );
   if ( !event )
      mem_exit();
   for( count=0; parm.event->answers[count] != NULL; count++ )
   {
      if ( !set_event( &(event[count]), parm.event->answers[count] ) )
         valid = FALSE;
   }
   if ( !valid )
   {
      fprintf (stdout, "    ERROR: Invalid flood event specification.\n" );
      return( 0 );
   }
   return( count );
}

/*==========================================================================*/
read_hec2( fid, do_split )
   FILE *fid;
   int   do_split;
{
   char    heading[LINELEN];
   int     prev_nsecno;

   /*------------------------------------*/
   /* scan file for summary output table */
   /*------------------------------------*/
   if ( !find_summary( fid ) )
   {
      fprintf (stdout,"    ERROR: summary table not found in HEC2 file.\n");
      return(0);
   }

   /*---------------------------------*/
   /* scan for summary table headings */
   /*---------------------------------*/
   heading[0] = '\0';
   if ( !find_headings( fid, heading ) )
   {
      fprintf (stdout,"    ERROR: unexpected format of summary table.\n");
      return(0);
   }

   /*---------------------------------------------------*/
   /* Get column positions for two important variables: */
   /*    SECNO - cross section id                       */
   /*    CWSEL - water surface elevation                */
   /*---------------------------------------------------*/
   if ( !find_variables( heading ) )
      return(0);

   /*-------------------------------------------------*/
   /* Get SECNO and CWSEL values for all flood events */
   /*-------------------------------------------------*/
   if ( do_split )
   {
      prev_nsecno = nsecno;
      nsecno = load_split( heading );
      nsplit = nsecno - prev_nsecno;
   }
   else
      nsecno = load_data( heading );

   if ( !nsecno )
      return(0);

   return(1);
}
/*==========================================================================*/
int find_summary( fid )
   FILE *fid;
{
   char buffer[LINELEN];
   int  found;

   found = FALSE;
   while ( fgets( buffer, LINELEN, fid ) && !found )
   {
      if ( !strncmp( buffer,   " SUMMARY PRINTOUT",
		     strlen(   " SUMMARY PRINTOUT" ) ) )
	 found = TRUE;
   }
   return( found );
}

/*==========================================================================*/
int find_headings( fid, heading )
   FILE *fid;
   char *heading;
{
   char buffer[LINELEN];
   char garbage[LINELEN];
   int  found;

   found = FALSE;

   if ( heading[0] == '\0' )
   {
      /*---------------------------------------*/
      /* read headings and next non-blank line */
      /*---------------------------------------*/
      fgets( heading, LINELEN, fid );
      while ( sscanf( heading, " %s", garbage ) != 1 )
	 fgets( heading, LINELEN, fid );
      found = TRUE;
   }
   else
   {
      /*-------------------------------------------------*/
      /* keep reading until we get to the known headings */
      /*-------------------------------------------------*/
      while ( fgets( buffer, LINELEN, fid ) && !found )
      {
	 if ( !strncmp( buffer, heading, strlen( heading ) ) )
	    found = TRUE;
      }
   }
   return( found );
}

/*==========================================================================*/
find_variables( heading )
   char *heading;
{
   int   rc=1;

   secno_col = get_column( heading, "SECNO" );
   cwsel_col = get_column( heading, "CWSEL" );
   if ( secno_col < 0  ||  cwsel_col < 0 )
      rc = 0;

   return( rc );
}

/*==========================================================================*/
int get_column( buffer, title )
   char *buffer;
   char *title;
{
   char *buff_ptr;
   char  word[WORDLEN];
   int   column;
   int   found;
   int   rc;

   /*--------------------------------------------------------*/
   /* scan summary table headings for specified column title */
   /*--------------------------------------------------------*/
   column   = 0;
   found    = FALSE;
   buff_ptr = buffer;

   while ( sscanf( buff_ptr, " %s", word ) == 1  &&  !found )
   {
	 column++;
	 if ( !strcmp( word, title ) )
	    found = TRUE;
	 else
	    buff_ptr += lscan( buff_ptr, word) + strlen( word );
   }

   if ( !found )
   {
      fprintf (stdout,"    ERROR: column heading %s not in summary table.\n",
			 title );
      rc = 0;
   }
   else
      rc = column;

   return( rc );
}

/*==========================================================================*/
int load_data( heading )
   char   *heading;
{
   char   buffer[LINELEN];
   int    moredata;
   int    nval;
   int    rc;
   int    i,j;
   double xsectid;

   /*-----------------------------------------*/
   /* Allocate memory for section numbers     */
   /* and calculated water surface elevations */
   /*-----------------------------------------*/
   max_secno   = XSECT_INCREMENT;
   secno_match = (int *) G_calloc( max_secno, sizeof(int) );
   secno_val   = (double *) G_calloc( max_secno, sizeof(double) );
   cwsel_val   = (double **) G_calloc( max_secno, sizeof(double *) );
   if ( !secno_match  ||  !secno_val  ||  !cwsel_val )
      mem_exit();

   /*------------------------*/
   /* Get first line of data */
   /*------------------------*/
   nval          = 0;
   nsecno        = 0;
   moredata      = next_line( hec2_fid, heading, buffer );

   while( moredata )
   {
      /*----------------------------*/
      /* Load next cross section id */
      /*----------------------------*/
      get_value( buffer, &xsectid, secno_col );
      if ( itruncate )
         secno_val[nsecno] = (double)((long)xsectid);
      else
         secno_val[nsecno] = xsectid * 1000;

      /*---------------------------------------------*/
      /* Allocate memory for cross section's CWSEL's */
      /*---------------------------------------------*/
      cwsel_val[nsecno] = (double *) malloc( nevents * sizeof(double) );
      if ( !cwsel_val[nsecno] )
         mem_exit();

      for ( i=0; i < nevents  &&  moredata; i++ )
      {
	 /*-------------------------------------------------------------*/
	 /* Load all water surface elevations for current cross section */
	 /*-------------------------------------------------------------*/
	 get_value( buffer, &(cwsel_val[nsecno][i]), cwsel_col );
	 nval++;
	 moredata = next_line( hec2_fid, heading, buffer );
      }

      /*--------------------------------------------------------------*/
      /* See if we need to allocate more space for next cross section */
      /*--------------------------------------------------------------*/
      nsecno++;
      if ( nsecno == max_secno )
      {
	 max_secno  += XSECT_INCREMENT;
         secno_match = (int *) G_realloc( (char *) secno_match,
                                        max_secno * sizeof(int) );
         secno_val   = (double *) G_realloc( (char *) secno_val,
                                        max_secno * sizeof(double) );
	 cwsel_val   = (double **) G_realloc( (char *) cwsel_val,
					max_secno * nevents * sizeof(double) );
	 if ( !secno_match  ||  !secno_val  ||  !cwsel_val )
            mem_exit();

         /*---------------------------------*/
         /* match array must be initialized */
         /*---------------------------------*/
         for( j=nsecno; j < max_secno; j++ )
            secno_match[i] = 0;
      }
   }

   /*-----------------------------------------*/
   /* Set return code status before returning */
   /*-----------------------------------------*/
   if ( i != nevents  ||  nsecno * nevents != nval )
   {
      fprintf (stdout,"    ERROR: unexpected end of summary table\n" );
      fprintf (stdout,"           loaded %d values for %d cross sections\n",
			 nval, nsecno );
      rc = 0;
   }
   else
      rc = nsecno;

   return( rc );
}

/*==========================================================================*/
int load_split( heading )
   char   *heading;
{
   char   buffer[LINELEN];
   int    moredata;
   int    rc;
   int    i;
   int    id_indx;
   double xsectid;

   /*------------------------*/
   /* Get first line of data */
   /*------------------------*/
   moredata = next_line( split_fid, heading, buffer );
   while( moredata )
   {
      /*-------------------------------*/
      /* Look at next cross section id */
      /*-------------------------------*/
      get_value( buffer, &xsectid, secno_col );
      if ( itruncate )
         xsectid = (double)((long)xsectid);
      else
         xsectid *= 1000;

      id_indx = get_dindex( nsecno, secno_val, xsectid );
      if ( id_indx == -1 )
      {
         /*---------------------------------------*/
         /* Allocate memory for new cross section */
         /*---------------------------------------*/
         cwsel_val[nsecno] = (double *) malloc( nevents * sizeof(double) );
         if ( !cwsel_val[nsecno] )
            mem_exit();

         /*---------------------------*/
         /* Load new cross section id */
         /*---------------------------*/
         id_indx = nsecno++;
         secno_val[id_indx] = xsectid;
            
         /*--------------------------------------------------------------*/
         /* See if we need to allocate more space for next cross section */
         /*--------------------------------------------------------------*/
         if ( nsecno == max_secno )
         {
            max_secno  += XSECT_INCREMENT;
            secno_match = (int *) G_realloc( (char *) secno_match,
                                         max_secno * sizeof(int) );
            secno_val   = (double *) G_realloc( (char *) secno_val,
                                         max_secno * sizeof(double) );
   	    cwsel_val   = (double **) G_realloc( (char *) cwsel_val,
                                         max_secno * nevents * sizeof(double) );
            if ( !secno_match  ||  !secno_val  ||  !cwsel_val )
               mem_exit();
         }

         /*---------------------------------*/
         /* match array must be initialized */
         /*---------------------------------*/
         for( i=nsecno; i < max_secno; i++ )
            secno_match[i] = 0;
      }

      for ( i=0; i < nevents  &&  moredata; i++ )
      {
	 /*-------------------------------------------------------------*/
	 /* Load all water surface elevations for current cross section */
	 /*-------------------------------------------------------------*/
	 get_value( buffer, &(cwsel_val[id_indx][i]), cwsel_col );
	 moredata = next_line( split_fid, heading, buffer );
      }

   }

   /*-----------------------------------------*/
   /* Set return code status before returning */
   /*-----------------------------------------*/
   if ( i != nevents )
   {
      fprintf (stdout,"    ERROR: unexpected end of summary table\n" );
      rc = 0;
   }
   else
      rc = nsecno;

   return( rc );
}

/*==========================================================================*/
int get_value( buffer, value, column )
   char   *buffer;
   double *value;
   int     column;
{
   int   i;
   char  garbage[WORDLEN];
   char *buff_ptr;

   /*-----------------------------------*/
   /* skip notation in column 2, if any */
   /*-----------------------------------*/
   buff_ptr = buffer;
   if ( buffer[1] = '*' )
      buff_ptr += 2;

   /*-------------------------------*/
   /* skip over to specified column */
   /*-------------------------------*/
   for ( i=1; i < column; i++ )
   {
      sscanf( buff_ptr, " %s", garbage );
      buff_ptr += lscan( buff_ptr, garbage) + strlen( garbage );
   }

   /*----------------------*/
   /* grab next data value */
   /*----------------------*/
   sscanf( buff_ptr, " %lf", value );
}

/*==========================================================================*/
int next_line( fid, heading, buffer )
   FILE *fid;
   char *heading;
   char *buffer;
{
   char  garbage[LINELEN];
   char *status;
   int   rc = 1;

   status = fgets( buffer, LINELEN, fid );
   if ( status == NULL )
      rc = 0;
   else
   {
      /*--------------------------------------------*/
      /* keep reading until we get a non-blank line */
      /*--------------------------------------------*/
      while ( sscanf( buffer, " %s", garbage ) != 1   &&  status != NULL )
	 status = fgets( buffer, LINELEN, fid );

      /*-----------------------------------*/
      /* check for bad read or end-of-file */
      /*-----------------------------------*/
      if ( status == NULL )
	 rc = 0;
      else
      {
	 /*------------------------------------------------------*/
	 /* check for new page and continuation of summary table */
	 /*------------------------------------------------------*/
	 if ( buffer[0] == '1' )
	    if ( (rc = find_headings( fid, heading )) )
	    {
	       /*--------------------------------------------*/
	       /* skip over blank lines to next line of data */
	       /*--------------------------------------------*/
	       rc = next_line( fid, heading, buffer );
	    }
      }
   }
   return( rc );
}

/*==========================================================================*/
ctrl_events( mapset )
   char  *mapset;
{
   int   count;
   FILE *fid;
   char *gotline;
   char  token[WORDLEN];
   char  answer[WORDLEN];
   char  buffer[LINELEN];

   /*-------------------*/
   /* Read flood events */
   /*-------------------*/
   count = read_events( mapset, finput_ctrl, &event, &fid );
   if ( !count )
      return(0);

   /*-------------------------------------------------------*/
   /* set defaults for processing options                   */
   /* note that the sort option is not set here             */
   /* that's because it may have already been set to IGNORE */
   /*-------------------------------------------------------*/
   splitflow = FALSE;
   itruncate  = TRUE;

   /*------------------------------*/
   /* Check for special conditions */
   /*------------------------------*/
   gotline = fgets( buffer, LINELEN, fid );
   while ( gotline )
   {
      /*--------------------------------------------*/
      /* keep reading until we get to an assignment */
      /*--------------------------------------------*/
      while ( sscanf( buffer, " %s = %s", token, answer ) != 2   &&  gotline )
	 gotline = fgets( buffer, LINELEN, fid );

      /*-----------------------------*/
      /* process keyword assignments */
      /*-----------------------------*/
      if ( gotline )
      {
         /*---------------------------------------*/
         /* see if it is a split flow designation */ 
         /*---------------------------------------*/
         if( !strcmp( token, "SPLITFLOW" )  ||  !strcmp( token, "splitflow" ) )
         {
            if( answer[0] == 'Y'  ||  answer[0] == 'y' )
            {
               splitflow = TRUE;
               fprintf (stdout, "    NOTE: split flow conditions to be processed.\n" );
            }
            else if( answer[0] == 'N'  ||  answer[0] == 'n' )
               splitflow = FALSE;
         }

         /*------------------------------------*/
         /* see if it is a sorting designation */ 
         /*------------------------------------*/
         else if( !strcmp( token, "SORT" )  ||  !strcmp( token, "sort" ) )
         {
            if ( sort != IGNORE )
               sort = check_sort( answer );
         }

         /*---------------------------------------*/
         /* see if it is a truncation designation */ 
         /*---------------------------------------*/
         else if( !strcmp( token, "TRUNCATE" ) || !strcmp( token, "truncate" ) )
         {
            if( answer[0] == 'Y'  ||  answer[0] == 'y' )
            {
               itruncate = TRUE;
               fprintf (stdout, "    NOTE: HEC-2 secno's will be truncated.\n" );
            }
            else if( answer[0] == 'N'  ||  answer[0] == 'n' )
               itruncate = FALSE;
         }

         /*-------------------------*/
         /* get the next input line */
         /*-------------------------*/
         gotline = fgets( buffer, LINELEN, fid );
      }
   }

   fclose( fid );
   return( count );
}

/*==========================================================================*/
ask_events()
{
   int   i;
   int   count;
   int   valid;
   char  token[WORDLEN];
   char  prompt[LINELEN];

   /*----------------------------*/
   /* Get number of flood events */
   /*----------------------------*/
   valid = FALSE;
   while ( !valid )
   {
      fprintf (stdout,"    How many flood events are modeled in HEC2? : ");
      scanf( " %s", token );
      if ( sscanf( token, " %d", &count ) == 1 )
      {
         valid = TRUE;
         event = (char **) malloc( count *  sizeof( char *) );
         if ( !event )
            mem_exit();
      }
   }

   /*------------*/
   /* Get events */
   /*------------*/
   for ( i=0; i < count; i++ )
   {
      valid = FALSE;
      while( !valid )
      {
         sprintf( prompt, "    Enter flood event #%d (e.g., 1, 50, spf): ", 
                               i+1 );
         fprintf (stdout, prompt );
         scanf( " %s", token );
         if ( set_event( &(event[i]), token ) )
            valid = TRUE;
         else
            fprintf (stdout, "    Invalid flood event. Try again.\n" );
      }
   }

   /*--------------------------------------------------------------*/
   /* See if the is a second HEC2 file for "split flow" conditions */
   /*--------------------------------------------------------------*/
   splitflow = ask_yesno("    Is there a 'split flow' condition (2 HEC2 output files)?");

   /*--------------------------------*/
   /* See what the sorting option is */
   /*--------------------------------*/
   sort = ask_sort();

   /*-----------------------------------------------------------------*/
   /* See if the HEC2 secno's should be truncated to match vector map */
   /*-----------------------------------------------------------------*/
   itruncate = ask_yesno("    Truncate HEC-2 secno's?");

   return( count );
}

/*==========================================================================*/
xsect_map( event_indx, extra_xsect )
   int     event_indx;
   int    *extra_xsect;
{
   int    secno_indx;
   int    old_att;
   int    new_att;
   char   old_att_str[WORDLEN];
   char   new_att_str[WORDLEN];
   char   buffer[LINELEN];
   char  *buff_ptr;
   char   suffix[SUFFIXLEN];
   char   fout_map[WORDLEN];

   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   rewind( In_att );
   *extra_xsect = 0;

   /*------------------------*/
   /* Open output vector map */
   /*------------------------*/
   map_suffix( event[event_indx], suffix );
   sprintf( fout_map, "%s.%s", xsect_wsel_mconv, suffix );
   if ( (Vect_open_new( &Out_dig, fout_map ) ) < 0 )
   {
       fprintf (stdout,"   ERROR: Cannot create vector map '%s'.\n", fout_map );
       return(0);
   }

   /*-----------------------------------------------*/
   /* Write new xsect vector map (header and lines) */
   /*-----------------------------------------------*/
   if ( !copy_vector( &In_dig, &Out_dig, &nxsect ) )
      return(0);
   Vect_copy_head_data( &In_dig.head, &Out_dig.head );
   strcpy (Out_dig.head.map_name, "Output from f.input");
   strcpy (Out_dig.head.your_name, G_whoami ());

   /*-----------------------*/
   /* update attribute data */
   /*-----------------------*/
   if ((Out_att = G_fopen_new( "dig_att", fout_map )) == NULL)
   {
       fprintf (stdout,"   ERROR: Cannot write new attributes for '%s'.\n",
                   fout_map );
       return(0);
   }
   while ( fgets( buffer, LINELEN,  In_att ) )
   {
      sscanf( buffer, " %*s %*f %*f %d", &old_att );
      sprintf( old_att_str, "%d", old_att );
      secno_indx = get_dindex( nsecno, secno_val, (double)old_att );
      if ( secno_indx >= 0 )
      {
         /*-------------------------------------------------------*/
         /* Note that here we multiply CWSEL by a factor of 10 to */
         /* capture 1/10th ft. precision for integer GRASS maps.  */
         /*-------------------------------------------------------*/
         new_att = round( (cwsel_val[secno_indx][event_indx] * 10.0) );
         sprintf( new_att_str, "%d", new_att );
         secno_match[secno_indx] = TRUE;
      }
      else
      {
         (*extra_xsect)++;
         sprintf( new_att_str, "%d", 0 );
      }

      buff_ptr = buffer + rscan( buffer, old_att_str );
      strcpy( buff_ptr, new_att_str );
      fprintf( Out_att, "%s\n", buffer ); 
   }

   /*-------------*/
   /* Close files */
   /*-------------*/
   fclose( Out_att );
   Vect_close( &Out_dig );
   support_vector( fout_map );

   return(1);
}
