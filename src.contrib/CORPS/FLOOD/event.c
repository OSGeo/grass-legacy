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
#include <sys/types.h>
#include <dirent.h>
#include "gis.h"
#include "flood.h"

/*==========================================================================*/
list_events( count, event_list )
   int   count;
   char **event_list;
{
   int  i;
   char buffer[BUFFLEN];

   sprintf( buffer, "    %s %d %s\n", "NOTE: the following", count,
                    "event(s) will be processed..." );
   printf( buffer );
   for ( i=0; i < count; i++ )
   {
      if ( !strcmp( event_list[i], "spf" ) )
         printf( "          %d) %s\n", i+1, event_list[i] );
      else
         printf( "          %d) %s year\n", i+1, event_list[i] );
   }
}

/*==========================================================================*/
map_suffix( event, suffix )
   char *event;
   char *suffix;
{
   strcpy( suffix, event );
   if ( strcmp( event, "spf" ) )
      strcat( suffix, "yr" );
}

/*==========================================================================*/
read_events( mapset, fname, list, fid )
   char   *mapset;
   char   *fname;
   char ***list;
   FILE  **fid;
{
   int   i;
   int   count;
   char *gotline;
   char  path[PATHLEN];
   char  token[WORDLEN];
   char  buffer[LINELEN];
 
   /*-------------------*/
   /* Open control file */
   /*-------------------*/
   G__file_name( path, felement, fname, mapset );
   if ( (*fid = fopen( path, "r" )) == NULL )
   {
      printf( "    ERROR: cannot open control file '%s'.\n", path );
      return( 0 );
   }
 
   /*--------------------------------------------------*/
   /* Read count of modeled events and allocate memory */
   /*--------------------------------------------------*/
   if ( !(gotline = fgets( buffer, LINELEN, *fid )) )
   {
      printf( "    ERROR: cannot read control file '%s'.\n", path );
      return( 0 );
   }
   while ( sscanf( buffer, " %d", &count ) != 1   &&  gotline )
         gotline = fgets( buffer, LINELEN, *fid );
   if ( !gotline )
   {
      printf( "    ERROR: %s '%s'.\n",
                  "unexpected end-of-file in control file", path );
      return( 0 );
   }
   *list = (char **) malloc( count *  sizeof( char *) );
   if ( !(*list) )
      mem_exit();
 
   /*---------------------------*/
   /* Read actual event strings */
   /*---------------------------*/
   for ( i=0; i < count; i++ )
   {
      /*--------------------------------------------*/
      /* keep reading until we get a non-blank line */
      /*--------------------------------------------*/
      gotline = fgets( buffer, LINELEN, *fid );
      while ( sscanf( buffer, " %s", token ) != 1   &&  gotline )
         gotline = fgets( buffer, LINELEN, *fid );
 
      /*----------------------*/
      /* process event string */
      /*----------------------*/
      if ( gotline )
      {
/*---
         if( !set_event( &((*list)[i]), token ) )
---*/
         if( !set_event( (*list)+i, token ) )
         {
            printf( "    ERROR: %s '%s'.\n",
                        "invalid flood event in control file at token ", 
                         token );
            return( 0 );
         }
      }
      else
      {
         printf( "    ERROR: %s '%s'.\n",
                     "unexpected end-of-file in control file", path );
         return( 0 );
      }
   }
   return( count );
}

/*==========================================================================*/
set_event( element, token )
   char **element;
   char  *token;
{
   int   year;
   int   valid;
   char  eve_str[WORDLEN];
 
   if ( sscanf( token, "%d", &year ) )
   {
      valid = TRUE;
      sprintf( eve_str, "%d", year );
   }
   else if ( !strcmp( token, "SPF" )  ||  !strcmp( token, "spf" ) )
   {
      valid = TRUE;
      strcpy( eve_str, "spf" );
   }
   else
      valid = FALSE;
       
   if ( valid )
   {
      *element = (char *)malloc( strlen(eve_str) + 1 );
      strcpy( *element, eve_str );
   }
   return( valid );
}   
 
/*==========================================================================*/
prompt_events( element, basename, event_list )
   char   *element;
   char   *basename;
   char ***event_list;
{
   int    i;
   int    index;
   int    nfree;
   int    finished;
   int    select_all;

   int    navail;
   int    nselect;
   char **avail_list;
   char **select_list;

   char   token[WORDLEN];
   char   prompt[LINELEN];
 
   /*-------------------------------*/
   /* See what events are available */
   /*-------------------------------*/
   navail = file_list( element, basename, &avail_list );
   if ( !navail )
      return(0);
   ssort( navail, avail_list );
   select_list = (char **) malloc( navail * sizeof(char*) );

   /*-----------------------*/
   /* Construct user prompt */
   /*-----------------------*/
   strcpy( prompt, "    Enter flood event (" );
   for ( i=0; i < navail; i++ )
   {
      sprintf( token, "%s, ", avail_list[i] );
      strcat( prompt, token );
   }
   strcat( prompt, "ALL[*], END[.]): " );

   /*--------------------------------*/
   /* See what events the user wants */
   /*--------------------------------*/
   nselect    = 0;
   finished   = FALSE;
   select_all = FALSE;

   while ( !finished )
   {
      printf( prompt );
      scanf( " %s", token );

      /*-------------------------------*/
      /* Check for end of user request */
      /*-------------------------------*/
      if ( !strcmp( token, "." )  ||  !strcmp( token, "END" )  ||
                                      !strcmp( token, "end" ) )
         finished = TRUE;

      /*-----------------------*/
      /* Check for ALL request */
      /*-----------------------*/
      else if ( !strcmp( token, "*" )  ||  !strcmp( token, "ALL" )  ||  
                                           !strcmp( token, "all" ) )
      {
         finished   = TRUE;
         select_all = TRUE;
         nfree      = nselect;
         nselect    = navail;
      }
      else
      {
         /*-----------------------*/
         /* Validate user request */
         /*-----------------------*/
         index = get_sindex( navail, avail_list, token );
         if ( index < 0 )
            printf("    Invalid flood event. Try again.\n");
         else
         {
            /*-------------------------------------------*/
            /* Make sure this is not a duplicate request */
            /* before adding it to the selection list    */
            /*-------------------------------------------*/
            index = get_sindex( nselect, select_list, token );
            if ( index < 0 )
            {
               select_list[nselect] = (char *) malloc( strlen(token) + 1 );
               strcpy( select_list[nselect], token );
               nselect++;
            }
         }
      }
   }

   /*-----------*/
   /* Finish up */
   /*-----------*/
   if ( select_all )
   {
      free_2d( (char *)select_list, nfree );
      *event_list = avail_list;
   }
   else
   {
      free_2d( (char *)avail_list, navail );
      *event_list = select_list;
   }

   return( nselect );
}

/*==========================================================================*/
file_list( element, basename, list )
   char   *element;
   char   *basename;
   char ***list;
{
   int             count;
   int             maxevents;
   int             index;
   int             baselen;
   int             valid;
   char            path[PATHLEN];
   char          **valid_list;
   char            suffix[WORDLEN];
   DIR            *dir_id;
   struct dirent  *entry;

   /*---------------------*/
   /* Open directory path */
   /*---------------------*/
   G__file_name( path, element, "", f_mapset );
   dir_id = opendir( path );
   if ( !dir_id )
      return(0);

   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   count      = 0;
   maxevents  = EVENT_INCREMENT;
   baselen    = strlen( basename );
   valid_list = (char **)malloc( maxevents * sizeof(char*) );
   if ( !valid_list )
      mem_exit();

   /*-------------------------------------*/
   /* For every entry in the directory... */
   /*-------------------------------------*/
   while ( entry = readdir( dir_id ) )
   {
      /*-----------------------------*/
      /* Check for matching basename */
      /*-----------------------------*/
      valid = FALSE;
      if ( !strncmp( entry->d_name, basename, baselen )  &&
                     entry->d_name[baselen] == '.' )
      {
         /*------------------------*/
         /* Check for valid suffix */
         /*------------------------*/
         strcpy( suffix, entry->d_name+baselen+1 );
         if ( !strcmp( suffix, "spf" ) )
            valid = TRUE;
         else
         {
            index = rscan( suffix, "yr" );
            if ( index > 0  &&  !strcmp( suffix+index, "yr" ) )
            {
               valid = TRUE;
               suffix[index] = '\0';
            }
         }

         /*---------------------*/
         /* Store valid entries */
         /*---------------------*/
         if ( valid )
         {
            valid_list[count] = (char *) malloc( strlen(suffix)+1 );
            strcpy( valid_list[count], suffix );
            count++;

            /*------------------------------------------*/
            /* See if we need more space for next entry */
            /*------------------------------------------*/
            if ( count == maxevents )
            {
               maxevents += EVENT_INCREMENT;
               valid_list = (char **) realloc( (char *)valid_list,
                                               maxevents * sizeof(char *) );
               if ( !valid_list )
                  mem_exit();
            }
         }
      }
   }

   /*-----------*/
   /* Finish up */
   /*-----------*/
   *list = valid_list;
   closedir( dir_id );
   return( count );
}
