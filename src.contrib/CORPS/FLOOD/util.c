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
#include "flood.h"

/*==========================================================================*/
do_init( program )
   char *program;
{
   FILE *fid;

   print_title( stdout, program );
   G_gisinit( program );
   f_mapset = G_mapset();

   /*-----------------------*/
   /* Log user transactions */
   /*-----------------------*/
   G__file_name( log_fname, felement, log_fconv, f_mapset ); 
   fid = fopen( log_fname, "w" );
   if ( fid )
   {
      print_title( fid, program );
      fclose( fid );
      print_date( log_fname );
   }
   else
   {
      printf( "    INTERNAL ERROR: %s '%s'.\n",
              "cannot open log file.", log_fname );
      exit(1);
   }
}

/*==========================================================================*/
init_graphics( pwindow )
   C_HEAD *pwindow;
{
   /*-------------------------------*/
   /* Initialization for digitizing */
   /*-------------------------------*/
   R_open_driver();
   D_setup(0);
   G_get_set_window( pwindow );
   G_setup_plot ( D_get_d_north(), D_get_d_south(),
                  D_get_d_west(),  D_get_d_east(),
                  D_move_abs,      D_cont_abs );
}

/*==========================================================================*/
print_date( fname )
   char *fname;
{
   char  command[BUFFLEN];

   sprintf( command, "date >> %s", fname );
   system( command );
}

/*==========================================================================*/
print_title( fid, title )
   FILE *fid;
   char *title;
{
   int  i;
   int  len;
   char buff1[WORDLEN];
   char buff2[WORDLEN];

   sprintf( buff1, "    /// %s ///\n", title );
   len = strlen( buff1 ) - 5;

   strcpy( buff2, "    " );
   for ( i=0; i < len; i++ )
      strcat( buff2, "/" );
   strcat( buff2, "\n" );

   fprintf( fid, buff2 );
   fprintf( fid, buff1 );
   fprintf( fid, buff2 );
}

/*==========================================================================*/
mem_exit()
{
   printf( "    ERROR: out of memory!\n" );
   exit( 1 );
}

/*==========================================================================*/
non_blank_line( fid, buffer )
   FILE *fid;
   char *buffer;
{
   char *newline;
   char  token[BUFFLEN];

   newline = fgets( buffer, BUFFLEN, fid );
   while ( newline  &&  sscanf( buffer, " %s", token ) != 1 )
      newline = fgets( buffer, BUFFLEN, fid );
   

   if ( newline )
      return(1);
   else
      return(0);
}

/*==========================================================================*/
blank_pad( buffer, nbytes )
   char *buffer;
   int   nbytes;
{
   int   i;

   for ( i=0; i < nbytes; i++ )
      buffer[i] = ' ';
}

/*==========================================================================*/
r_justify( field, word, field_len )
   char *field;
   char *word;
   int   field_len;
{
   int  word_len;
   int  offset;

   blank_pad( field, WORDLEN );
   word_len = strlen( word );
   offset  = field_len - word_len;
   strcpy( field+offset, word );
}

/*==========================================================================*/
lscan( s, t )
   char *s;
   char *t;
{
   int i, j, k;

   for ( i=0; s[i] != '\0'; i++ )
   {
      for ( j=i, k=0;  t[k] != '\0'  &&  s[j] == t[k];  j++, k++ );
      if ( t[k] == '\0' )
         return(i);
   }
   return(-1);
}

/*==========================================================================*/
rscan( s, t )
   char *s;
   char *t;
{
   int i, j, k;
 
   for ( i=strlen(s)-1; i >= 0; i-- )
   {
      for ( j=i, k=strlen(t)-1;  k >= 0  &&  s[j] == t[k];  j--, k-- );
      if ( k < 0 )
         return(j+1);
   }
   return(-1);
}   
 
/*==========================================================================*/
ssort( nlist, list )
    int    nlist;
    char **list;
{
    int   i;
    int   j;
    int   gap;
    int   jval;
    int   gval;
    char *temp;

    for( gap=nlist/2;  gap > 0;  gap/=2 )
       for( i=gap;  i < nlist;  i++ )
          for( j=i-gap;  j >= 0;  j-=gap )
          {
             if ( sscanf( list[j], "%d", &jval ) == 1  &&
                  sscanf( list[j+gap], "%d", &gval ) == 1 )
             {
                if ( jval <= gval )
                   break;
             }
             else 
             {
                if ( strcmp( list[j], list[j+gap] ) <= 0 )
                   break;
             }
             temp = list[j];
             list[j] = list[j+gap];
             list[j+gap] = temp;
          }
}

/*==========================================================================*/
ask_yesno( question )
   char *question;
{
   int  flag;
   char answer[WORDLEN];

   flag = -1;
   while ( flag == -1 )
   {
      printf( "%s (y/n): ", question );
      scanf( " %s", answer );

      if ( !strcmp( answer, "YES" )  ||  !strcmp( answer, "yes" )  ||
           !strcmp( answer, "Y" )    ||  !strcmp( answer, "y" ) )
           flag = TRUE;
      else if ( !strcmp( answer, "NO" )  ||  !strcmp( answer, "no" )  ||
                !strcmp( answer, "N" )   ||  !strcmp( answer, "n" ) )
           flag = FALSE;
   }

   return( flag );
}

/*==========================================================================*/
point_yesno( question )
   char *question;
{
   int flag;
   int button;
   int screen_x;
   int screen_y;

   flag = -1;
   while ( flag == -1 )
   {
      printf( "%s\n", question );
      fflush( stdout );
      fprintf (stderr, "    Buttons\n");
      fprintf (stderr, "       Left:   YES\n");
      fprintf (stderr, "       Right:  NO\n");
      R_get_location_with_pointer( &screen_x, &screen_y, &button ) ;

      if ( button == 1 )
           flag = TRUE;
      else if ( button == 3 )
           flag = FALSE;
   }

   return( flag );
}

/*==========================================================================*/
catindex( category_str )
   char *category_str;
{
   int index;

   /*---------------------------------------------*/
   /* NOTE: these indicies must match the #define */
   /*       categories in the file "flood.h       */
   /*---------------------------------------------*/
        if ( !strncmp( category_str, "R",   1 ) )   index = 0;
   else if ( !strncmp( category_str, "MFR", 3 ) )   index = 1;
   else if ( !strncmp( category_str, "MR",  2 ) )   index = 2;
   else if ( !strncmp( category_str, "C",   1 ) )   index = 3;
   else if ( !strncmp( category_str, "P",   1 ) )   index = 4;
   else if ( !strncmp( category_str, "POV", 3 ) )   index = 5;
   else index = -1;

   return( index );
}

/*==========================================================================*/
get_dindex( nitems, list, value )
    int    nitems;
    double list[];
    double value;
{
   int i;
   int index = -1;
   int found = FALSE;
 
   for ( i=0; i < nitems  &&  !found; i++ )
   {
      /*----------------------------------------------------------------*/
      /* What is this wierd round off problem with depth categories ??? */
      /* The test SHOULD BE:    if ( list[i] == value )                 */
      /*----------------------------------------------------------------*/
      if ( fabs( (list[i] - value) ) < 0.001 )
      {
         found = TRUE;
         index = i;
      }
   }   
   return( index );
}
 
/*==========================================================================*/
get_sindex( nitems, list, string )
    int   nitems;
    char *list[];
    char *string;
{
   int i;
   int index = -1;
   int found = FALSE;

   for ( i=0; i < nitems  &&  !found; i++ )
   {
      if ( !strcmp( list[i], string ) )
      {
         found = TRUE;
         index = i;
      }
   }
   return( index );
}   

/*==========================================================================*/
round( value )
   double value;
{
   double answer;
   double remainder;
  
   remainder = value - (int)value;
   if ( remainder >= 0.5 && value > 0.0 ||  /* positive values */
        remainder <= 0.5 && value < 0.0 )   /* negative values */
      answer = ceil( value );
   else
      answer = floor( value );

   return( (int)answer );
}

/*==========================================================================*/
double distance( x, y, x1, y1 )
   double x, y;
   double x1, y1;
{
   double dx, dy;
   double dist;

   dx = x1 - x;
   dy = y1 - y;

   dist = sqrt( dx*dx + dy*dy ); 
   return( dist );
}

/*==========================================================================*/
double distance_sq( x, y, x1, y1 )
   double x, y;
   double x1, y1;
{
   double dx, dy;
   double dist2;

   dx = x1 - x;
   dy = y1 - y;

   dist2 = dx*dx + dy*dy; 
   return( dist2 );
}

/*==========================================================================*/
free_2d( list, nlist )
   char *list;
   int   nlist;
{
   int i;

   for ( i=0; i < nlist; i++ )
      free( (char *)list[i] );

   free( list );
}

/*==========================================================================*/
value_fmt( value, fmt_str, fmt_type, brief )
   double value;
   char  *fmt_str;
   int    fmt_type;
   int    brief;
{
   int  i;
   int  j;
   int  tindx;
   int  findx;
   int  nlead;
   int  ndigits;
   int  ncommas;
   char decimal;
   char token[WORDLEN];

   /*--------------------------------*/
   /* Check for special NOVALUE case */
   /*--------------------------------*/
   if ( value == (double)NOVALUE )
   {
      strcpy( fmt_str, " " );
      return;
   }
 
   /*---------------------------------------------*/
   /* Round to whole value, and convert to string */
   /*---------------------------------------------*/
   if ( brief )
      value = value / 1000.0;
   if ( fmt_type == ONEDEC )
      value = value * 10.0;
   sprintf( token, "%ld", round( value ) );
 
   /*------------------------------------------*/
   /* For ONEDEC format, save right-most digit */
   /*------------------------------------------*/
   if ( fmt_type == ONEDEC )
   {
      ndigits = strlen( token );
      decimal = token[ndigits-1];
      if ( ndigits == 1 )
         token[0] = '0';
      else 
      {
         if ( ndigits == 2  &&  token[0] == '-' )
            token[1] = '0';
         else
            token[ndigits-1] = '\0';
      }
   }      
 
   /*--------------------------------------------------*/
   /* Take count of digits, leading digits, and commas */
   /*--------------------------------------------------*/
   ndigits  = strlen( token );
   nlead    = ndigits % 3;
   if ( !nlead )
      nlead = 3;
   ncommas  = (ndigits - nlead) / 3;
 
   /*-------------------------*/
   /* Initialize value string */
   /*-------------------------*/
   tindx = 0;
   findx = 0;
   if ( fmt_type == DOLLAR )
      fmt_str[findx++] = '$';
 
   /*-----------------------*/
   /* Format leading digits */
   /*-----------------------*/
   for ( i=0;  i<nlead;  i++ )
      fmt_str[findx++] = token[tindx++];
 
   /*-----------------------------------------------*/
   /* Format groups of 3 digits separated by commas */
   /*-----------------------------------------------*/
   for ( i=0; i<ncommas; i++ )
   {
      fmt_str[findx++] = ',';
      for ( j=1; j<=3; j++ )
         fmt_str[findx++] = token[tindx++];
   }
 
   /*---------------------------------------*/
   /* Add single decimal place if requested */
   /*---------------------------------------*/
   if ( fmt_type == ONEDEC )
   {
      fmt_str[findx++] = '.';
      fmt_str[findx++] = decimal;
   }
 
   /*----------------*/
   /* Null terminate */
   /*----------------*/
   fmt_str[findx] = '\0';
}

/*==========================================================================*/
double dist_factor( base_unit, new_unit )
   int    base_unit;
   int    new_unit;
{
   double factor;

   /*--------------*/
   /* Get map info */
   /*--------------*/
   if ( base_unit == PROJECTION )
      base_unit = G__projection_units( G_projection() );
   factor   = 1.0;

   /*----------------*/
   /* Do conversions */
   /*----------------*/
   switch( new_unit )
   {
      case( FEET ):
            if ( base_unit == METERS ) factor = 3.28;
            break;

      case( METERS ):
            if ( base_unit == FEET ) factor = 0.3048;
            break;
   }
   return( factor );
}      

/*==========================================================================*/
double cvt_dist( distance, base_unit, new_unit )
   double  distance;
   int     base_unit;
   int     new_unit;
{
   double answer;

   answer = distance * dist_factor( base_unit, new_unit );
   return( answer );
}

/*==========================================================================*/
double cvt_area( area, base_unit, new_unit )
   double  area;
   int     base_unit;
   int     new_unit;
{
   int    dist_unit;
   double answer;
   double d_factor;
   double a_factor;

   switch( new_unit )
   {
      case( SQ_FEET ):
      case( SQ_MILES ):
      case( ACRES ):
            dist_unit = FEET;
            break;
      case( SQ_METERS ):
      case( HECTARES ):
            dist_unit = METERS;
            break;
   }

   d_factor = dist_factor( base_unit, dist_unit );
   a_factor = d_factor * d_factor;
   switch( new_unit )
   {
      case( SQ_FEET ):
      case( SQ_METERS ):
            answer = area * a_factor;
            break;
      case( SQ_MILES ):
            answer = area * a_factor / 27878400.0;
            break;
      case( ACRES ):
            answer = area * a_factor / 43560.0;
            break;
      case( HECTARES ):
            answer = area * a_factor / 10000.0;
            break;
   }
   return( answer );
}

/*==========================================================================*/
double cvt_volume( vol, base_unit, new_unit )
   double  vol;
   int     base_unit;
   int     new_unit;
{
   int    dist_unit;
   double answer;
   double d_factor;
   double v_factor;

   switch( new_unit )
   {
      case( CU_FEET ):
      case( ACRE_FEET ):
            dist_unit = FEET;
            break;
      case( CU_METERS ):
            dist_unit = METERS;
            break;
   }

   d_factor = dist_factor( base_unit, dist_unit );
   v_factor = d_factor * d_factor * d_factor;

   switch( new_unit )
   {
      case( CU_FEET ):
      case( CU_METERS ):
            answer = vol * v_factor;
            break;
      case( ACRE_FEET ):
            answer = vol * v_factor / 43560.0;
            break;
   }
   return( answer );
}

/*==========================================================================*/
coord_to_cell( window, loc_north, loc_east, row, col )
   struct Cell_head window;
   double loc_north;
   double loc_east;
   int   *row;
   int   *col;
{
   static int nrows;
   static int first = TRUE;
 
   if ( first )
   {
      first = FALSE;
      nrows = G_window_rows();
   }
 
   *row = ( nrows - ( ( loc_north - window.south ) / window.ns_res) );
   *col = ( ( loc_east - window.west ) / window.ew_res );
}

/*==========================================================================*/
screen_to_utm( window, screen_x, screen_y, utm_x, utm_y )
   struct Cell_head window;
   int screen_x;
   int screen_y;
   double *utm_x;
   double *utm_y;
{
   int     row, col;

   *utm_x = D_d_to_u_col((double)screen_x);
   *utm_y = D_d_to_u_row((double)screen_y);
   row = (window.north - *utm_y) / window.ns_res;
   col = (*utm_x - window.west) / window.ew_res;
   if (row < 0 || row >= window.rows) return(0);
   if (col < 0 || col >= window.cols) return(0);
   *utm_y = window.north - (row+.5) * window.ns_res;
   *utm_x  = window.west  + (col+.5) * window.ew_res;

   return(1);
}

/*==========================================================================*/
