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

/*---------*/
/* globals */
/*---------*/
#define CLDIST 1
#define SECNO  2
#define X_SORT struct xs_sort
struct xs_sort
{
   double   cl_dist;
   int      secno;
   L_PNTS  *points;
   X_SORT  *next;
   X_SORT  *prev;
};

X_SORT *headptr; 
X_SORT *tailptr;
C_HEAD  window;

/*==========================================================================*/
ask_sort()
{
   int   valid;
   char  token[WORDLEN];

   if ( sort != IGNORE )
   {
      valid = FALSE;
      while ( !valid )
      {
         printf("    Enter cross section sorting option [NONE, USER, VECTOR, VALUE] : ");
         scanf( " %s", token );
         sort = check_sort( token );
         if ( sort )
            valid = TRUE;
      }
   }  
   return( sort );
}

/*==========================================================================*/
check_sort( answer )
   char *answer;
{
   int sort_opt;

   if ( !strcmp( answer, "NONE" )  ||  !strcmp( answer, "none" ) )
   {
      sort_opt = NOSORT;
      printf( "    NOTE: cross sections will not be sorted.\n" );
   }
 
   else if ( !strcmp( answer, "VALUE" )  ||  !strcmp( answer, "value" ) )
   {
      sort_opt = VALUE;
      printf( "    NOTE: cross sections will be sorted by value.\n" );
   }
 
   else if ( !strcmp( answer, "USER" )  ||  !strcmp( answer, "user" ) )
      sort_opt = USER;
 
   else if ( !strcmp( answer, "VECTOR" )  ||  !strcmp( answer, "vector" ) )
      sort_opt = VECTOR;

   else
      sort_opt = IGNORE;
 
   return( sort_opt );
}

/*==========================================================================*/
user_sort( xsect_vid, sorted_fname, sorted_vid )
   M_INFO *xsect_vid;
   char   *sorted_fname;
   M_INFO *sorted_vid;
{
   int     i, npoints = 0;
   int     button;
   int     screen_x, screen_y;
   int     line_color;
   double  x_pair[2];
   double  y_pair[2];
   L_PNTS *center_line;

   /*-------------------------------*/
   /* Initialization for digitizing */
   /*-------------------------------*/
   init_graphics( &window );
   line_color = D_translate_color( "red" );
   R_standard_color( line_color );

   /*-------------------------*/
   /* Begin digitizing points */
   /*-------------------------*/
   printf( "\n    Digitize vector (downstream to upstream) for cross section sorting...\n" );
   center_line = Vect_new_line_struct();
   user_show_buttons(); 
   while ( TRUE )
   { 
      R_get_location_with_pointer( &screen_x, &screen_y, &button ) ;
      if ( button == 3 ) 
         break; 
      if ( !user_add_vertex( npoints, screen_x, screen_y, 
                             x_pair, y_pair ) )
      { 
         printf("    ERROR: cannot add digitized point.\n" ); 
         return( FALSE );
      }
      Vect_append_point( center_line, x_pair[1], y_pair[1] ); 
      npoints++; 
   } 

   /*------------------------------------------------*/
   /* sort cross sections from digitized center line */
   /*------------------------------------------------*/
   if ( !cline_sort( xsect_vid, center_line ) )
      return( FALSE );

   /*----------*/
   /* clean up */
   /*----------*/
   Vect_destroy_line_struct( center_line );

   /*-----------------------*/
   /* write out the results */
   /*-----------------------*/
   if ( !write_sorted( xsect_vid, sorted_fname, sorted_vid ) )
      return( FALSE );
   else
      return( TRUE );
}

/*==========================================================================*/
vector_sort( xsect_vid, sorted_fname, sorted_vid )
   M_INFO *xsect_vid;
   char   *sorted_fname;
   M_INFO *sorted_vid;
{
   char   *mapset;
   char    cline_fname[FNAMELEN];
   L_PNTS *center_line;
   M_INFO  cline_vid;

   /*---------------------*/
   /* Get vector map name */
   /*---------------------*/
   printf("    Enter vector map name to be used for sorting cross sections: ");
   scanf( " %s", cline_fname );
   mapset = G_find_vector( cline_fname, "" );
   if ( mapset == NULL )
   {
       printf ( "    ERROR: vector map '%s' not found.\n", cline_fname );
       return( FALSE );
   }

   /*-----------------*/       
   /* Open vector map */
   /*-----------------*/
   if ( ( Vect_open_old( &cline_vid, cline_fname, mapset ) ) < 0 )
   {
       printf ("    ERROR: Cannot open vector map '%s'.\n", cline_fname );
       return( FALSE );
   }                     

   /*------------------------------------------------*/ 
   /* Read center line vector                        */
   /* NOTE: assumed to be the only vector in the map */
   /*------------------------------------------------*/ 
   center_line = Vect_new_line_struct();
   Vect_read_next_line( cline_vid, center_line );

   /*------------------------------------------*/
   /* sort cross sections from map center line */
   /*------------------------------------------*/
   if ( !cline_sort( xsect_vid, center_line ) )
      return( FALSE );

   /*----------*/
   /* clean up */
   /*----------*/
   Vect_destroy_line_struct( center_line );

   /*-----------------------*/
   /* write out the results */
   /*-----------------------*/
   if ( !write_sorted( xsect_vid, sorted_fname, sorted_vid ) )
      return( FALSE );
   else
      return( TRUE );
}

/*==========================================================================*/
value_sort( xsect_vid, sorted_fname, sorted_vid )
   M_INFO *xsect_vid;
   char   *sorted_fname;
   M_INFO *sorted_vid;
{
   /*-------------------------------------*/
   /* sort cross sections by secno values */
   /*-------------------------------------*/
   if ( !secno_sort( xsect_vid ) )
      return( FALSE );

   /*-----------------------*/
   /* write out the results */
   /*-----------------------*/
   if ( !write_sorted( xsect_vid, sorted_fname, sorted_vid ) )
      return( FALSE );
   else
      return( TRUE );
}

/*==========================================================================*/
cline_sort( xsect_vid, center_line )
   M_INFO *xsect_vid;
   L_PNTS *center_line;
{
   int i, nsegs, xseg, cseg;
   int isect, sort_count, found;
   double *seg_dist, cl_dist;
   double int_x, int_y;
   L_PNTS *xs_points;

   /*--------------------------------------*/
   /* calculate cumulative segment lengths */
   /*--------------------------------------*/
   nsegs = center_line->n_points - 1;
   seg_dist = (double *)malloc( nsegs * sizeof(double) );
   if ( !seg_dist )
      mem_exit();
   for( i=0; i < nsegs; i++ )
   {
      seg_dist[i] = distance ( center_line->x[i],   center_line->y[i],
                               center_line->x[i+1], center_line->y[i+1] );
      if ( i > 0 )
         seg_dist[i] += seg_dist[i-1];
   }

   /*----------------------------*/
   /* Process each cross section */
   /*----------------------------*/
   sort_count = 0;
   Vect_rewind( xsect_vid );
   for( i=0; i < xsect_vid->n_lines; i++ )
   {
      xs_points = Vect_new_line_struct();
      Vect_read_next_line( xsect_vid, xs_points );

      /*---------------------------------------*/
      /* Process each segment of cross section */
      /*---------------------------------------*/
      found = FALSE;
      for( xseg=0; !found  &&  xseg < xs_points->n_points-1; xseg++ )
      {
         /*-----------------------------------------------*/
         /* Check against each segment of the center line */
         /*-----------------------------------------------*/
         for( cseg=0; !found  &&  cseg < nsegs; cseg++ )
         {
            isect = dig_find_intersection( xs_points->x[xseg],     xs_points->y[xseg],
                                           xs_points->x[xseg+1],   xs_points->y[xseg+1],
                                           center_line->x[cseg],   center_line->y[cseg],
                                           center_line->x[cseg+1], center_line->y[cseg+1],
                                           &int_x,                 &int_y );

            /*----------------------------------*/
            /* check for multiple intersections */
            /*----------------------------------*/
            if ( isect == -1 )
            {
               printf("    ERROR: multiple intersections not allowed.\n" );
               free_sortlist();
               return( FALSE );
            }

            /*----------------------------------------*/
            /* See if we found the right intersection */
            /*----------------------------------------*/
            if ( isect == 1 )
            {
               found = TRUE;
               cl_dist = distance( center_line->x[cseg], center_line->y[cseg],
                                   int_x, int_y );
               if ( cseg > 0 )
                  cl_dist += seg_dist[cseg-1];
               add_sort_entry( xsect_vid, i, xs_points, CLDIST, cl_dist );
               sort_count++;
            }
         }
      }
      if ( !found )
         free( xs_points );
   }

   /*------------------------------*/
   /* check for minimum conditions */
   /*------------------------------*/
   if ( sort_count < 2 )
   {
      printf("    ERROR: sort vector must cross at least two cross sections.\n" );
      free_sortlist();
      return( FALSE );
   }

   return( TRUE );
}

/*==========================================================================*/
secno_sort( xsect_vid )
   M_INFO *xsect_vid;
{
   int i;
   L_PNTS *xs_points;

   /*----------------------------*/
   /* Process each cross section */
   /*----------------------------*/
   Vect_rewind( xsect_vid );
   for( i=0; i < xsect_vid->n_lines; i++ )
   {
      xs_points = Vect_new_line_struct();
      Vect_read_next_line( xsect_vid, xs_points );
      add_sort_entry( xsect_vid, i, xs_points, SECNO, 0L );
   }
   return( TRUE );
}

/*==========================================================================*/
add_sort_entry( xsect_vid, xs_index, xs_points, criterion, dist )
   M_INFO *xsect_vid;
   int     xs_index;
   L_PNTS *xs_points;
   int     criterion;
   double  dist;
{
   int     att_index;
   int     here, inserted;
   X_SORT *listptr;
   X_SORT *newptr;

   /*--------------------------*/
   /* new entry to linked list */
   /*--------------------------*/
   newptr = (X_SORT *) malloc( sizeof(X_SORT) );
   if ( !newptr )
      mem_exit();

   /*---------------*/
   /* assign fields */
   /*---------------*/
   att_index = xsect_vid->Line[xs_index+1].att;
   newptr->secno = xsect_vid->Att[att_index].cat;
   newptr->points = xs_points;
   newptr->cl_dist = dist;

   /*---------------------*/
   /* process linked list */
   /*---------------------*/
   inserted = FALSE;
   listptr = headptr;
   while( !inserted && listptr )
   {
      /*-----------------------------------------------------*/
      /* insert entry in increasing order based on criterion */
      /*-----------------------------------------------------*/
      here = FALSE;
      switch ( criterion )
      {
         case CLDIST:
            if ( listptr->cl_dist > dist )
            here = TRUE;
            break;

         case SECNO:
            if ( listptr->secno > newptr->secno )
            here = TRUE;
            break;
      }

      if ( here )
      {
         /*-------------*/
         /* insert here */
         /*-------------*/
         newptr->next = listptr;
         newptr->prev = listptr->prev;

         /*---------------------------------------*/
         /* see if "here" is the head of the list */
         /*---------------------------------------*/
         if ( !listptr->prev )
            headptr = newptr;
         else
            listptr->prev->next = newptr;

         listptr->prev = newptr;
         inserted = TRUE;
      }
      listptr = listptr->next;
   }

   /*------------------------------------*/
   /* check for insertion at end of list */
   /*------------------------------------*/
   if ( !inserted )
   {
      newptr->next = NULL;
      newptr->prev = tailptr;
      if ( tailptr )
         tailptr->next = newptr;
      tailptr = newptr;
      if ( !headptr )
         headptr = newptr;
   }
}

/*==========================================================================*/
user_add_vertex( npoints, screen_x, screen_y, x_pair, y_pair )
   int     npoints;
   int     screen_x;
   int     screen_y;
   double *x_pair;
   double *y_pair;
{
   int     row, col;
   double  east, north;
   double  dist;
 
   /*---------------------------*/
   /* Determine vertex location */
   /*---------------------------*/
   if ( !screen_to_utm( window, screen_x, screen_y, &east, &north ) )
   {
      printf("    ERROR: cannot convert screen coordinates.\n");
      return(0);
   }
   x_pair[1] = east;
   y_pair[1] = north;
    
   /*--------------*/
   /* plot segment */
   /*--------------*/
   if ( npoints )
   {
      G_plot_line( x_pair[0], y_pair[0],
                   x_pair[1], y_pair[1] );
      R_flush();
   }
 
   /*-------------------------------------------------*/
   /* Save vertex's x,y location for next time around */
   /*-------------------------------------------------*/
   x_pair[0] = x_pair[1];
   y_pair[0] = y_pair[1];
 
   return(1);
}

/*==========================================================================*/
user_show_buttons ()
{
   fprintf (stderr, "    Buttons\n");
   fprintf (stderr, "       Left:   add a point\n");
   fprintf (stderr, "       Right:  end digitizing\n\n");
}

/*==========================================================================*/
write_sorted( xsect_vid, sorted_fname, sorted_vid )
   M_INFO *xsect_vid;
   char   *sorted_fname;
   M_INFO *sorted_vid;
{
   char    buffer[LINELEN];
   double  lbl_x, lbl_y;
   X_SORT *listptr;
   FILE   *sorted_att;

   /*------------------------*/
   /* Open sorted vector map */
   /*------------------------*/
   if ( (Vect_open_new( sorted_vid, sorted_fname ) ) < 0 )
   {
       printf ("   ERROR: Cannot create vector map '%s'.\n", sorted_fname );
       free_sortlist();
       return( FALSE );
   }

   /*----------------------------*/
   /* open sorted attribute file */
   /*----------------------------*/
   if ((sorted_att = G_fopen_new( "dig_att", sorted_fname )) == NULL)
   {
       printf ("   ERROR: Cannot write sorted attributes for '%s'.\n",
                   sorted_fname );
       return(0);
   }

   /*------------------------------------*/
   /* Write sorted cross section vectors */
   /*------------------------------------*/
   listptr = headptr;
   while( listptr )
   {
      if ( Vect_write_line( *sorted_vid, LINE, listptr->points ) < 0 )
      {
         printf("    ERROR: cannot write out sorted cross section map.\n");
         free_sortlist();
         return( FALSE );
      }
      /*-----------------------*/
      /* write attribute value */
      /*-----------------------*/
      get_line_center( &lbl_x, &lbl_y, listptr->points );
      sprintf( buffer, "L %f %f %d", lbl_x, lbl_y, listptr->secno );
      fprintf( sorted_att, "%s\n", buffer );

      /*-------------*/
      /* next vector */
      /*-------------*/
      listptr = listptr->next;
   }
   free_sortlist();

   /*-----------------------------------*/
   /* Write sorted cross section header */
   /*-----------------------------------*/
   Vect_copy_head_data( &(xsect_vid->head), &(sorted_vid->head) );
   strcpy (sorted_vid->head.map_name, "Sorted cross sections from f.input");
   strcpy (sorted_vid->head.your_name, G_whoami ());

   /*-------------------------------------------*/
   /* do topology support for sorted vector map */
   /*-------------------------------------------*/
   fclose( sorted_att );
   Vect_close( sorted_vid );
   support_vector( sorted_fname );
 
   /*--------------------------*/
   /* reopen sorted vector map */
   /*--------------------------*/
   if ( ( Vect_open_old( sorted_vid, sorted_fname, f_mapset ) ) < 0 )
   {
       printf ("    ERROR: Cannot re-open sorted cross section map '%s'.\n",
                    sorted_fname );
       return( FALSE );
   }

   return( TRUE );
}

/*==========================================================================*/
free_sortlist()
{
   X_SORT *thisptr, *nextptr;

   nextptr = headptr;
   while( nextptr )
   {
      thisptr = nextptr;
      Vect_destroy_line_struct( thisptr->points );
      nextptr = thisptr->next;
      free( thisptr );
   }
}

/*==========================================================================*/
char *get_xsect_name( xsect_fname )
   char *xsect_fname;
{
   char *xsect_mapset;
   char *sort_mapset;
   char  sorted_fname[FNAMELEN];

   sort = NOSORT;
   xsect_mapset = G_find_vector( xsect_id_mconv, "" );
   if ( xsect_mapset )
   {   
      sprintf( sorted_fname, "%s.%s",  xsect_id_mconv, sort_mconv );
      sort_mapset = G_find_vector( sorted_fname, "" );
      if ( sort_mapset )
      {
         /*------------------------------*/
         /* use sorted cross section map */
         /*------------------------------*/
         printf("    NOTE: Using sorted cross section map '%s'.\n",
                     sorted_fname );
         strcpy( xsect_fname, sorted_fname );
         sort = IGNORE;
      }
      else
         /*-------------------------------------*/
         /* sorted cross sections not available */
         /*-------------------------------------*/
         strcpy( xsect_fname, xsect_id_mconv );
   }
   else
   { 
      printf("    Enter cross section map name: ");
      scanf( " %s", xsect_fname );
      xsect_mapset = G_find_vector( xsect_fname, "" );
      if ( xsect_mapset )
      {
         sprintf( sorted_fname, "%s.%s",  xsect_fname, sort_mconv );
         sort_mapset = G_find_vector( sorted_fname, "" );
         if ( sort_mapset )
         {
            /*------------------------------*/
            /* use sorted cross section map */
            /*------------------------------*/
            printf("    NOTE: Using sorted cross section map '%s'.\n",
                        sorted_fname );
            strcpy( xsect_fname, sorted_fname );
            sort = IGNORE;
         }
      }
      else
          /*---------------------------------------*/
          /* Note: this may not always be an error */
          /*---------------------------------------*/
          return(NULL);
   }

   return( xsect_mapset );
}

/*==========================================================================*/
get_xsect_map( xsect_fname, mapset, sorted_vid )
   char   *xsect_fname;
   char   *mapset;
   M_INFO *sorted_vid;
{
   int    sort_flag;
   char   sorted_fname[FNAMELEN];
   M_INFO xsect_vid;

   /*-------------------------------------*/
   /* Open the original cross section map */
   /*-------------------------------------*/
   if ( ( Vect_open_old( &xsect_vid, xsect_fname, mapset ) ) < 0 )
   {
       printf ("    ERROR: Cannot open cross section map '%s'.\n",
                    xsect_fname );
       return( FALSE );
   }

   /*------------------------*/
   /* Process sorting option */
   /*------------------------*/
   switch( sort )
   {
      case IGNORE:
      case NOSORT:
         Vect_close( &xsect_vid );
         Vect_open_old( sorted_vid, xsect_fname, mapset );
         return( TRUE );
         break;
      case USER:
         printf("    Sorting cross sections...\n");
         sprintf( sorted_fname, "%s.%s", xsect_fname, sort_mconv );
         sort_flag = user_sort( &xsect_vid, sorted_fname, sorted_vid );
         break;
      case VECTOR:
         printf("    Sorting cross sections...\n");
         sprintf( sorted_fname, "%s.%s", xsect_fname, sort_mconv );
         sort_flag = vector_sort( &xsect_vid, sorted_fname, sorted_vid );
         break;
      case VALUE:
         printf("    Sorting cross sections...\n");
         sprintf( sorted_fname, "%s.%s", xsect_fname, sort_mconv );
         sort_flag = value_sort( &xsect_vid, sorted_fname, sorted_vid );
         break;
   }
   if ( !sort_flag )
   {
      printf("    ERROR: Cannot sort cross sections.\n" );
      return( FALSE );
   }

   strcpy( xsect_fname, sorted_fname );
   Vect_close( &xsect_vid );
   return( TRUE );
}

/*==========================================================================*/
