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

#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "flood.h"

/*==========================================================================*/
copy_vector( in_map, out_map, nvect )
   M_INFO *in_map;
   M_INFO *out_map;
   int    *nvect;
{
   int     itype;
   L_PNTS *points;

   /*-----------------*/
   /* Initializations */
   /*-----------------*/
   points = Vect_new_line_struct();
   Vect_rewind( in_map );

   /*------------------*/
   /* Copy header info */
   /*------------------*/
   Vect_copy_head_data( &(in_map->head), &(out_map->head) );
   strcpy (out_map->head.your_name, G_whoami ());

   /*-------------------*/
   /* Copy every vector */
   /*-------------------*/
   *nvect = 0;
   while ((itype = Vect_read_next_line( in_map, points )) > 0)
   {
      (*nvect)++;
      Vect_write_line( out_map, itype, points );
   }
   Vect_destroy_line_struct( points );
   
   /*------------------------*/
   /* Reposition source file */
   /*------------------------*/
   Vect_rewind( in_map );

   /*---------------*/
   /* Check for EOF */
   /*---------------*/
   if ( itype != -2 )
       return (0);
   else 
       return(1);
}

/*==========================================================================*/
support_vector( mapname )
   char *mapname;
{
   char command[BUFFLEN];

   G__make_mapset_element(ATT) ;
   G__make_mapset_element(PLUS) ;

   sprintf( command, "%s/etc/v.build  %s >> %s", 
                     G_gisbase(), mapname, log_fname ) ;
   return( system( command ) );
}
/*==========================================================================*/
/* D.Gerdes code from $GIS/src/mapdev/v.digit/label.c */
/*----------------------------------------------------*/
double fabs ();

get_line_center (x, y, Points)
    double *x, *y;
    struct line_pnts *Points;
{
    register int i;
    register int n_points;
    register double *ux, *uy;
    double dist;                /* running total of line length */
    double half_dist;           /* half total line length */
    double len;                 /* tmp length of current line seg */
    double frac;                /* overshoot / line length */

    n_points = Points->n_points;
    ux = Points->x;
    uy = Points->y;

    if (n_points <= 0)
        return -1;
    if (n_points == 1)
    {
        *x = Points->x[0];
        *y = Points->y[0];
        return (0);
    }
    dist = 0.0;
    /* get total dist */
    for (i = 1 ; i < n_points ; i++)
        dist += (fabs(ux[i]-ux[i-1]) + fabs(uy[i]-uy[i-1]));
    if (dist == 0.0)
    {
        *x = Points->x[0];
        *y = Points->y[0];
        return (0);
    }
 
    half_dist = dist / 2.0;
 
    dist = 0.0;
    for (i = 1 ; i < n_points ; i++)
    {
        len = (fabs(ux[i]-ux[i-1]) + fabs(uy[i]-uy[i-1]));
        dist += len;
        if (dist >= half_dist)  /* we're there */
        {
            frac = 1 - (dist - half_dist) / len;
            *x = frac * (ux[i]-ux[i-1]) + ux[i-1];
            *y = frac * (uy[i]-uy[i-1]) + uy[i-1];
            return (0);
        }
    }
 
    fprintf (stderr, "Get_line_center failed.\n");
    *x = Points->x[0];
    *y = Points->y[0];
    return (-1);
}
