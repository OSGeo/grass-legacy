/*******************************************************************************                         r.surf.voronoi
               Mother Earth Systems, Boulder, Colorado
 
 
This software was been developed for the U.S. Army Corps of Engineers,
Omaha District under contract #DACW45-92-P-1301. 
 
This code is in the public domain.  Permission to use, copy, modify, and 
distribute this software and its documentation for any purpose and without 
fee is granted. 
 
*******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "voronoi.h"
#include "rsv.h"

/*============================================================================*/
calc_areas( nsites )
  int     nsites;
{
  int    sid, nverts;
  float  pverts[MAX_PVERTS][2];
  extern double *area;

  /*-----------------------------------*/
  /* allocate memory for voronoi areas */
  /*-----------------------------------*/
  fprintf( stderr, "    Calculating Voronoi areas...\n" );
  area = (double*) malloc( nsites * sizeof(double) );
  if ( !area )
      G_fatal_error ("Insufficent memory for voronoi areas");

  for( sid = 0; sid < nsites; sid++ )
  {
    /*-------------------------------------*/
    /* get the voronoi region for the site */
    /*-------------------------------------*/
    nverts = find_vregion( sid, pverts );
    if ( nverts > MAX_PVERTS )
      G_fatal_error( "Maximum number of vertices exceeded." );

#ifdef DEBUG
    plot_polygon( "red", pverts, nverts );
#endif

    /*--------------------------------------------*/
    /* calculate and store area of voronoi region */
    /*--------------------------------------------*/
    polygon_area( pverts, nverts, &(area[sid]) );
  }
}

/*==========================================================================*/
polygon_area( poly, nverts, total_area )
   float   poly[][2];
   int     nverts;
   double *total_area;
{
   int    i, next, prev, last;

   *total_area = 0.0;
   last = nverts - 1;

   for( i=0; i<nverts; i++ )
   {
      prev = i-1;
      next = i+1;
      if ( i == 0 )
         prev = last;
      if ( i == last )
         next = 0;
     *total_area += poly[i][1] * (poly[prev][0] - poly[next][0]);
   }
 
   /*--------------*/
   /* polygon area */
   /*--------------*/
   *total_area *= -0.5;
}   
 
/*============================================================================*/
plot_polygon( color, poly, nverts )
   char   *color;
   float   poly[][2];
   int     nverts;
{
   int i, j;
   int line_color;

   line_color = D_translate_color( color );
   R_standard_color( line_color );
   for( i=0; i<nverts; i++)
   {
      if ((j = i + 1) == nverts)
        j = 0;

      G_plot_line( poly[i][0], poly[i][1],
                   poly[j][0], poly[j][1] );
      R_flush();
   }
}
