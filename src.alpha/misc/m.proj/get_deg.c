/* @(#)get_deg.c	3.1.2.3 3/25/93 */
/* get_deg.c    1.0   6/182/91
*    Created by : R.L.Glenn , Soil Conservation Service, USDA
*    Purpose: function
*			Provide a means of collecting user lat/long
*			data, in different formats; convert to
*                       decimal degrees
*    Input arguements : lat or long string   and
*                       a 1 for latitude or 0 for longitude
*    Output arguements: decimal degrees in string
*
*		Note: All functions are callable directly see
*			g.get_stp	g.get_fips	g.stp_proj  geo
*
*/
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <sys/types.h>
#include "gis.h"


int get_deg(strng, degrees, ll_swt)
char *strng;
double *degrees;
int ll_swt;
{

   if(ll_swt == 0)
   {
      if(!G_scan_easting(strng, degrees, PROJECTION_LL))
      {
         fprintf(stderr,
        "\n\t** %s is invalid for longitude **\n", strng);
         sleep(2);
         return(0);
      }
   }
   else
   {
      if(G_scan_northing(strng, degrees, PROJECTION_LL) == 0)
      {
         fprintf(stderr,
      "\n\t** %s is invalid for latitude **\n", strng);
         sleep(2);
         return(0);
      }
   }
         sprintf(strng,"%.10lf",*degrees);
         return(1);
}
