/*
****************************************************************************
*
* MODULE:       v.transform
* AUTHOR(S):    See other files as well...
*               Eric G. Miller <egm2@jps.net>
* PURPOSE:      To transform a vector layer's coordinates via a set of tie
*               points.
* COPYRIGHT:    (C) 2002 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/*
*  Read all the registration (map) coordinates in from the file.
*/

#include    <stdio.h>
#include    "trans.h"

int 
get_coor_from_file (FILE *fp)
{

    int    i;
    char  buff[128];

    for ( i=0;  i<MAX_COOR; i++ )
     {

	if ( fgets (buff, sizeof (buff), fp) == NULL)
	    break;

	if ( sscanf (buff, "%lf %lf %lf %lf", &ax[i], &ay[i], &bx[i], &by[i])  !=  4)
	 {
	    fprintf (stderr, " ERROR:  Reading coordinates from file\n.");
	    return (-1);
	 }
	use[i] = 1 ;

     }	/*  for (i)  */

    return (i);

}	/*    get_coor_from_file ()   */



