
/*
*  Read all the registration (map) coordinates in from the file.
*/

#include    <stdio.h>
#include    "trans.h"

get_coor_from_file (fp)
    FILE    *fp;
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



