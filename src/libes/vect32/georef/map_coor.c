
/*
*  Read all the registration (map) coordinates in from the file.
*  Save coordinates to file.
*/

#include <stdio.h>
#include "Vect.h"
#include "map.h"

int 
load_coor_from_file (FILE *fp)
{

    int    i;
    char  buff[85];

    for ( i=0;  i<MAX_COOR; i++  )
     {

	if ( fgets (buff, sizeof (buff), fp) == NULL)
	    break;

	if ( sscanf (buff, "%lf %lf", &bx[i], &by[i])  !=  2)
	 {
	    Write_info (1, " ERROR:  Reading coordinates.");
	    sleep (3);
	    break;
	 }

     }	/*  for (i)  */

    return (i);

}	/*    load_coor_from_file ()   */




int 
save_coor_to_file (FILE *fp)
{
    register    int    i;


    fseek (fp, 0L, 2) ;	/*  seek to eof  */

    for ( i=0;  i<MAX_COOR;  i++)
	if (use[i])
	    fprintf (fp, "%lf %lf\n", bx[i], by[i]);

}	    /*  save_coor_to_file ()  */
