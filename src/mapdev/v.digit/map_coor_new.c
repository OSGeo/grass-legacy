/*
*  Read all the registration (map) coordinates in from the file.
*  Save coordinates to file.
*/

#include    <stdio.h>
#include "digit.h"
#include    "map.h"

load_coor_from_file (fp)
    FILE    *fp;
{

    int    i;
    char  buff[85];


#ifdef LATLON
    for ( i=0;  i<MAX_COOR; i++  )
     {

	if ( fgets (buff, sizeof (buff), fp) == NULL)
	    break;
	if ( sscanf (buff, "%s %s %d", bcx[i], bcy[i], &ll_flag)  !=  3)
	 {
	    if ( sscanf (buff, "%s %s", bcx[i], bcy[i])  !=  2) {
	      Write_info (1, " ERROR:  Reading coordinates.");
	      sleep (3);
	      break;
            }
            else ll_flag = 0;   /* file was created with old version */
	 }
/* changes 4-21-93 */
         if (ll_flag == 0) {
	    if ( sscanf (buff, "%lf %lf", &bx[i], &by[i])  !=  2) {
	      Write_info (1, " ERROR:  Reading coordinates.");
	      sleep (3);
	      break;
            }
         }
/* end of changes */
     }	/*  for (i)  */
#else
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
#endif

    return (i);

}	/*    load_coor_from_file ()   */




save_coor_to_file (fp) 
    FILE    *fp;
{
    register    int    i;


    fseek (fp, 0L, 2) ;	/*  seek to eof  */
#ifdef LATLON

    for ( i=0;  i<MAX_COOR;  i++)
	    if ((strncmp( bcx[i],"0.0",3)==0) || (strncmp(bcy[i],"0.0",3)==0)
               || (bcx[i][0]=='\0') || (bcy[i][0]=='\0'))
		continue ;
	    else
	    	fprintf (fp, "%s %s %d\n", bcx[i], bcy[i], ll_flag);
#else
    for ( i=0;  i<MAX_COOR;  i++)
	    if (bx[i] == 0.0 || by[i] == 0.0)
		continue ;
	    else
	    	fprintf (fp, "%lf %lf\n", bx[i], by[i]);
#endif

}	    /*  save_coor_to_file ()  */

