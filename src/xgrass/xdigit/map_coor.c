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

    int    i,ju;
    char  buff[85];


#ifdef LATLON
  if (ll_flag) {
    for ( i=0;  i<MAX_COOR; i++  )
     {

	if ( fgets (buff, sizeof (buff), fp) == NULL)
	    break;
	if ( sscanf (buff, "%s %s %d", bcx[i], bcy[i], &ll_flag)  !=  3)
	 {
	    if ( sscanf (buff, "%s %s", bcx[i], bcy[i])  !=  2) {
	      write_info (1, " ERROR:  Reading coordinates.");
	      sleep (3);
	      break;
            }
            else ll_flag = 0;   /* file was created with old version */
	 }
     }	/*  for (i)  */
  }
  else
#endif
    for ( i=0;  i<MAX_COOR; i++  )
     {

	if ( fgets (buff, sizeof (buff), fp) == NULL)
	    break;
	if ( sscanf (buff, "%lf %lf %d", &bx[i], &by[i],&ju)  !=  3) {
         if ( sscanf (buff, "%lf %lf", &bx[i], &by[i])  !=  2)
	 {
	    write_info (1, " ERROR:  Reading coordinates.");
	    sleep (3);
	    break;
	 }
       }
     }	/*  for (i)  */

    return (i);

}	/*    load_coor_from_file ()   */




save_coor_to_file (fp) 
    FILE    *fp;
{
    register    int    i;


    fseek (fp, 0L, 2) ;	/*  seek to eof  */
#ifdef LATLON
  if (ll_flag) {

    for ( i=0;  i<MAX_COOR;  i++)
	    if ((strncmp( bcx[i],"0.0",3)==0) || (strncmp(bcy[i],"0.0",3)==0)
               || (bcx[i][0]=='\0') || (bcy[i][0]=='\0'))
		continue ;
	    else
	    	fprintf (fp, "%s %s %d\n", bcx[i], bcy[i], ll_flag);
  }
  else
#endif
    for ( i=0;  i<MAX_COOR;  i++)
	    if (bx[i] == 0.0 || by[i] == 0.0)
		continue ;
	    else
	    	fprintf (fp, "%lf %lf 0\n", bx[i], by[i]);

}	    /*  save_coor_to_file ()  */
