/*
*
*  THIS FUNCTION WRITES THE MAPINFO IMPORT COORDINATE RECORDS
*  FOR A POINT, LINE, POLYGON. 
*
*  WRITTEN for MOSS BY:
*  CHRIS EMMERICH, AUTOMETRIC INC., 10/3/89
*  Modified BY:
*  R.L. Glenn, USDA, NRCS, 9/19/95
*/

#include <stdio.h>
#define LATLONG 3

write_mpinfo_coordinates (mpinfo,ncoord,x,y,proj,type)

FILE *mpinfo;       /* POINTER TO MAPINFO IMPORT FILE */
int ncoord;       /* NUMBER OF COORDINATE PAIRS IN FEATURE */
double x[],y[];   /* COORDINATE ARRAYS */
int proj;         /* PROJECTION TYPE */
int type;         /* FEATURE TYPE */

{
    int ret,ipnt;


    /* WRITE POINT HEADER */
    if (type == 1) 
	  fprintf (mpinfo,"Point "); 

    /* WRITE THE COORDINATE RECORDS */
    for (ipnt = 0; ipnt < ncoord; ipnt++)
    {
/*
fprintf (stderr,"%14.5f%14.5f\015\n",x[ipnt],y[ipnt]); fflush(stderr);
*/
       if(proj == LATLONG)
	  ret = fprintf (mpinfo,"%10.5f%10.5f\015\n",x[ipnt],y[ipnt]); 
       else
	  ret = fprintf (mpinfo,"%14.5f%14.5f\015\n",x[ipnt],y[ipnt]); 

       if (ret < 0) break;
    }
    
    /* WRITE THE POINT TRAILER */
    if (type == 1)
          ret = fprintf (mpinfo,"    Symbol (49,0,12) \015\n");
    /* WRITE THE LINE TRAILER */
    if (type == 2)
          ret = fprintf (mpinfo,"    Pen (1,2,0) \015\n");

    return (ret);
}
