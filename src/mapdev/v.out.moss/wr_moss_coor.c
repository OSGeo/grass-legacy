/*
*
*  THIS FUNCTION WRITES THE MOSS IMPORT COORDINATE RECORDS
*  FOR A POINT, LINE, POLYGON, OR ISLAND.  THERE ARE TWO
*  FORMATS: LAT/LONG DATA, AND ALL OTHER TYPES.  IF THE COORDINATE
*  STRING IN X,Y DEFINES AN ISLAND, IT WILL BE IN COUNTERCLOCKWISE
*  ORDER.  AN ISLAND IS DELINEATED IN LAT/LONG BY A VALUE OF 1
*  IN THE 3RD FIELD OF ITS 1ST COORDINATE RECORD.  FOR
*  OTHER DATA TYPES, AN ISLAND IS DELINEATED BY A NEGATIVE X
*  VALUE IN ITS 1ST COORDINATE RECORD.
*
*  WRITTEN BY:
*  CHRIS EMMERICH, AUTOMETRIC INC., 10/3/89
*/

#include <stdio.h>
#define LATLONG 3

write_moss_coordinates (moss,ncoord,x,y,isle_flag,proj)

FILE *moss;       /* POINTER TO MOSS IMPORT FILE */
int ncoord;       /* NUMBER OF COORDINATE PAIRS IN FEATURE */
double x[],y[];   /* COORDINATE ARRAYS */
int isle_flag;    /* ISLAND FLAG */
int proj;         /* PROJECTION TYPE */

{
    int ret,ipnt;

    /* WRITE THE COORDINATE RECORDS */
    for (ipnt = 0; ipnt < ncoord; ipnt++)
    {
       if (proj == LATLONG)
       { 
          ret = fprintf (moss,"%10.5f%10.5f%2d\n",x[ipnt],y[ipnt],isle_flag); 
       }
       else
       {
          if (isle_flag)
             ret = fprintf (moss,"%11.2f%11.2f\n",-x[ipnt],y[ipnt]);
          else
             ret = fprintf (moss,"%11.2f%11.2f\n",x[ipnt],y[ipnt]);
       }

       if (ret < 0) break;
       if (isle_flag) isle_flag = 0;   /* ONLY 1ST RECORD USED TO FLAG ISLANDS */
    }
    
    return (ret);
}
