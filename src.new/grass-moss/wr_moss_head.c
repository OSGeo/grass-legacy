/*
*
*  THIS FUNCTION WRITES A MOSS IMPORT HEADER RECORD.
*
*  WRITTEN BY:
*  CHRIS EMMERICH, AUTOMETRIC INC., 10/3/89
*/

#include <stdio.h>
#define LATLONG 3

write_moss_header (moss,feature,ncoord,attr,proj)

FILE *moss;     /* POINTER TO MOSS IMPORT FILE */
int feature;    /* MOSS FEATURE NUMBER */
int ncoord;     /* NUMBER OF COORDINATE PAIRS IN FEATURE */
char *attr;     /* FEATURE ATTRIBUTE (SUBJECT) */
int proj;       /* PROJECTION TYPE */

{
    int ret;

    if (proj == LATLONG)
    /* NEGATIVE FEATURE NUMBER SIGNIFIES LAT/LONG DATA */
       ret = fprintf (moss,"%5d          %-30s     %5d\n",-feature,attr,ncoord);
    else
       ret = fprintf (moss,"%5d          %-30s     %5d\n",feature,attr,ncoord);
    return (ret);
}
