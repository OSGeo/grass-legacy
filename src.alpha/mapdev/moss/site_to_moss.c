/*
*
*  THIS FUNCTION CONVERTS GRASS SITE DATA INTO MOSS IMPORT FORMAT
*
*  WRITTEN BY:
*  CHRIS EMMERICH, AUTOMETRIC INC., 10/3/89
*/

#include "gis.h"

site_to_moss (site,moss,proj)

FILE *site;     /* POINTER TO SITE_LISTS FILE */
FILE *moss;     /* POINTER TO MOSS IMPORT FILE */
int proj;       /* PROJECTION TYPE */

{
    char *desc;
    double x,y;
    int feature = 0;
    int ncoord = 1;
    int isle_flag = 0;

    printf ("\nConverting GRASS sites to MOSS import point features\n");

    /* SEQUENTIALLY PROCESS SITES IN FILE, CONVERTING TO MOSS POINT DATA */
    while (G_get_site (site,&x,&y,&desc) > 0)
    {
        feature++;
        if (write_moss_header (moss,feature,ncoord,desc,proj) < 0 || 
            write_moss_coordinates (moss,ncoord,&x,&y,isle_flag,proj) < 0)
        {
            printf ("\nNot able to write MOSS import file\n");
            exit (-1);
        }
    }

    printf ("\n%d Sites Converted\n",feature);
    return (0);
}
