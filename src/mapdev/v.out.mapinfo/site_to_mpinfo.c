/*
*
*  THIS FUNCTION CONVERTS GRASS SITE DATA INTO MAPINFO IMPORT FORMAT
*
*  WRITTEN BY:
*  R.L. GLENN, USDA, NRCS, 09/27/95
*/

#include "gis.h"

site_to_mpinfo (site,mpmif,mpmid,proj,nad,zone)
FILE *site;     /* POINTER TO SITE_LISTS FILE */
FILE *mpmid, *mpmif;       /* POINTER$ TO MAPINFO EXPORT FILES */
int proj;                  /* PROJECTION TYPE */
int nad;                   /* NAD TYPE */
int zone;                  /* ZONE */

{
    char *desc;
    double x,y;
    int feature = 0;
    int ncoord = 1;

    fprintf (stderr, "\nConverting GRASS sites to MAPINFO import point features\n");

    /* SEQUENTIALLY PROCESS SITES IN FILE, CONVERTING TO MAPINFO POINT DATA */
    while (G_get_site (site,&x,&y,&desc) > 0)
    {
          /* WRITE HEADER RECORDS FOR THE MIF */
        if (!feature && write_mpinfo_header (mpmif,0,proj,nad,zone) < 0 )
        {
            fprintf (stderr, "\nNot able to write MAPINFO import file\n");
            exit (-1);
        }

        feature++;
        if (write_mpinfo_coordinates (mpmif,ncoord,&x,&y,proj,1) < 0)
        {
            fprintf (stderr, "\nNot able to write MAPINFO import file\n");
            exit (-1);
        }

        fflush(mpmif);
        
            /* WRITE POINT RECORD IN THE MID */
        fprintf(mpmid,"\"%s\"\015\n",desc);
        fflush(mpmid);
    }

    fprintf (stderr, "\n%d Sites Converted\n",feature);
    return (0);
}
