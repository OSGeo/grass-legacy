/*
*
*  THIS FUNCTION CONVERTS GRASS POINT DATA (SITE DATA IN DIGIT FILES) INTO 
*  MAPINFO IMPORT FORMAT
*
*  WRITTEN BY:
*  CHRIS EMMERICH, AUTOMETRIC INC., 10/3/89
*
*  MODIFIED BY:
*  R.L. GLENN, USDA, NRCS, 09/27/95
*/

#include  "Vect.h"
#include  "gis.h"

point_to_mpinfo (map,pcats,mpmif,mpmid,proj,nad,zone)

struct Map_info *map;      /* SPATIAL INFO FOR THE DIGIT FILE */
struct Categories *pcats;  /* CATEGORY INFO FOR THE DIGIT FILE */
FILE *mpmid, *mpmif;       /* POINTER$ TO MAPINFO EXPORT FILES */
int proj;                  /* PROJECTION TYPE */
int nad;                   /* NAD TYPE */
int zone;                  /* ZONE */

{
    char *text;

    double *x,*y;

    int category,line,nlines,type;
    int feature = 0;
    int ncoord = 1;
    int isle_flag = 0;

    struct line_pnts *Points;

	Points = Vect_new_line_struct();

    fprintf (stderr, "\nConverting GRASS points (digit sites) to MAPINFO import point features\n");
 
    /* GET NUMBER OF LINES, AND PROCESS THEM SEQUENTIALLY */
    nlines = V2_num_lines (map);

    for (line = 1; line <= nlines; line++)
    {
        if ((type = V2_read_line (map, Points, line)) < 0)
        {
            fprintf (stderr, "\nNot able to read line %d\n",line);
            exit (-1);
        }

        /* PROCESS ITEM IF IT IS A POINT */
        if (type == DOT)
        {
            /* GET POINT ATTRIBUTE */
            category = V2_line_att (map,line);

            x = Points->x;
            y = Points->y;

            /* GET CATEGORY TEXT */
            text = G_get_cat (category,pcats);

            /* WRITE MAPINFO IMPORT HEADER RECORD */
	    if (!feature && write_mpinfo_header (mpmif,3,proj,nad,zone) < 0)
            {
                fprintf (stderr, "\nNot able to write MAPINFO import file\n");
                exit (-1);
            }

            /* WRITE MAPINFO IMPORT RECORDS FOR THIS LINE */
            feature++;
            if (write_mpinfo_coordinates (mpmif,ncoord,x,y,proj,1) < 0)
            {
                fprintf (stderr, "\nNot able to write MAPINFO import file\n");
                exit (-1);
            }
            fflush(mpmif);
        
                /* WRITE POINT RECORD IN THE MID */
            fprintf(mpmid,"%d,\"%s\"\015\n",category,text);
            fflush(mpmid);
         }
    }  /* END LOOP */

	Vect_destroy_line_struct (Points);

    fprintf (stderr, "\n%d Points Converted\n",feature);
    return (0);
}
