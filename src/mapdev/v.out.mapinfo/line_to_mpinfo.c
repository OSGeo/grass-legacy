/*
*
*  THIS FUNCTION CONVERTS GRASS LINE DATA INTO MAPINFO IMPORT FORMAT
*
*  WRITTEN BY:
*  CHRIS EMMERICH, AUTOMETRIC INC., 10/3/89
*/

#include  "Vect.h"
#include  "gis.h"

line_to_mpinfo (map,pcats,mpmif,mpmid,proj,nad,zone)

struct Map_info *map;      /* SPATIAL INFO FOR THE DIGIT FILE */
struct Categories *pcats;  /* CATEGORY INFO FOR THE DIGIT FILE */
FILE *mpmid, *mpmif;       /* POINTER$ TO MAPINFO EXPORT FILES */
int proj;                  /* PROJECTION TYPE */
int nad;                   /* NAD TYPE */
int zone;                  /* ZONE */

{
    char resp[2],*text;
    double *x,*y;

    int arcflag,category,line,nlines,ncoord,type;
    int feature = 0;
    int isle_flag = 0;

    struct line_pnts *Points;

	Points = Vect_new_line_struct();

    fprintf (stderr, "\nConvert area edges as lines (y/n) [n]? ");
    fgets(resp,80,stdin);
    if (resp[0] == 'Y' || resp[0] == 'y') 
    {
        arcflag = 1;
        fprintf (stderr, "\nConverting GRASS lines and area edges to MAPINFO import line features\n");
    }
    else
    {
        arcflag = 0;
        fprintf (stderr, "\nConverting GRASS lines to MAPINFO import line features\n");
    }

    /* GET NUMBER OF LINES, AND PROCESS THEM SEQUENTIALLY */
    nlines = V2_num_lines (map);

    for (line = 1; line <= nlines; line++)
    {
        if ((type = V2_read_line (map, Points, line)) < 0)
        {
            fprintf (stderr, "\nNot able to read line %d\n",line);
            exit (-1);
        }

        /* PROCESS ITEM IF IT IS THE RIGHT TYPE (MUST BE A LINE, AN AREA EDGE 
         * WITH A LINE ATTRIBUTE, OR EITHER TYPE IF THE USER HAS SPECIFIED
         * THAT AREA EDGES WILL BE CONVERTED AS LINES). */

        if (type == LINE || type == AREA)
        {
            /* GET LINE ATTRIBUTE */
            category = V2_line_att (map,line);

            if (type == LINE || arcflag || category > 0)
            {
                x = Points->x;
                y = Points->y;
                ncoord = Points->n_points;

                /* GET CATEGORY TEXT (IF AREA EDGE, FORCE TEXT TO NULL) */
                if (category == 0)
                   text = "";
                else
                   text = G_get_cat (category,pcats);

                /* WRITE MAPINFO IMPORT HEADER */
	        if (!feature && write_mpinfo_header (mpmif,3,proj,nad,zone) < 0)
                {
                    fprintf (stderr, "\nNot able to write MAPINFO import file\n");
                    exit (-1);
                }

                /* WRITE LINE TYPE HEADER */
                fprintf (mpmif,"Pline %d\015\n",ncoord);

                /* WRITE MAPINFO IMPORT RECORDS FOR THIS LINE */
                feature++;
                if (write_mpinfo_coordinates (mpmif,ncoord,x,y,proj,2) < 0)
                {
                    fprintf (stderr, "\nNot able to write MAPINFO import file\n");
                    exit (-1);
                }
                fflush(mpmif);
        
                /* WRITE POINT RECORD IN THE MID */
                fprintf(mpmid,"%d,\"%s\"\015\n",category,text);
                fflush(mpmid);

            }
        }
    }  /* END LOOP */

	Vect_destroy_line_struct (Points);
    fprintf (stderr, "\n%d Lines Converted\n",feature);
    return (0);
}
