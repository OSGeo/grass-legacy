/*
*
*  THIS FUNCTION CONVERTS GRASS LINE DATA INTO MOSS IMPORT FORMAT
*
*  WRITTEN BY:
*  CHRIS EMMERICH, AUTOMETRIC INC., 10/3/89
*/

#include  "digit.h"
#include  "gis.h"

line_to_moss (map,pcats,moss,proj)

struct Map_info *map;      /* SPATIAL INFO FOR DIGIT FILE */
struct Categories *pcats;  /* CATEGORY INFO FOR DIGIT FILE */
FILE *moss;                /* POINTER TO MOSS IMPORT FILE */
int proj;                  /* PROJECTION TYPE */

{
    char resp[2],*text;
    double *x,*y;

    int arcflag,category,line,nlines,ncoord,type;
    int feature = 0;
    int isle_flag = 0;

    struct line_pnts *points;

    printf ("\nConvert area edges as lines (y/n) [n]? ");
    gets(resp);
    if (resp[0] == 'Y' || resp[0] == 'y') 
    {
        arcflag = 1;
        printf 
      ("\nConverting GRASS lines and area edges to MOSS import line features\n");
    }
    else
    {
        arcflag = 0;
        printf 
        ("\nConverting GRASS lines to MOSS import line features\n");
    }

    /* GET NUMBER OF LINES, AND PROCESS THEM SEQUENTIALLY */
    nlines = dig_P_num_lines (map);

    for (line = 1; line <= nlines; line++)
    {
        if ((type = dig_P_read_line (map,line,&points)) < 0)
        {
            printf ("\nNot able to read line %d\n",line);
            exit (-1);
        }

        /* PROCESS ITEM IF IT IS THE RIGHT TYPE (MUST BE A LINE, AN AREA EDGE 
         * WITH A LINE ATTRIBUTE, OR EITHER TYPE IF THE USER HAS SPECIFIED
         * THAT AREA EDGES WILL BE CONVERTED AS LINES). */

        if (type == LINE || type == AREA)
        {
            /* GET LINE ATTRIBUTE */
            category = dig_P_line_att (map,line);

            if (type == LINE || arcflag || category > 0)
            {
                x = points->x;
                y = points->y;
                ncoord = points->n_points;

                /* GET CATEGORY TEXT (IF AREA EDGE, FORCE TEXT TO NULL) */
                if (category == 0)
                   text = "";
                else
                   text = G_get_cat (category,pcats);

                /* WRITE MOSS IMPORT RECORDS FOR THIS LINE */
                feature++;
                if (write_moss_header (moss,feature,ncoord,text,proj) < 0 ||
                    write_moss_coordinates (moss,ncoord,x,y,isle_flag,proj) < 0)
                {
                    printf ("\nNot able to write MOSS import file\n");
                    exit (-1);
                }
            }
        }
    }  /* END LOOP */

    printf ("\n%d Lines Converted\n",feature);
    return (0);
}
