/*
*
*  THIS FUNCTION CONVERTS GRASS POINT DATA (SITE DATA IN DIGIT FILES) INTO 
*  MOSS IMPORT FORMAT
*
*  WRITTEN BY:
*  CHRIS EMMERICH, AUTOMETRIC INC., 10/3/89
*/

#include  "digit.h"
#include  "gis.h"

point_to_moss (map,pcats,moss,proj)

struct Map_info *map;      /* SPATIAL INFO FOR THE DIGIT FILE */
struct Categories *pcats;  /* CATEGORY INFO FOR THE DIGIT FILE */
FILE *moss;                /* POINTER TO MOSS IMPORT FILE */
int proj;                  /* PROJECTION TYPE */

{
    char *text;

    double *x,*y;

    int category,line,nlines,type;
    int feature = 0;
    int ncoord = 1;
    int isle_flag = 0;

    struct line_pnts *points;

    printf 
    ("\nConverting GRASS points (digit sites) to MOSS import point features\n");
 
    /* GET NUMBER OF LINES, AND PROCESS THEM SEQUENTIALLY */
    nlines = dig_P_num_lines (map);

    for (line = 1; line <= nlines; line++)
    {
        if ((type = dig_P_read_line (map,line,&points)) < 0)
        {
            printf ("\nNot able to read line %d\n",line);
            exit (-1);
        }

        /* PROCESS ITEM IF IT IS A POINT */
        if (type == DOT)
        {
            /* GET POINT ATTRIBUTE */
            category = dig_P_line_att (map,line);

            x = points->x;
            y = points->y;

            /* GET CATEGORY TEXT */
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
    }  /* END LOOP */

    printf ("\n%d Points Converted\n",feature);
    return (0);
}
