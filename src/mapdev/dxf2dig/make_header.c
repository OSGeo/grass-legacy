#include "dxf2vect.h"
#include "gis.h"


dxf_make_header (vect_file)
FILE	*vect_file;
{
    char *date; 
    char *name;

    /* ORGANIZATION NAME DEFAULT IS USED */
    fprintf (vect_file, "ORGANIZATION: US Army Const. Eng. Rsch. Lab\n");

    /* CALCULATE TODAY'S DATE */
    if ((date = G_date()) != NULL)
    {
	fprintf (vect_file, "DIGIT DATE:   %s\n",date);
    }
    else /* IF G_date RETURNS A NULL */
    {
	fprintf (vect_file, "DIGIT DATE:     \n"); 
    }

    /* PRINT OUT THE USER'S NAME */
    if ((name = G_whoami()) != NULL)
	fprintf (vect_file, "DIGIT NAME:   %s\n",name);
    else
	fprintf (vect_file, "DIGIT NAME:     \n");
    fprintf (vect_file, "MAP NAME:     %s\n",dxf_file);
    fprintf (vect_file, "MAP DATE:      \n");
    fprintf (vect_file, "MAP SCALE:     2400\n");
    fprintf (vect_file, "OTHER INFO:    \n");
    fprintf (vect_file, "ZONE:          \n");
    fprintf (vect_file, "WEST EDGE:    ");
    w_off = ftell (vect_file);
    fprintf (vect_file, "%-60s\n", "0");
    fprintf (vect_file, "EAST EDGE:    ");
    e_off = ftell (vect_file);
    fprintf (vect_file, "%-60s\n", "0");
    fprintf (vect_file, "SOUTH EDGE:   ");
    s_off = ftell (vect_file);
    fprintf (vect_file, "%-60s\n", "0");
    fprintf (vect_file, "NORTH EDGE:   ");
    n_off = ftell (vect_file);
    fprintf (vect_file, "%-60s\n", "0");
    fprintf (vect_file, "MAP THRESH:   00.00\n");
    fprintf (vect_file, "VERTI:         \n");
}


