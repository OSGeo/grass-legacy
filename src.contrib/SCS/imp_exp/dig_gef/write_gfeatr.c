/* %W% %G% */
/*
**  Written by R.L.Glenn  2/26/90
**  USDA, Soil Conservation Service, CGIS Division
*/
#include    "gis.h"
#include    "Vect.h"
#include    "export_gef.h"

#define LINE_FMT "%7s%-9.9s%-9.9s%5d%5d%5d\n"
#define COOR_FMT "                %12.7lf%12.7lf\n"

write_gef_featrs ( Cats, ghead, fp)
    struct Categories *Cats;
    struct ghead *ghead;
    FILE *fp;
{
	int cnt, cat, att, line, proj;
	register int i;
	char  label[15], buff[100];
	double N, E, *X, *Y;
	int n_points;
        double lat, lon;
        P_LINE *Lines;
        P_NODE *Nodes;

/* ---------------- Executable code --------------------------- */
 
        fprintf (fp, "-text\n");

        for (line=1; line <= Map.n_lines; line++)
	     {
	     Lines = &(Map.Line[line]);
	            /* skip area lines */
	     if (Lines->type != LINE)   continue;
             if (line == 1)
                       fprintf (stdout,"Writing Feature information\n");
             att = Map.Line[line].att;
             cat = Map.Att[att].cat;
	     if (Cats->list[cat].label)
		    sscanf (Cats->list[cat].label, "%s", label);
             else sprintf(label,"%d",cat);

             if (0 > V1_read_line (&Map, Points,  Map.Line[line].offset))
	        	fprintf (stderr, "Out of Memory\n"), exit (-1);

              n_points = Points->n_points;
	      X = Points->x;
	      Y = Points->y;
             fprintf (fp, LINE_FMT, 
                               "9000435",label,"         ",0,0,n_points);

	     while (n_points--)
                  {
                  if (proj == PROJECTION_LL)
                     {
                     lat = *(Y++); lon = *(X++);
                     }
                  else
                     {
                     N = *(Y++);
                     E = *(X++);
                     lon = E; lat = N;
                     do_INV(&lon,&lat);
                     }
                  fprintf (fp, COOR_FMT, lon, lat);
                  }
	     }    /* end for loop */

}
