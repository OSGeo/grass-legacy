/* %W% %G% */
/*
**  Written by R.L.Glenn  2/26/90
**  USDA, Soil Conservation Service, CGIS Division
*/
#include <ctype.h>
#include    "gis.h"
#include    "Vect.h"
#include    "export_gef.h"

#define LINE_FMT "%7s%-9.9s%12.7lf%12.7lf\n"
#define LIN2_FMT "%7s   %6d%12.7lf%12.7lf\n"

write_gef_points (Cats, ghead, fp)
    struct Categories *Cats;
    struct ghead *ghead;
    FILE *fp;
{
	int cat, line, att, proj;
	register int area_num, ii;
	char  label[100], buff[100], junk[100];
	double N, E, *X, *Y;
	int n_points;
        double lat, lon;
        P_LINE *Lines;
        P_NODE *Nodes;

/* ---------------- Executable code --------------------------- */
        fprintf (fp, "-line\n");

                     /* Cycle through all areas */
	for (area_num = 1 ; area_num <= Map.n_areas ; area_num++)
	  {            /* does "area_num" have any attributes ? */
             cat = Map.Att[Map.Area[area_num].att].cat;
             N =   Map.Att[Map.Area[area_num].att].y;
             E =   Map.Att[Map.Area[area_num].att].x;
             if (cat > 0)
               {
               if (Cats->list[cat].label)
                  sscanf (Cats->list[cat].label, "%s", label);
               else sprintf(label,"%d",cat);
               lat = N; lon = E;
               if (proj != PROJECTION_LL) do_INV(&lon,&lat);
                
               fprintf(fp,LINE_FMT,"9000330", label, lon, lat);
               }
           }  /* end of areas */

                     /* Cycle through all lines */
        for (line=1; line <= Map.n_lines; line++)
	     {
	     Lines = &(Map.Line[line]);
	            /* want only DOTS, these are SCS_GEF symbols */
	     if (Lines->type != DOT)   continue;

             att = Map.Line[line].att;
             cat = Map.Att[att].cat;
             if (Cats->list[cat].label)
                  sscanf (Cats->list[cat].label, "%s", label);
             else sprintf(label,"%d",cat);
    	     if (0 > V1_read_line (&Map, Points, Map.Line[line].offset))
	        	fprintf (stderr, "Out of Memory\n"), exit (-1);

              n_points = Points->n_points;
	      X = Points->x;
	      Y = Points->y;

              N = *(Y);
              E = *(X);
	      lon = E; lat = N;
              if (proj != PROJECTION_LL) do_INV(&lon,&lat);

              fprintf(fp,LINE_FMT,"9000439",label,lon,lat);
	      }    /* end for loop */
}
