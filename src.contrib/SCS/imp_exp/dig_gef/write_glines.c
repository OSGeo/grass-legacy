/* %W% %G% */
/*
**  Written by R.L.Glenn  2/26/90
**  USDA, Soil Conservation Service, CGIS Division
*/
#include    "gis.h"
#include    "digit.h"
#include    "dig_head.h"
#include    "export_gef.h"

#define LINE_FMT "%7s%-9.9s%-9.9s%5d%5d%5d\n"
#define COOR_FMT "                %12.7lf%12.7lf\n"

write_gef_lines (Cats, ghead, fp)
    struct Categories *Cats;
    struct ghead *ghead;
    FILE *fp;
{
	int cnt, cat1, cat2, line, linea, larea, rarea, proj;
	register int area_num, i, ii;
	char  label_l[15], label_r[15], buff[100];
	double N, E, *X, *Y;
	int n_points;
        double lat, lon;
        P_LINE *Lines;
        P_NODE *Nodes;

/* ---------------- Executable code --------------------------- */
 
        fprintf (fp, "-head\n");

        for (ii=1; ii <= Map.n_lines; ii++)
	     {
	     Lines = &(Map.Line[ii]);
	            /* skip anything other than area lines */
	     if (Lines->type != AREA)   continue;

              cat1 = cat2 = 0;
              strcpy(label_l,"         ");
              strcpy(label_r,"         ");
	            /* get the category for areas left & right */
              larea = Lines->left;
              rarea = Lines->right;

              if (larea > 0) 
                 cat1 = Map.Att[Map.Area[larea].att].cat;
              else
                 {
                 larea = abs(Lines->left);
                 linea = Map.Isle[larea].area;
                 cat1 = Map.Att[Map.Area[linea].att].cat;
                 }

              if (rarea > 0) 
                    cat2 = Map.Att[Map.Area[rarea].att].cat; 
              else
                 {
                 rarea = abs(Lines->right);
                 linea = Map.Isle[rarea].area;
                 cat2 = Map.Att[Map.Area[linea].att].cat; 
                 }

              if (cat1 == 0) 
                  {
                  strcpy(label_l,"         ");
                  larea = 0;
                  }
              else 
		  {
		  if (Cats->list[cat1].label)
		    sscanf (Cats->list[cat1].label, "%s", label_l);
		  }
              if (cat2 == 0)
                  {
                  strcpy(label_r,"         ");
                  rarea = 0;
                  }
              else 
		  {
		  if (Cats->list[cat2].label)
		     sscanf (Cats->list[cat2].label, "%s", label_r);
                  }

    	      if (0 > V1_read_line (&Map, Points, Map.Line[ii].offset))
	        	fprintf (stderr, "Out of Memory\n"), exit (-1);

              n_points = Points->n_points;
	      X = Points->x;
	      Y = Points->y;
              fprintf (fp, LINE_FMT, 
                               "9000230",label_l,label_r,larea,rarea,n_points);

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

codes (type)
    char type;
{
    switch (type) {
	case LINE:
	    return ('L');
	    break;
	case AREA:
	    return ('A');
	    break;
	case DOT:
	    return ('P');
	    break;
	case DEAD_LINE:
	    return ('l');
	    break;
	case DEAD_AREA:
	    return ('a');
	    break;
	case DEAD_DOT:
	    return ('p');
	    break;
	default:
	    return ('X');
    }
}
