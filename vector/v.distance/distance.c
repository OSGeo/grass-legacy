/* bugfixes 2/2002: Stefano Menegon & Markus Neteler */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "local_proto.h"

/*#define DEBUG */

int distance (char **coords, struct Map_info *Map, int type, int field, double max) 
{
    int    i;
    double x, y, n, e;
    double px, py, dist, sdist, ldist; 
    int    cat, seg;
    register int line;
    char buf[1024],text[1024];
    struct line_pnts *Points;
    struct line_cats *Cats;

    /* TODO 3D */
    Points=Vect_new_line_struct();
    Cats = Vect_new_cats_struct ();

    line = 0;

    if(!coords && isatty(0))
	fprintf (stderr, "enter points, optionally a label to be printed. \"end\" to quit\n");
    
    i = 0;
    while (1)
    {
        G_debug (1, "i = %d", i);
        /* interactive mode: reading from kbd */
	if(!coords && isatty(0))
	{
	    fprintf (stdout, "\neast north >  ");
	    fflush(stdout);
	}
	if(!coords)
	{
	  if (fgets(buf,1024,stdin) == NULL)
	      return -1;
	  if (strncmp (buf, "end",3) == 0)
	      return -1;
	  if (strncmp (buf, "exit",4) == 0)
	      return -1;
	}
	else
	{
	   if(!coords[i])
	       break;
	   e = atof(coords[i++]);
	   n = atof(coords[i++]);
	   /* field separator is pipe | */
	   sprintf(buf, "%f|%f", e, n);
	}
	if ( (sscanf(buf, "%lf|%lf|%s", &x, &y, text)) < 3 )
	  strcpy (text, "-");

        line = Vect_find_line (Map, x, y, 0, type, max, 0, 0);
        G_debug (1, "line = %d", line);
	
	if ( line > 0 ) {
	    Vect_read_line (Map, Points, Cats, line );
	  
	    seg = Vect_line_distance ( Points, x, y, 0, 0, &px, &py, NULL, &dist, &sdist, &ldist); 
	    /* get distance to vector */
	  
	    Vect_cat_get(Cats, field, &cat);
	    
            fprintf(stdout, "%f|%f|%s|%f|%i|%f|%f|%d|%f|%f\n", 
		              x,y,text,dist,cat,px,py,seg,sdist,ldist);
	} else {
            fprintf(stdout, "%f|%f|%s|-|-|-|-|-|-|-\n", x, y, text);
        }
	fflush(stdout);
    }

    return 0;
}

