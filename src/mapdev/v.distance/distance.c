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

int distance (char **coords, struct Map_info *Map) 
{
    double x, y, d,d2;
    double n,e;
    int vectATT_L, closestV_ID;
    register int line,vline,seg;
    register int i, nlines; 
    char buf[1024],text[1024];
    struct line_pnts *Points;

    Points=Vect_new_line_struct();

    line = 0;

    if(!coords && isatty(0))
	fprintf (stderr, "enter points, optionally a label to be printed. \"end\" to quit\n");
    
    i = 0;
    while (1)
    {
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
	 sprintf(buf, "%f %f", e, n);
	}
	line++;
	sscanf(buf, "%lf %lf %s", &x, &y, text);

	nlines=V2_num_lines(Map);	
	/* we need to initialize for dig_check_dist*/
	V2_read_line(Map,Points,1);

	/* get closest vector segment number */
	seg=dig_check_dist(Map,1,x,y,&d);
#ifdef DEBUG
fprintf(stderr, "seg: %i ", seg);
#endif
	/* get distance to vector */
	d=sqrt(d);
#ifdef DEBUG
fprintf(stderr, "d: %f", d);
#endif

	vectATT_L=1;
	closestV_ID=1;
	/* since we don't have a function which returns the vector ID
	 * for a segment, we have to walk alonmg each vector to find out
	 * the vector ID: */
	for(vline=1;vline<=nlines;vline++)
	{
#ifdef DEBUG
fprintf(stderr, " for loop... %i\n", vline);
#endif

	    /* only check undeleted lines */
	    if (LINE_ALIVE (&(Map->Line[vline])))
	    {                        
#ifdef DEBUG
fprintf(stderr, " line found... %i\n", vline);
#endif
		/* dig_check_dist() obviously updates vline ?!*/
		seg=dig_check_dist(Map,vline,x,y,&d2);
		d2=sqrt(d2);

		if(d2<d)
		{
		 /* closest line found */
		 d=d2;
	         closestV_ID=vline; /* store vect ID */
		}
	     }
	     else
	         closestV_ID=0; /* vect was deleted, but still in map */
	}
        vectATT_L=V2_line_att (Map, closestV_ID);
        printf("%f|%f|%s|%f|%i\n",x,y,text,d,vectATT_L);
    }

    return 0;
}
