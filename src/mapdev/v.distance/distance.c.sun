#include <stdio.h>
#include <math.h>
#include "gis.h"
#include "dig_structs.h"
#include "dig_defines.h"
#include "Vect.h"


distance(Map)
    struct Map_info *Map;

{
    double x, y, d,d2;
    double x1,y1,x2,y2;
    double s,n,w,e;
    register int line,vline,seg;
    register int np, i, nlines; 
    char buf[1024],text[1024];
    struct line_pnts *Points;

    Points=Vect_new_line_struct();

    line = 0;

    if (isatty(0))
	fprintf (stderr, "enter points, \"end\" to quit\n");
    while (1)
    {
	if (isatty(0))
	    fprintf (stderr, "\neast north >  ");
	if (gets(buf) == NULL) {
              return;
	}
	if (strcmp (buf, "end") == 0) {
              return;
	}
	if (strcmp (buf, "exit") == 0) {
              return;
	}
	line++;
	sprintf(text,"\0");
	sscanf(buf, "%lf %lf %s", &x, &y, text);
        sprintf(buf,"\0");
      	nlines=V2_num_lines(Map);	
	V2_read_line(Map,Points,1);

	seg=dig_check_dist(Map,1,x,y,&d);
	d=sqrt(d);
	for(vline=1;vline<=nlines;vline++)
		{
                    if (LINE_ALIVE (&(Map->Line[vline])))
                      {                        
				seg=dig_check_dist(Map,vline,x,y,&d2);
				d2=sqrt(d2);
				if(d2<d) d=d2;

		      }


/*
printf("Next Line\n");
*/
		}
	printf("%lf|%lf|%s|%lf\n",x,y,text,d);
	}
}
