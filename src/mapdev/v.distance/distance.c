#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "local_proto.h"


int distance (char **coords, struct Map_info *Map) 
{
    double x, y, d,d2;
    double n,e;
    int vectATT_L;
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
	V2_read_line(Map,Points,1);

	seg=dig_check_dist(Map,1,x,y,&d);
	d=sqrt(d);
	vectATT_L=0;
	for(vline=1;vline<=nlines;vline++)
	{
	    if (LINE_ALIVE (&(Map->Line[vline])))
	    {                        
		seg=dig_check_dist(Map,vline,x,y,&d2);
		d2=sqrt(d2);
		if(d2<d)
		{
		 d=d2;
		 vectATT_L=V2_line_att (Map, vline);
		}
	    }

/*
printf("Next Line\n");
*/
	}
    printf("%f|%f|%s|%f|%i\n",x,y,text,d,vectATT_L);
    }

    return 0;
}
