#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "text.h"
#include "barscale.h"
#include "misc.h"
#include "graphics.h"
#include "fullwindow.h"
#include "parms.h"
#include "colormode.h"
#include "labels.h"
#include "local_proto.h"

#define FIELD(x) strcmp(x,field)==0
static int which_color (char *);

int do_barscales (void)
{
	FILE	*fd;

	if (barscale.other) {
		fd = fopen (barscale.other, "r");
	if (fd == NULL)
	{
	return 1;
	}
	else 
		do_barscale(fd);
	}
	return 0;
}

int 
do_barscale (FILE *fd)
{
	FILE *fd2;
    	double east = 0.0L, north = 0.0L;
	char buf[1024];
	char value[1024];
	char field[1024];
	int width;
	int length;
	int color = 0;
	int textcolor;
	float textsize;
	int textwidth;
	float interval;
	char style[10];
	char unit[10];
	double dtmp;
	int  background = 0;
	int  border = 0;

	width 	= 2;
	length  = 1;
	interval = 1.0;
	textsize = 400.0;
	textwidth = 1;

	strcpy (unit, "m");
	strcpy (style, "dash");

	if (labels.other == NULL)
	{
	labels.other = G_tempfile();
	if ((fd2 = fopen (labels.other, "w")) != NULL)
		fclose (fd2);
	}

	fd2 = fopen (labels.other, "a");
	if (fd2 == NULL)
	{
	error ("misc labels file", "", "can't open");
	return 1;
	}

	while (fgets (buf, sizeof buf, fd) )
	{

        *value = 0;
        *field = 0;
        if (sscanf (buf,"%[^:]:%[^\n]", field, value) < 1) continue;


	if (FIELD("unit"))
	{
	/* to do the unit into the la file ??*/
	   strcpy (unit, value);
	   continue;
	}

	if (FIELD("length"))
	{
	   length = atoi(value) ;
	   continue;
	}

        if (FIELD("interval"))
        {
	   interval = atof(value);
           continue;
        }
			
        if (FIELD("width"))
        {
	   width = atoi(value);
           continue;
        }

	if (FIELD("color")) {
	   color = which_color(value);
	   continue;
	}

	if (FIELD("style")) {
	   strcpy (style, value);
	   continue;
	}

	if (FIELD("north"))
        {
	   if (scan_northing (value, &dtmp))
	      north = dtmp;
           continue;
        }

        if (FIELD("east"))
        {
	   if (scan_easting (value, &dtmp))
	      east = dtmp;
           continue;
        }

	if (FIELD("textcolor"))
	{
	   textcolor = which_color(value);
	   fprintf (fd2, "color: %d\n", textcolor);
	   fprintf (fd2, "border: none\n");
	   fprintf (fd2, "background: none\n");
	   fprintf (fd2, "opaque: yes\n");
	   fprintf (fd2, "rotation: 0\n");
	   continue;
	}

	if (FIELD("textsize"))
	{
	   scan_resolution (value, &dtmp);
	   textsize = dtmp;
	   fprintf (fd2, "size: %8.2f\n", textsize);
	   continue;
	}

	if (FIELD("textwidth"))
	{
	   textwidth = atoi(value);
	   fprintf (fd2, "width: %d\n", textwidth);
	   continue;
	}

	if (FIELD("font"))
	{
	   G_strip(value);
	   select_font(value);
	   fprintf (fd2, "font: %s\n", value);
	   continue;
	}


	if (FIELD("background"))
	{
	    background= which_color(value);
	    continue;
	}

	if (FIELD("border"))
	{
	    border= which_color(value);
	    continue;
	}

	if (FIELD("end"))
	{
           set_color (color);
           set_width (1);


/* draw scale */

	   if (strcmp (style, "dash") == 0) {
	      draw_barscale(fd2, width,length,interval,unit,textsize, east, north,0,color,background,border);
	   }

	   if (strcmp (style, "tick") == 0) {
	      draw_barscale(fd2, width,length,interval,unit,textsize, east, north, 1,color,background,border);
	   }
	   continue;
	}

	}

	fclose(fd2);
	return 0;
}



static int which_color (char *value)
{
int n;
int r,g,b;

if (!scan_color (value, &n,&r,&g,&b))
n = -1;
return n;
}

