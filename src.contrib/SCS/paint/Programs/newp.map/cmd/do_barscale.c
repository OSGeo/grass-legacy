
#include "gis.h"
#include "text.h"
#include "barscale.h"
#include "misc.h"
#include "fullwindow.h"
#include "parms.h"
#include "colormode.h"
#include "labels.h"

#define FIELD(x) strcmp(x,field)==0

do_barscales(pcolr)
struct Colors *pcolr;

{
	FILE	*fd;

	if (barscale.other) {
		fd = fopen (barscale.other, "r");
	if (fd == NULL)
	{
	return;
	}
	else 
		do_barscale(fd);
	}
}

do_barscale(fd)
    FILE *fd;
{
	FILE *fd2;
    double east, north;
	double ieast, inorth;
	char buf[1024];
	char value[1024];
	char field[1024];
	int width;
	int length;
	int color;
	int textcolor;
	float textsize;
	int background;
	int border;
	int interval;
	char style[10];
	char unit[10];
	double dtmp;
	char text[50];

	width 	= 2;
	length  = 1;
	interval = 1;
	textsize = 400.0;

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
	return;
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
		}

		if (FIELD("length"))
		{
			length = atoi(value) ;
			continue;
		}

        if (FIELD("interval"))
        {
			interval = atoi(value);
            continue;
        }

			
        if (FIELD("width"))
        {
			width	= atoi(value);
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
		inorth = dtmp;
            continue;
        }

        if (FIELD("east"))
        {
	    if (scan_easting (value, &dtmp))
		east = dtmp;
		ieast = dtmp;
            continue;
        }


		if (FIELD("textcolor"))
		{
			textcolor = which_color(value);
			fprintf (fd2, "color: %d\n", textcolor);
			fprintf (fd2, "border: none\n");
			fprintf (fd2, "background: none\n");
			fprintf (fd2, "opaque: yes\n");
			continue;
		}

		if (FIELD("textsize"))
		{
		if (scan_resolution (value, &dtmp));
			textsize = dtmp;
			fprintf (fd2, "size: %8.2f\n", textsize);

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
			background	= which_color(value);
			continue;
		}

		if (FIELD("border"))
		{
			border = which_color(value);
			continue;
		}

	if (FIELD("end"))
	{
	int i, j;
	int x, y;

    set_color (color);
    set_width (1);


/* draw scale */

	if (strcmp (style, "dash") == NULL) {
	draw_barscale(fd2, width,length,interval,unit, east, north,0);
	}

	if (strcmp (style, "tick") == NULL) {
	draw_barscale(fd2, width,length,interval,unit, east, north, 1);
	}



	continue;
}



	}


	fclose(fd2);
}





static
which_color (value)
char *value;
{
int n;
int r,g,b;

if (!scan_color (value, &n,&r,&g,&b))
n = -1;
return n;
}

