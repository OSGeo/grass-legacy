/* Function: do_labels
**
** This function is a much modified version of the p.map function of
** the same name.
**
** Author: Paul W. Carlson	March 1992
*/

#include <stdlib.h>
#include <string.h>
#include "ps_info.h"
#include "labels.h"
#include "local_proto.h"

#define FIELD(x) strcmp(x,field)==0
#define LEFT 0
#define RIGHT 1
#define LOWER 0
#define UPPER 1
#define CENTER 2

extern int verbose;

int 
do_labels (int other)
{
    FILE *fd;

    int i;

    if (!labels.count && labels.other == NULL) return 0;

    /* default is Helvetica font */
    set_font_name("Helvetica");

    if (!other)
    {
	for (i = 0; i < labels.count; i++)
	{
	    fd = G_fopen_old("paint/labels", labels.name[i], labels.mapset[i]); 
	
	    if (fd == NULL)
	    {
		char msg[100];
		sprintf(msg, 
		    "Can't open label file %s in %s", labels.name[i], labels.mapset[i]);
		G_warning (msg);
	    }
	    else
	    {
		if (verbose > 1)
		{
	    	    fprintf (stdout,"PS-PAINT: reading labels file <%s in %s> ...",
			labels.name[i], labels.mapset[i]);
	    	    fflush(stdout);
		}
		if (labels.font[i] != NULL) set_font_name(labels.font[i]);
    		set_font_size(10);
		do_label(fd);
		fclose(fd);
		if (verbose > 1) fprintf (stdout,"\n");
	    }
	}
    }
	
    if ( other && labels.other)
    {
	fd = fopen(labels.other, "r");
	if (fd == NULL)
	{
	    char msg[100];
	    sprintf(msg, "can't open temp label file %s", labels.other);
	    G_warning(msg);
	}
	else
	{
	    if (verbose > 1)
	    {
	        fprintf (stdout,"PS-PAINT: reading text file ...");
	        fflush(stdout);
	    }
	    do_label(fd);
	    fclose(fd);
	    if (verbose > 1) fprintf (stdout,"\n");
	}
    }

    return 0;
}

int 
do_label (FILE *fd)
{
    double east, north, dtmp, x, y;
    float size, rotate, margin;
    int xoffset, yoffset, xref, yref;
    int background, border, color, hcolor;
    double width, hwidth;
    int opaque, fontsize, multi_text, x_int, y_int;
    char field[1024];
    char value[1024];
    char ch, buf[1024];
    double atof();

    /* initialize */
    north = PS.w.north;
    east  = PS.w.west;
    opaque = 0;
    xoffset = 0;
    yoffset = 0;
    color = BLACK;
    width = 1.;
    background = WHITE;
    border = BLACK;
    hcolor = -1;
    hwidth = 0.;
    xref = CENTER;
    yref = CENTER;
    rotate = 0., 
    size = 0;

    /* read the labels file */
    while (fgets (buf, sizeof buf, fd))
    {
        *value = 0;
        *field = 0;
        if (sscanf(buf,"%[^:]:%[^\n]", field, value) < 1) continue;

        if (FIELD("text"))
        {
	    G_strip(value);

	    /* get reference coordinates */
	    /*
	    x = XCONV(east);
	    y = YCONV(north);
	    */
	    G_plot_where_xy(east, north, &x_int, &y_int);
	    x = (double) x_int / 10.;
	    y = (double) y_int / 10.;

            x += xoffset;
            y += yoffset;

	    /* set font size */
	    fontsize = size * PS.ns_to_y;
	    /*
 	    if (fontsize < 10) fontsize = 10;
 	    if (fontsize > 50) fontsize = 50;
	    */
    	    set_font_size(fontsize);

	    /* set margin to 20% of font size */
	    if (opaque || border >= 0)
	    {	
		margin = 0.2 * (double)fontsize + 0.5;
	     	/*if (margin < 2) margin = 2;*/ /* commented because
		    too big box was created for little font; RB March 2000 */			    
		if (hcolor >= 0) margin += hwidth;
	    }
	    else margin = 0;
	    fprintf(PS.fp, "/mg %.2f def\n", margin);

	    /* construct path for box */
	    multi_text = multi_lines(value);
	    if (multi_text)
	    {
		/* multiple lines - text is in PostScript array "ta" */
	    	multi_text_box_path(x, y, xref, yref, value, fontsize, rotate);
	    }
	    else
	    {
		/* single line - text is left on stack */
	    	text_box_path(x, y, xref, yref, value, fontsize, rotate);
	    }

	    if (opaque && background >= 0)
	    {
		/* fill the box */
		set_rgb_color(background);
		fprintf(PS.fp, "F ");
		opaque = 0;
	    }

	    if (border >= 0)
	    {
		/* draw the border */
		set_line_width(width ); /* added by RB, ? add bwidth option */
		set_rgb_color(border);
		fprintf(PS.fp, "D ");
		border = -1;
	    }

	    /* draw the text */
	    if (hcolor >= 0)
	    {   
		set_rgb_color(hcolor);
		set_line_width(width + 2 * hwidth);
		if (multi_text) fprintf(PS.fp, "DMH ");
		else fprintf(PS.fp, "HC ");
	    }
	    set_rgb_color(color);
	    if (multi_text) fprintf(PS.fp, "DMT ");
	    else fprintf(PS.fp, "TIB ");

	    hcolor = -1;
	    hwidth = 0.;
	    width = 1.;
            continue;
        }

        if (FIELD("color"))
        {
	    color = get_color_number(value);
            if (color < 0) color = BLACK;
            continue;
        }

        if (FIELD("hcolor"))
        {
	    hcolor = get_color_number(value);
            continue;
        }

        if (FIELD("xoffset"))
        {
            xoffset = atoi(value);
            continue;
        }

        if (FIELD("yoffset"))
        {
            yoffset = atoi(value);
            continue;
        }

        if (FIELD("ref"))
        {
	    if (!scan_ref(value, &xref, &yref))
	    {
                yref = CENTER;
                xref = CENTER;
	    }
            continue;
        }

        if (FIELD("background"))
        {
	    /*
	    if(strncmp(value, "none", 4)==0) opaque = 0;
	    */
	    background = get_color_number(value);
            continue;
        }

        if (FIELD("border"))
        {
	    border = get_color_number(value);
            continue;
        }

        if (FIELD("opaque"))
        {
	    G_strip(value);
            opaque = (strcmp(value, "no") != 0);
            continue;
        }

        if (FIELD("width"))
        {
	    ch = ' ';
 	    sscanf(value, "%lf%c", &width, &ch);
	    if(ch=='i') width = width/72.0;
	    if (width < 0.) width = 1.;
	    if (width > 5.) width = 5.;
            continue;
        }

        if (FIELD("hwidth"))
        {
	    ch = ' ';
 	    sscanf(value, "%lf%c", &hwidth, &ch);
	    if(ch=='i') hwidth = hwidth/72.0;
	    if (hwidth < 0.) hwidth = 0.;
	    if (hwidth  > 5.) hwidth = 5.;
            continue;
        }

        if (FIELD("size"))
        {
	    if (scan_resolution(value, &dtmp)) size = dtmp;
            continue;
        }

        if (FIELD("north"))
        {
	    if (scan_northing(value, &dtmp)) north = dtmp;
            continue;
        }

        if (FIELD("east"))
        {
	    if (scan_easting(value, &dtmp)) east = dtmp;
            continue;
        }

	if (FIELD("font"))
	{
	    G_strip(value);
	    set_font_name(value);
	    continue;
	}

        if (FIELD("rotate"))
        {
	    if (scan_northing(value, &dtmp)) rotate = dtmp;
            continue;
        }

    }
    /*fclose (fd);*/ /* commented because fd is closed after 
			do_label call; RB March 2000 */
    return 0;
}
