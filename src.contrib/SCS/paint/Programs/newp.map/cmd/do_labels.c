#include "gis.h"
#include "text.h"
#include "labels.h"
#include "misc.h"
#include "fullwindow.h"

/***********************************************************
 * the labels file has the following records:
 *
 * text:       text to be printed
 * north:      coordinate north for placement of text
 * east:       coordinate east for placement of text
 * color:      color of text (rgb % | printer # | color name)
 * background: color of background
 * border:     color of border
 * opaque:     background is opaque to vectors and grid (yes/no)
 * size:       size of text
 * width:      width of lines used to draw text
 * ref:        reference location for text. 
 *               lower left  | lower right  | lower center
 *               upper left  | upper right  | upper center
 *               center left | center right | center
 * xoffset:    number of pixels offset in horizontal to place text
 * yoffset:    number of pixels offset in vertical to place text
 * hcolor      highlight color
 * hwidth      highlight width
 *
 * the inital values are:
 *   north, east - undefined (off the image)
 *   color         black
 *   hcolor        none
 *   background    white
 *   border        black
 *   opaque        yes
 *   ref           center
 *   x,y           0
 *
 * as soon as a "text" field is seen it is printed using the current
 * configuration. A configuration value is not changed unless explicitly
 * changed in the labels file
 ************************************************************/

#define FIELD(x) strcmp(x,field)==0
BOX	rbox;
float rrot;


do_labels (draw)
{
    FILE *fd;

    int i;

    for (i = 0; i < labels.count; i++)
    {
	fd = G_fopen_old ("paint/labels", labels.name[i], labels.mapset[i]);
	if (fd == NULL)
	{
	    char msg[100];
	    sprintf (msg, "can't open label file %s in %s",
		labels.name[i], labels.mapset[i]);
	    G_warning (msg);
	}
	else
	{
	    do_label (fd, draw);
	    fclose (fd);
	}
    }
    if (labels.other)
    {
	fd = fopen (labels.other,"r");
	if (fd == NULL)
	{
	    char msg[100];
	    sprintf (msg, "can't open temp label file %s", labels.other);
	    G_warning (msg);
	}
	else
	{

	    do_label (fd, draw);
	    fclose (fd);
	}
    }
}

do_label (fd,draw)
    FILE *fd;
{
    double atof();
    double Mheight() ;

    double east, north;
    int xoffset, yoffset;
    int xref, yref;
    int background;
    int border;
    int color;
    int hcolor;
    int width;
    int hwidth;
	int rotation;
    int opaque;
    float size;
    int x, y;
    double dtmp;

    char field[1024];
    char value[1024];
    char buf[1024];

    select_standard_font();

/*
 * initialize the text location 
 * set background to white
 * set color to black
 * set size to default
 */
    north = fullwindow.north;
    east = fullwindow.west;
    opaque = 0;
    xoffset = 0;
    yoffset = 0;

    color      = BLACK;
    width      = 1;
    background = WHITE;
    hcolor     = -1;
    hwidth     = 0;
	rotation   = 0;

    border = BLACK;
    size = 0.0;
    xref = CENTER;
    yref = CENTER;
    set_text_rotation (0);

#ifdef DEBUG
 printf ("do_label(draw=%d)\n", draw);
#endif
    while (fgets (buf, sizeof buf, fd))
    {
#ifdef DEBUG
 printf (" %s", buf);
#endif
        *value = 0;
        *field = 0;
        if (sscanf (buf,"%[^:]:%[^\n]", field, value) < 1) continue;

        if (FIELD("text"))
        {
	    G_plot_where_xy(east, north, &x, &y);
            x += xoffset;
            y += yoffset;

            if (size <= 0.0)
		set_reasonable_text_size();
	    else
		set_text_size (size / Mheight(1));
	    set_text_border (border);
            set_text_xref (xref);
            set_text_yref (yref);

            if (draw)
            {
                set_text_background (opaque?background:-1);
                set_text_color (color);
                set_text_width (width);
                set_text_hcolor (hcolor);
                set_text_hwidth (hwidth);
            }
            else
            {
                set_text_background (background);
                set_text_color (-1);
                set_text_hcolor (-1);
            }
#ifdef DEBUG
 printf (" at x=%d y=%d\n", x, y);
#endif



			if (draw) {
			if (rrot> 0) {
				xref  = LEFT;
				yref  = LOWER;
				set_text_xref (xref);
				set_text_yref (yref);

			}

			
			if (draw) {

            draw_text (value, x, y, 0);


		    if (rrot != 0.0)
			set_text_size (size / Mheight(1));
			if (rrot > 90.0 && rrot <=180.0) {
				xref  = RIGHT; 
				yref  = LOWER;
				set_text_xref (xref);
				set_text_yref (yref);
				}
		    if (rrot > 180.0 && rrot <= 270.0) {
				xref  = RIGHT; 
				yref  = UPPER;
				set_text_xref (xref);
				set_text_yref (yref);
				}

		    if (rrot > 270.0 && rrot < 360.0) {
				xref  = LEFT; 
				yref  = UPPER;
				set_text_xref (xref);
				set_text_yref (yref);
				}


			getbox(rrot, border, background);

            draw_text (value, x, y, 1);
			}
			}
            continue;
        }

        if (FIELD("color"))
        {
            color = which_color (value);
            continue;
        }

        if (FIELD("hcolor"))
        {
            hcolor = which_color (value);
            continue;
        }

        if (FIELD("xoffset"))
        {
            xoffset = atoi (value);
            continue;
        }

        if (FIELD("yoffset"))
        {
            yoffset = atoi (value);
            continue;
        }

        if (FIELD("ref"))
        {
	    if (!scan_ref (value, &xref, &yref))
	    {
                yref = CENTER;
                xref = CENTER;
	    }
            continue;
        }

        if (FIELD("background"))
        {
            background = which_color (value);
            continue;
        }

        if (FIELD("border"))
        {
            border = which_color (value);

            continue;
        }

        if (FIELD("opaque"))
        {
	    G_strip (value);
            opaque = (strcmp (value,"no") != 0);
            continue;
        }

        if (FIELD("width"))
        {
            width = atoi (value);
            continue;
        }

        if (FIELD("hwidth"))
        {
            hwidth = atoi (value);
            continue;
        }

        if (FIELD("rotation"))
        {
            rotation = atoi (value);
			set_text_rotation(rotation);
            continue;
			}

        if (FIELD("size"))
        {
	    if (scan_resolution (value, &dtmp))
		size = dtmp;
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

	if (FIELD("font"))
	{
	    G_strip (value);
	    select_font (value);
	    continue;
	}

    }
    fclose (fd);
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

static
double
Mheight(drawflag)
{
    BOX box;

    set_text_border(-1);
    set_text_background(-1);
    set_text_width(1);
    set_text_size(100.0);
    text_bounds ("M",0,0,&box, drawflag);

    return (fullwindow.ns_res * (box.bottom-box.top+1) / 100.0) ;
}
