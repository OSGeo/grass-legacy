/*
 * Start up graphics processing.  Anything that needs to be assigned, set up,
 * started-up, or otherwise initialized happens here.  This is called only at
 * the startup of the graphics driver.
 *
 * The external variables define the pixle limits of the graphics surface.  The
 * coordinate system used by the applications programs has the (0,0) origin
 * in the upper left-hand corner.  Hence,
 *    screen_left < screen_right
 *    screen_top  < screen_bottom 
 *
 * NCOLORS is set to the total number of colors available on the device.  This
 * most certainly needs to be more than 100 (or so).  If you are writing a
 * driver with fewer colors you probably need to provide your own Color(),
 * Color_table_float(), Color_table_fixed(), and
 * Reset_color() routines (see ../lib/{Color.c,Reset_clr.c,Clr_table.c}).
 */

#include <string.h>
#include <stdlib.h>
#include <gd.h>

#include "gis.h"
#include "driverlib.h"
#include "png.h"

char *file_name;
FILE *output;
gdImagePtr im;
int currentColor;
unsigned long *xpixels;
int true_color;

int Graph_Set (int argc, char **argv) 
{
    unsigned int bgcol;
    char *p;

    G_gisinit("PNG driver") ;

    /*
     * open the output file
     */

    if (NULL != (p = getenv ("GRASS_PNGFILE"))) {
        if (strlen(p) == 0) {
            p = FILE_NAME;
        }
    } else {
        p = FILE_NAME;
    }
    file_name = p;

    output = fopen(file_name, "w");
    if (output == NULL) {
      fprintf(stderr,"PNG: couldn't open output file %s\n",file_name);
      exit(1);
    }

    /*
     * Creating the image
     */

#ifdef HAVE_GDIMAGECREATETRUECOLOR
    p = getenv("GRASS_TRUECOLOR");
    if (p && strcmp(p, "TRUE") == 0) {
	true_color = 1;
	fprintf(stderr,"PNG: GRASS_TRUECOLOR status: TRUE\n");
	im = gdImageCreateTrueColor(screen_right - screen_left, screen_bottom - screen_top);
    }
    else
#endif
    im = gdImageCreate(screen_right - screen_left, screen_bottom - screen_top);

    NCOLORS = true_color ? (1<<24) : gdMaxColors;

    InitColorTableFixed();

    p = getenv("GRASS_BACKGROUNDCOLOR");
    if (p && *p && sscanf(p, "%x", &bgcol) == 1) {
	int r = (bgcol >> 16) & 0xff;
	int g = (bgcol >>  8) & 0xff;
	int b = (bgcol >>  0) & 0xff;
	int color = _get_lookup_for_color(r, g, b);

	gdImageFilledRectangle(im, screen_left, screen_top,
			       screen_right - screen_left,
			       screen_bottom - screen_top,
			       color);
    }

    p = getenv("GRASS_TRANSPARENT");
    if (p && strcmp(p, "TRUE") == 0) {
	int color;
#ifdef HAVE_GDIMAGECREATETRUECOLOR
	if (true_color)
	    color = gdTrueColorAlpha(0, 0, 0, gdAlphaTransparent);
	else 
#endif
	{
	    color = 216; /* first unused colour */
	    gdImageColorTransparent(im, color);
	}

	gdImageFilledRectangle(im, screen_left, screen_top,
			       screen_right - screen_left,
			       screen_bottom - screen_top,
			       color);
    }

    /*
     * Init finished
     */
    
    fprintf(stdout, "PNG: collecting to file: %s,\n     GRASS_WIDTH=%d, GRASS_HEIGHT=%d\n",
	   file_name, screen_right - screen_left, screen_bottom - screen_top);

    fflush(stdout);
    return 0;
}

