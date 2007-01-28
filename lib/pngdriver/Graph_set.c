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

#include <grass/gis.h>
#include "pngdriver.h"

char *file_name;
int currentColor;
unsigned int *xpixels;
int true_color;
int auto_write;
int has_alpha;

int width, height;
unsigned int *grid;
unsigned char palette[256][4];
unsigned int transparent;
unsigned int background;
int modified;

int PNG_Graph_set(int argc, char **argv)
{
	char *p;

	G_gisinit("PNG driver") ;

	p = getenv("GRASS_PNGFILE");
	if (!p || strlen(p) == 0)
		p = FILE_NAME;

	file_name = p;

	p = getenv("GRASS_TRUECOLOR");
	true_color = p && strcmp(p, "TRUE") == 0;

	G_message("PNG: GRASS_TRUECOLOR status: %s",
		true_color ? "TRUE" : "FALSE");

	p = getenv("GRASS_PNG_AUTO_WRITE");
	auto_write = p && strcmp(p, "TRUE") == 0;

	width = screen_right - screen_left;
	height = screen_bottom - screen_top;

	grid = G_malloc(width * height * sizeof(unsigned int));

	NCOLORS = true_color ? (1<<24) : 256;

	p = getenv("GRASS_BACKGROUNDCOLOR");
	if (!p || !*p || sscanf(p, "%x", &background) != 1)
	{
		/* 0xffffff = white, 0x000000 = black */
		if(strcmp(DEFAULT_FG_COLOR, "white") == 0)
			/* foreground: white, background: black */
			background = 0;
		else
			/* foreground: black, background: white */
			background = 0xffffff;
	}

	p = getenv("GRASS_TRANSPARENT");
	if (p && strcmp(p, "TRUE") == 0)
	{
		has_alpha = 1;
		transparent = background;
	}

	init_color_table();

	COM_Erase();

	G_message("PNG: collecting to file: %s,\n     GRASS_WIDTH=%d, GRASS_HEIGHT=%d",
		file_name, width, height);

	modified = 1;

	p = getenv("GRASS_PNG_READ");
	if (p && strcmp(p, "TRUE") == 0)
		read_image();

	return 0;
}

