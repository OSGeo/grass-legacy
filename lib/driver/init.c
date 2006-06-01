#include <grass/config.h>

#include <stdio.h>
#include <stdlib.h>

#include <grass/gis.h>
#include "driverlib.h"
#include "driver.h"
#include "pad.h"

const struct driver *driver;

int NCOLORS;

int screen_left;
int screen_right;
int screen_bottom;
int screen_top;

int cur_x;
int cur_y;

double text_size_x;
double text_size_y;
double text_rotation;

int LIB_init(const struct driver *drv, int argc, char **argv)
{
	const char *p;

	driver = drv;

	/* initialize graphics */

	p = getenv("GRASS_WIDTH");
	screen_left = 0;
	screen_right = (p && atoi(p)) ? atoi(p) : DEF_WIDTH;

	p = getenv("GRASS_HEIGHT");
	screen_top = 0;
	screen_bottom = (p && atoi(p)) ? atoi(p) : DEF_HEIGHT;

	if (COM_Graph_set(argc, argv) < 0)
		exit(1);

	/* Initialize color stuff */
	COM_Color_table_fixed();

	/* initialize the pads */
	create_pad("");    /* scratch pad */

	return 0;
}

