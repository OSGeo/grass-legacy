/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       d.info
 * AUTHOR(S):    Glynn Clements
 * PURPOSE:      Display information about the active display monitor
 * COPYRIGHT:    (C) 2004 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#include <stdio.h>
#include "gis.h"
#include "raster.h"

int main(int argc,char *argv[])
{
	struct GModule *module;
	struct Flag *rflag, *dflag, *cflag;
	int l, r, t, b;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description = 
		"Display information about the active display monitor";

	rflag = G_define_flag();
	rflag->key = 'r';
	rflag->description = "Display screen rectangle";

	dflag = G_define_flag();
	dflag->key = 'd';
	dflag->description = "Display screen dimensions";

	cflag = G_define_flag();
	cflag->key = 'c';
	cflag->description = "Display number of colors";

	if (argc > 1 && G_parser(argc, argv))
		return 1;

	if(!rflag->answer && !dflag->answer && !cflag->answer) {
		G_usage();
		return 1;
	}

	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

	if (rflag->answer || dflag->answer)
	{
		l = R_screen_left();
		r = R_screen_rite();
		t = R_screen_top();
		b = R_screen_bot();
	}

	if (rflag->answer)
		printf("rectangle: %d %d %d %d\n", l, r, t, b);

	if (dflag->answer)
		printf("dimensions: %d %d\n", r - l, b - t);

	if (cflag->answer)
	{
		int colors;
		R_get_num_colors(&colors);
		printf("colors: %d\n", colors);
	}

	R_close_driver();

	return 0;
}
