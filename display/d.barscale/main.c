
/****************************************************************************
 *
 * MODULE:       d.barscale
 * AUTHOR(S):    unknown but from CERL code (original contributor)
 *               Markus Neteler <neteler itc.it>, 
 *               Bernhard Reiter <bernhard intevation.de>, 
 *               Cedric Shock <cedricgrass shockfamily.net>, 
 *               Huidae Cho <grass4u gmail.com>, 
 *               Eric G. Miller <egm2 jps.net>, 
 *               Glynn Clements <glynn gclements.plus.com>, 
 *               Hamish Bowman <hamish_b yahoo.com>, 
 *               Jan-Oliver Wagner <jan intevation.de>
 * PURPOSE:      displays a barscale on graphics monitor
 * COPYRIGHT:    (C) 1999-2011 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <grass/gis.h>
#include <grass/display.h>
#include <grass/raster.h>
#include "options.h"
#include <grass/glocale.h>

int color1;
int color2;
double east;
double north;
int use_feet;
int do_background = 1;
int do_bar = 1;
int draw = 0;

int main(int argc, char **argv)
{
    char window_name[64];
    struct Cell_head window;
    int t, b, l, r, fontsize;
    struct GModule *module;
    struct Option *opt1, *opt2, *opt3, *fsize;
    struct Flag *mouse, *feet, *top, *linescale, *northarrow, *scalebar;
    struct Cell_head W;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("display, cartography");
    module->description = _("Displays a barscale on the graphics monitor.");

    mouse = G_define_flag();
    mouse->key = 'm';
    mouse->description = _("Use mouse to interactively place scale");

    feet = G_define_flag();
    feet->key = 'f';
    feet->description = _("Use feet/miles instead of meters");

    linescale = G_define_flag();
    linescale->key = 'l';
    linescale->description = _("Draw a line scale instead of a bar scale");

    top = G_define_flag();
    top->key = 't';
    top->description = _("Write text on top of the scale, not to the right");

    northarrow = G_define_flag();
    northarrow->key = 'n';
    northarrow->description = _("Draw a north arrow only");

    scalebar = G_define_flag();
    scalebar->key = 's';
    scalebar->description = _("Draw a scale bar only");

    opt1 = G_define_standard_option(G_OPT_C_BG);
    opt1->key = "bcolor";

    opt2 = G_define_standard_option(G_OPT_C_FG);
    opt2->key = "tcolor";
    opt2->label = _("Text color");
    
    opt3 = G_define_option();
    opt3->key = "at";
    opt3->key_desc = "x,y";
    opt3->type = TYPE_DOUBLE;
    opt3->answer = "0.0,0.0";
    opt3->options = "0-100";
    opt3->required = NO;
    opt3->description =
	_("The screen coordinates for top-left corner of label ([0,0] is top-left of frame)");

    fsize = G_define_option();
    fsize->key = "fontsize";
    fsize->type = TYPE_INTEGER;
    fsize->required = NO;
    fsize->answer = "14";
    fsize->options = "1-72";
    fsize->description = _("Font size");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);


    G_get_window(&W);
    if (W.proj == PROJECTION_LL && !northarrow->answer)
	G_fatal_error(_("%s does not work with a latitude-longitude location"),
		      argv[0]);

    if (linescale->answer)
	do_bar = 0;

    use_feet = feet->answer ? 1 : 0;
    if (northarrow->answer && scalebar->answer)
	G_fatal_error(_("Choose either -n or -s flag"));

    if (northarrow->answer)
	draw = 1;
    else if (scalebar->answer)
	draw = 2;

    sscanf(opt3->answers[0], "%lf", &east);
    sscanf(opt3->answers[1], "%lf", &north);
    fontsize = atoi(fsize->answer);

    if (R_open_driver() != 0)
	G_fatal_error(_("No graphics device selected"));

    if (D_get_cur_wind(window_name))
	G_fatal_error(_("No current window"));

    if (D_set_cur_wind(window_name))
	G_fatal_error(_("Current window not available"));


    /* Parse and select background color */
    color1 = D_parse_color(opt1->answer, 1);
    if (color1 == 0)
	do_background = 0;

    /* Parse and select foreground color */
    color2 = D_parse_color(opt2->answer, 0);


    /* Read in the map window associated with window */
    G_get_window(&window);

    if (D_check_map_window(&window))
	G_fatal_error(_("Setting map window"));

    if (G_set_window(&window) == -1)
	G_fatal_error(_("Current window not settable"));

    /* Determine conversion factors */
    if (D_get_screen_window(&t, &b, &l, &r))
	G_fatal_error(_("Getting screen window"));
    if (D_do_conversions(&window, t, b, l, r))
	G_fatal_error(_("Error in calculating conversions"));

    if (!mouse->answer) {
	/* Draw the scale */
	draw_scale(NULL, top->answer, fontsize);

	/* Add this command to list */
	D_add_to_list(G_recreate_command());
    }
    else if (mouse_query(top->answer, fontsize)) {
	char cmdbuf[512];

	sprintf(cmdbuf, "%s at=%.2f,%.2f", argv[0], east, north);
	sprintf(cmdbuf, "%s bcolor=%s", cmdbuf, opt1->answer);
	sprintf(cmdbuf, "%s tcolor=%s", cmdbuf, opt2->answer);
	if (top->answer)
	    strcat(cmdbuf, " -t");
	if (feet->answer)
	    strcat(cmdbuf, " -f");
	if (linescale->answer)
	    strcat(cmdbuf, " -l");
	if (northarrow->answer)
	    strcat(cmdbuf, " -n");
	if (scalebar->answer)
	    strcat(cmdbuf, " -s");

	/* Add this command to list */
	D_add_to_list(cmdbuf);
    }

    R_close_driver();

    exit(EXIT_SUCCESS);
}
