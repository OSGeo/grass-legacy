
#include <grass/config.h>

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <grass/gis.h>
#include <grass/glocale.h>
#include <grass/raster.h>
#include <grass/graphics.h>

#include "driverlib.h"
#include "transport.h"
#include "pngdriver.h"
#include "open.h"
#include "pad.h"

static void LOC_init(void)
{
	const char *name = "full_screen";
	const char *font = getenv("GRASS_FONT");
	int t = R_screen_top();
	int b = R_screen_bot();
	int l = R_screen_left();
	int r = R_screen_rite();
	char buff[256];

	R_font(font ? font : "romans");

	R_pad_select("");
	R_pad_set_item("time", "1");
	R_pad_set_item("cur_w", name);

	R_pad_create(name);
	R_pad_select(name);
	R_pad_set_item("time", "1");

	sprintf(buff, "%d %d %d %d", t, b, l, r);
	R_pad_set_item("d_win", buff);

	R_set_window(t, b, l, r);
}

int LOC_open_driver(void)
{
	static struct driver drv;

	drv.Box_abs		= PNG_Box_abs;
	drv.Box_rel		= NULL;
	drv.Can_do_float	= PNG_Can_do_float;
	drv.Client_Open		= NULL;
	drv.Client_Close	= PNG_Client_Close;
	drv.Color_table_float	= PNG_Color_table_float;
	drv.Color_table_fixed	= PNG_Color_table_fixed;
	drv.Erase		= PNG_Erase;
	drv.Get_with_box	= NULL;
	drv.Get_with_line	= NULL;
	drv.Get_with_pointer	= NULL;
	drv.Graph_set		= PNG_Graph_set;
	drv.Graph_close		= PNG_Graph_close;
	drv.Line_width		= PNG_Line_width;
	drv.Panel_save		= NULL;
	drv.Panel_restore	= NULL;
	drv.Panel_delete	= NULL;
	drv.Polydots_abs	= NULL;
	drv.Polydots_rel	= NULL;
	drv.Polyline_abs	= NULL;
	drv.Polyline_rel	= NULL;
	drv.Polygon_abs		= NULL;
	drv.Polygon_rel		= NULL;
	drv.RGB_set_colors	= PNG_RGB_set_colors;
	drv.RGB_raster		= PNG_RGB_raster;
	drv.Raster_int		= PNG_Raster_int;
	drv.Respond		= PNG_Respond;
	drv.Work_stream		= NULL;
	drv.Do_work		= NULL;
	drv.reset_color		= PNG_reset_color;
	drv.lookup_color	= PNG_lookup_color;
	drv.get_table_type	= PNG_get_table_type;
	drv.color		= PNG_color;
	drv.draw_line		= PNG_draw_line;
	drv.draw_point		= PNG_draw_point;
	drv.draw_bitmap		= NULL;

	LIB_init(&drv, 0, NULL);

	LOC_init();

	COM_Client_Open();

	return OK;
}

int LOC__open_quiet(void)
{
	return 0;
}

int LOC_stabilize(void)
{
	COM_Respond();

	return 0;
}

int LOC_kill_driver(void)
{
	COM_Graph_close();

	return 0;
}

int LOC_close_driver(void)
{
	LOC_stabilize();
	COM_Client_Close();
	LOC_kill_driver();

	return 0;
}

int LOC_release_driver(void)
{
	LOC_stabilize();
	COM_Client_Close();

	return 0;
}

