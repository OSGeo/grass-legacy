
#include "pngdriver.h"

const struct driver *PNG_Driver(void)
{
	static struct driver drv;
	static int initialized;

	if (initialized)
		return &drv;

	drv.Box_abs		= PNG_Box_abs;
	drv.Box_rel		= NULL;
	drv.Client_Open		= NULL;
	drv.Client_Close	= PNG_Client_Close;
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
	drv.Begin_scaled_raster	= PNG_begin_scaled_raster;
	drv.Scaled_raster	= PNG_scaled_raster;
	drv.Respond		= PNG_Respond;
	drv.Work_stream		= NULL;
	drv.Do_work		= NULL;
	drv.lookup_color	= PNG_lookup_color;
	drv.color		= PNG_color;
	drv.draw_line		= PNG_draw_line;
	drv.draw_point		= PNG_draw_point;
	drv.draw_bitmap		= PNG_draw_bitmap;
	drv.draw_text		= NULL;

	return &drv;
}

