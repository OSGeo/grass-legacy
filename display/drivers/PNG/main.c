
#include <stdio.h>
#include "pngdriver.h"

int main(int argc, char **argv)
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
	drv.RGB_set_colors	= NULL;
	drv.RGB_raster		= NULL;
	drv.Raster_int		= PNG_Raster_int;
	drv.Respond		= PNG_Respond;
	drv.Work_stream		= NULL;
	drv.Do_work		= NULL;
	drv.reset_color		= PNG_reset_color;
	drv.lookup_color	= PNG_lookup_color;
	drv.get_table_type	= PNG_get_table_type;
	drv.color		= PNG_color;
	drv.get_color		= PNG_get_color;
	drv.draw_line		= PNG_draw_line;
	drv.draw_point		= PNG_draw_point;

	return LIB_main(&drv, argc, argv);
}

