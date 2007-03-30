
#include <stdio.h>
#include "psdriver.h"

int main(int argc, char **argv)
{
	static struct driver drv;

	drv.Box_abs		= PS_Box_abs;
	drv.Box_rel		= NULL;
	drv.Client_Open		= NULL;
	drv.Client_Close	= PS_Client_Close;
	drv.Erase		= PS_Erase;
	drv.Get_with_box	= NULL;
	drv.Get_with_line	= NULL;
	drv.Get_with_pointer	= NULL;
	drv.Graph_set		= PS_Graph_set;
	drv.Graph_close		= PS_Graph_close;
	drv.Line_width		= PS_Line_width;
	drv.Panel_save		= NULL;
	drv.Panel_restore	= NULL;
	drv.Panel_delete	= NULL;
	drv.Polydots_abs	= NULL;
	drv.Polydots_rel	= NULL;
	drv.Polyline_abs	= NULL;
	drv.Polyline_rel	= NULL;
	drv.Polygon_abs		= NULL;
	drv.Polygon_rel		= NULL;
	drv.RGB_raster		= PS_RGB_raster;
	drv.Begin_scaled_raster	= PS_begin_scaled_raster;
	drv.Scaled_raster	= PS_scaled_raster;
	drv.Respond		= PS_Respond;
	drv.Work_stream		= NULL;
	drv.Do_work		= NULL;
	drv.lookup_color	= PS_lookup_color;
	drv.color		= PS_color;
	drv.draw_line		= PS_draw_line;
	drv.draw_point		= PS_draw_point;
	drv.draw_bitmap		= PS_draw_bitmap;
	drv.draw_text		= NULL;

	LIB_init(&drv, argc, argv);

	return LIB_main(argc, argv);
}

