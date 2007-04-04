
#include <stdio.h>
#include "driver.h"
#include "htmlmap.h"

int main(int argc, char **argv)
{
	static struct driver drv;

	drv.Box_abs		= NULL;
	drv.Box_rel		= NULL;
	drv.Client_Open		= NULL;
	drv.Client_Close	= NULL;
	drv.Erase		= NULL;
	drv.Get_with_box	= NULL;
	drv.Get_with_line	= NULL;
	drv.Get_with_pointer	= NULL;
	drv.Graph_set		= HTML_Graph_set;
	drv.Graph_close		= HTML_Graph_close;
	drv.Line_width		= NULL;
	drv.Panel_save		= NULL;
	drv.Panel_restore	= NULL;
	drv.Panel_delete	= NULL;
	drv.Polydots_abs	= NULL;
	drv.Polydots_rel	= NULL;
	drv.Polyline_abs	= NULL;
	drv.Polyline_rel	= NULL;
	drv.Polygon_abs		= HTML_Polygon_abs;
	drv.Polygon_rel		= NULL;
	drv.Begin_scaled_raster	= NULL;
	drv.Scaled_raster	= NULL;
	drv.Respond		= NULL;
	drv.Work_stream		= NULL;
	drv.Do_work		= NULL;
	drv.lookup_color	= NULL;
	drv.color		= NULL;
	drv.draw_line		= NULL;
	drv.draw_point		= NULL;
	drv.draw_bitmap		= NULL;
	drv.draw_text		= HTML_Text;

	LIB_init(&drv, argc, argv);

	return LIB_main(argc, argv);
}

