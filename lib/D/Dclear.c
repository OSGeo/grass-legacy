#include "gis.h"
#include "raster.h"
#include "display.h"


int Dclearscreen()
{
	char **pads;
	int npads;
	int p;

	R_pad_list (&pads, &npads);
	for (p = -1; p < npads; p++)
	{
		if (p < 0)
		{
			R_pad_select ("");
			R_pad_delete_item("time") ;
			R_pad_delete_item("cur_w") ;
		}
		else
		{
			R_pad_select (pads[p]);
			R_pad_delete ();
		}
	}

	R_standard_color(D_translate_color(DEFAULT_BG_COLOR)) ;
	R_erase() ;

	return 0;
}
