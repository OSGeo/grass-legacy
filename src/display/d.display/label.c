#include "windows.h"
#include "lproto.h"
#include "D.h"
#include "raster.h"

int label()
{
	if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");
	Dchoose(MAP.name) ;
	R_close_driver();

	gorun("d.label", "") ;

	return 0;
}
