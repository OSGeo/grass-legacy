#include "windows.h"
#include "lproto.h"
#include "raster.h"
#include "D.h"

int scale()
{
	if (R_open_driver() != 0)
	    G_fatal_error ("No graphics device selected");
	Dchoose(MAP.name) ;
	R_close_driver();

	gorun("d.scale", "") ;

	return 0;
}
