#include "D.h"
#include "lproto.h"
#include "windows.h"
#include "raster.h"

int cell3d()
{
	if (R_open_driver() != 0)
	    G_fatal_error ("No graphics device selected");
	Dchoose(MAP.name) ;
	R_close_driver();

	gorun("d.3d", "") ;

	return 0;
}
