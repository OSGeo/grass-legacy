#include "windows.h"
#include "lproto.h"
#include "raster.h"
#include "D.h"

int scale()
{
	R_open_driver();
	Dchoose(MAP.name) ;
	R_close_driver();

	gorun("d.scale", "") ;

	return 0;
}
