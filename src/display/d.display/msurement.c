#include "windows.h"
#include "lproto.h"
#include "raster.h"
#include "D.h"

int measurements()
{
	R_open_driver();
	Dchoose(MAP.name) ;
	R_close_driver();

	gorun("d.measure", "") ;

	return 0;
}
