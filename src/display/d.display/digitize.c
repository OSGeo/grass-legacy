#include "windows.h"
#include "lproto.h"
#include "D.h"
#include "raster.h"

int digitize()
{
	R_open_driver();
	Dchoose(MAP.name) ;
	R_close_driver();

	gorun("d.digit", "") ;

	return 0;
}
