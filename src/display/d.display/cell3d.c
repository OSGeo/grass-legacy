#include "D.h"
#include "lproto.h"
#include "windows.h"
#include "raster.h"

int cell3d()
{
	R_open_driver();
	Dchoose(MAP.name) ;
	R_close_driver();

	gorun("d.3d", "") ;

	return 0;
}
