#include "windows.h"
#include "lproto.h"
#include "D.h"
#include "raster.h"

int label()
{
	R_open_driver();
	Dchoose(MAP.name) ;
	R_close_driver();

	gorun("d.label", "") ;

	return 0;
}
