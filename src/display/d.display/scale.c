#include "windows.h"
scale()
{
	R_open_driver();
	Dchoose(MAP.name) ;
	R_close_driver();

	gorun("d.scale", "") ;
}
