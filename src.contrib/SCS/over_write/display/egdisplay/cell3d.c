#include "windows.h"

cell3d()
{
	R_open_driver();
	Dchoose(MAP.name) ;
	R_close_driver();

	gorun("d.3d", "") ;
}
