#include "windows.h"

digitize()
{
	R_open_driver();
	Dchoose(MAP.name) ;
	R_close_driver();

	gorun("d.digit", "") ;
}
