#include "windows.h"

label()
{
	R_open_driver();
	Dchoose(MAP.name) ;
	R_close_driver();

	gorun("d.label", "") ;
}
