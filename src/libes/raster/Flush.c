#include "raster.h"

int R_flush()
{
	R_stabilize() ;
	return 0;
}
