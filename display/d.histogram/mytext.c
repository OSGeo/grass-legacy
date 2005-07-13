#include "raster.h"

#ifdef R_text
#undef R_text
#endif
int 
mytext (char *s)
{
	R_stabilize();
	R_text(s);
	R_stabilize();
}
