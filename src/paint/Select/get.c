/* %W% %G% */
#include "gis.h"
char *
get_current_driver()
{
    char *PAINTER;
    PAINTER = G__getenv ("PAINTER");
    if (PAINTER && access(drivers(PAINTER),0) != 0)
    {
	G_setenv ("PAINTER","");
	PAINTER = NULL;
    }
    return PAINTER;
}
