#include "gis.h"
char *
get_current_painter()
{
    char *PAINTER;
    char *drivers();

    PAINTER = G__getenv ("PAINTER");
    if (PAINTER && access(drivers(PAINTER),0) != 0)
    {
	G_unsetenv ("PAINTER");
	PAINTER = NULL;
    }
    return PAINTER;
}
