#include "gis.h"
#include <unistd.h>
char *
get_current_painter (void)
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
