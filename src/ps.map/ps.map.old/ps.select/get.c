#include "gis.h"
#include <unistd.h>

char *
get_current_painter (void)
{
    char *PAINTER;
    char *ps_devices();

    PAINTER = G__getenv("PSPAINTER");
    if (PAINTER && access(ps_devices(PAINTER), 0) != 0)
    {
	G_unsetenv("PSPAINTER");
	PAINTER = NULL;
    }
    return PAINTER;
}
