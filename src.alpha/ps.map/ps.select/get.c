#include "gis.h"

char *get_current_painter()
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
