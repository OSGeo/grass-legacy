#include "gis.h"
int 
show_current_painter (void)
{
    char *PAINTER;
    char *get_current_painter();

    PAINTER = get_current_painter();
    if (PAINTER)
	fprintf (stdout,"%s\n", PAINTER);
    return PAINTER?1:0;
}
