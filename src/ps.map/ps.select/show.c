#include "gis.h"

show_current_painter()
{
    char *PAINTER;
    char *get_current_painter();

    PAINTER = get_current_painter();
    if (PAINTER) printf ("%s\n", PAINTER);
    return PAINTER ? 1 : 0;
}
