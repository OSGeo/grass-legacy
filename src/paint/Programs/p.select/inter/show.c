#include "gis.h"
#include "proto.h"
#include <unistd.h>

int show_current_painter()
{
    char *PAINTER;
    char *get_current_painter();

    PAINTER = get_current_painter();
    if (!PAINTER)
    {
	if (isatty(1))
	    fprintf (stdout,"No PAINTER currently selected\n");
    }
    else
    {
	if (isatty(1))
	    fprintf (stdout,"Currently selected PAINTER: ");
	fprintf (stdout,"%s\n", PAINTER);
    }
    return PAINTER?1:0;
}
