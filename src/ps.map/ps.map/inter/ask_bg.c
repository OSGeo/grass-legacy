#include "gis.h"
#include "ps_map.h"

int ask_background (FILE *fd)
{
    fprintf (stdout,"\n");
    if (yes("would you like to run in background"))
    {
	fprintf (fd,"verbose 0");
	return 1;
    }
    return 0;
}
