#include "gis.h"

ask_background (fd)
    FILE *fd;
{
    char buf[1024];
    char file[300];
    FILE *in;

    printf ("\n");
    if (yes("would you like to run in background"))
    {
	fprintf (fd,"verbose 0");
	return 1;
    }
    return 0;
}
