#include "gis.h"

getsize (argc, argv, msg, n)
    char **argv;
    char *msg;
    int *n;
{
    int size;
    char buf[100];
    int i;

    if (argc)
    {
	*n = 1;
	if (sscanf (argv[0], "%d", &size) == 1 && size > 0)
	    return(size);
    }
    *n = argc;
    if (!isatty(0)) return 0;

    while(1)
    {
	printf ("enter a size for %s: ", msg);
	if (!G_gets(buf)) continue;
	G_strip (buf);
	if (*buf == 0) return 0;
	if (sscanf (buf, "%d", &size) == 1 && size > 0)
	    return(size);
    }
}
