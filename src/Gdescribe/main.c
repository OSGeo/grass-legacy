#include "gis.h"
main(argc,argv) char *argv[];
{
    char *name;
    char *mapset;
    char msg[100];
    int i;
    int verbose;
    int compact;
    int range;
    int windowed;

    G_gisinit (argv[0]);

    name = NULL;
    verbose = 1;
    compact = 1;
    range = 0;
    windowed = 0;

    for (i = 1; i < argc; i++)
	if (argv[i][0] == '-')
	{
	    char *o;
	    o = &argv[i][1];
	    if (*o == 0) o--;

	    while (*o)
		switch (*o++)
		{
		case '1': compact = 0; break;
		case 'r': range = 1; break;
		case 'v': verbose = 0; break;
		case 'w': windowed = 1; break;
		default: usage(argv[0]);
		}
	}
	else if (name == NULL)
	    name = argv[i];
	else
	    usage(argv[0]);

    if (name == NULL)
	usage (argv[0]);

    if (mapset = G_find_cell2 (name, ""))
    {
	describe (name, mapset, compact, verbose, range, windowed);
	exit(0);
    }
    sprintf (msg,"%s: [%s] not found", G_program_name(), name);
    G_fatal_error (msg);
    exit(1);
}
usage (me) char *me;
{
    fprintf (stderr,"usage: %s [-1rvw] layer\n", me);
    exit(1);
}
