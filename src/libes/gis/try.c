#include "gis.h"
main(argc, argv) char *argv[];
{
int i;
	for (i = 1; i < argc; i++)
	{
	    fprintf (stderr, "%s=>", argv[i]);
	    G_tolcase(argv[i]);
	    fprintf (stderr, "%s\n", argv[i]);
	    }}
