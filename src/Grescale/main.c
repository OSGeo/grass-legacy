/* %W%  %G% */
#include "gis.h"
main (argc, argv) char *argv[];
{
    char buf[512];
    FILE *fd, *popen();
    long old_min, old_max;
    long new_min, new_max;
    long new_value, old_value;
    long new_delta, old_delta;
    int i;
    long value, first, prev;
    long cat;
    char *old_name;
    char *new_name;

    if (argc < 7)
	usage(argv[0]);

    G_gisinit (argv[0]);

    old_name = argv[1];
    new_name = argv[2];

    if (sscanf (argv[3], "%ld", &old_min) != 1)
	usage (argv[0]);
    if (sscanf (argv[4], "%ld", &old_max) != 1)
	usage (argv[0]);
    if (sscanf (argv[5], "%ld", &new_min) != 1)
	usage (argv[0]);
    if (sscanf (argv[6], "%ld", &new_max) != 1)
	usage (argv[0]);
    if (old_min > old_max || new_min > new_max)
	usage (argv[0]);
    if (G_find_cell (old_name,"") == NULL)
    {
	sprintf (buf, "%s - not found\n", old_name);
	G_fatal_error (buf);
	exit(1);
    }
    sprintf (buf, "Greclass '%s' '%s'", old_name, new_name);
    if (argc > 7)
    {
	strcat (buf, " '");
	for (i = 7; i < argc; i++)
	{
	    if (i != 7) strcat (buf, " ");
	    strcat (buf, argv[i]);
	}
	strcat (buf, "'");
    }

    fd = popen (buf, "w");
    old_delta = old_max - old_min + 1;
    new_delta = new_max - new_min + 1;

    prev = new_min;
    first = old_min;
    for (cat = old_min; cat <= old_max; cat++)
    {
	value = (cat - old_min) * new_delta / old_delta + new_min ;
	if (value != prev)
	{
	    fprintf (fd, "%ld thru %ld = %ld %ld", first, cat-1, prev, first);
	    if (cat-1 != first)
		fprintf (fd, " thru %ld", cat-1);
	    fprintf (fd, "\n");
	    prev = value;
	    first = cat;
	}
    }
    fprintf (fd, "%ld thru %ld = %ld %ld", first, cat-1, prev, first);
    if (cat-1 != first)
	fprintf (fd, " thru %ld", cat-1);
    fprintf (fd, "\n");

    pclose (fd);
    exit(0);
}

usage (me) char *me;
{
    fprintf (stderr, "\7usage: %s old_layer new_layer old_min old_max new_min new_max [title]\n", me);
    sleep (3);
    exit(1);
}
