#include "gis.h"

static char tab = '\t';
static struct Categories cats;

main(argc, argv) char *argv[];
{
    char name[128], *mapset;
    char command[512];
    FILE *fd, *popen();
    long x, y;
    int i;

    G_gisinit (argv[0]);
    argc--;
    argv++;
    if (argc < 1)
	usage();

    if (strncmp (argv[0], "-t", 2) == 0)
    {
	tab = argv[0][2];
	if (!tab) tab = '\t';

	argc--;
	argv++;
    }

    if (argc < 1)
	usage();

    strcpy (name, argv[0]);
    argc--;
    argv++;

    if (NULL == (mapset = G_find_cell2 (name,"")))
    {
	fprintf (stderr, "%s: %s - cell file not found\n", 
		G_program_name(), name);
	exit(1);
    }
    if (G_read_cats (name, mapset, &cats) < 0)
    {
	fprintf (stderr, "%s: %s in %s - can't read category file\n", 
		G_program_name(), name, mapset);
	exit(1);
    }

/* if no other arguments, use Gdescribe to get the cats */
    if (argc < 1)
    {
	sprintf (command, "Gdescribe -1 '%s in %s'", name, mapset);
	fd = popen (command, "r");
	while (fscanf (fd, "%ld", &x) == 1)
		print_label (x);
	pclose (fd);
    }
    else
    {
	for (i = 0; i < argc; i++)
	    if (!scan_cats (argv[i], &x, &y))
		usage();
	for (i = 0; i < argc; i++)
	{
	    scan_cats (argv[i], &x, &y);
	    while (x <= y)
		print_label (x++);
	}
    }
    exit(0);
}

print_label (x)
    long x;
{
    char *label, *G_get_cat();

    G_squeeze(label = G_get_cat ((CELL)x, &cats));
    printf ("%ld%c%s\n", x, tab, label);
}
usage()
{
    fprintf (stderr, "Usage: %s [-t'tabchar'] cellfile\n", G_program_name());
    exit(1);
}

scan_cats (s, x, y)
    char *s;
    long *x, *y;
{
    char dummy[2];

    *dummy = 0;
    if (sscanf (s, "%ld-%ld%1s", x, y, dummy) == 2)
	return (*dummy == 0 && *x <= *y);
    *dummy = 0;
    if (sscanf (s, "%ld%1s", x, dummy) == 1 && *dummy == 0)
    {
	*y = *x;
	return 1;
    }
    return 0;
}
