/* %W% %G% */

#include "gis.h"

#define INPUT argv[1]
#define OUTPUT argv[2]
#define TITLE 3

main (argc, argv) char *argv[];
{
    struct Colors colr;
    struct Range range;
    char *mapset;
    int in_fd, out_fd;
    char title[100];
    char name[100];
    char *me;
    int i;
    static int verbose = 1;


    me = argv[0];
    if (argc > 1 && strcmp (argv[1], "-v") == 0)
    {
	verbose = 0;
	argc--;
	argv++;
    }
    if (argc < 3)
    {
	fprintf (stderr, "usage: %s [-v] input-layer output-layer [title]\n", argv[0]);
	exit(1);
    }

    *title = 0;
    for (i = TITLE; i < argc; i++)
    {
	if (*title) strcat (title, " ");
	strcat (title, argv[i]);
    }

    G_gisinit (me);

    strcpy (name, INPUT);
    mapset = G_find_cell2 (name,"");
    if (!mapset)
    {
	char err[100];
	sprintf (err, "%s: [%s] not found", G_program_name(), INPUT);
	G_fatal_error (err);
	exit(1);
    }
    if (G_legal_filename (OUTPUT) < 0)
    {
	char err[100];
	sprintf (err, "%s: [%s] illegal name", G_program_name(), OUTPUT);
	G_fatal_error (err);
	exit(1);
    }

    in_fd = G_open_cell_old (name, mapset);
    if (in_fd < 0)
	exit(1);

    out_fd = G_open_cell_new (OUTPUT);
    if (out_fd < 0)
	exit(1);

    clump (in_fd, out_fd, verbose);

    if (verbose)
	fprintf (stderr, "CREATING SUPPORT FILES ... "); fflush (stderr);
    G_close_cell (in_fd);
    G_close_cell (out_fd);

    if (*title == 0)
	sprintf (title, "clump of %s in %s", name, mapset);
    G_put_cell_title (OUTPUT, title);
    G_read_range (OUTPUT, G_mapset(), &range);
    G_make_random_colors (&colr, range.pmin, range.pmax);
    G_write_colors (OUTPUT, G_mapset(), &colr);
    if (verbose)
	fprintf (stderr, "\n");
    printf ("%d clumps\n", range.pmax);
    exit(0);
}
