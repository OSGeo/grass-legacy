#define NFILES 14

#include "gis.h"

main(argc, argv) char *argv[];
{
    char *mapset;
    int i;
    int nfiles;
    int withcats;
    int fd[NFILES];
    struct Categories cats[NFILES];
    struct Cell_head window;
    CELL *cell, value;
    int row, col;
    double drow, dcol;
    int in_window;
    double east, north;
    double east_buf[100], north_buf[100];
    int line;
    char buf[1024];
    char label[1024];
    double G_northing_to_row();
    double G_easting_to_col();

    G_gisinit (argv[0]);

    withcats = 0;
    if (argc > 1 && argv[1][0] == '-')
    {
	if (strcmp (argv[1], "-f") != 0)
	    usage();
	withcats = 1;
	argc--;
	argv++;
    }
    nfiles = 0;
    if (argc <= 1)
	usage();
    
    for (i = 1; i < argc; i++)
    {
	if (nfiles >= NFILES)
	{
	    fprintf (stderr, "%s: can only do up to %d cell files, sorry\n",
		G_program_name(), NFILES);
	    exit(1);
	}
	if(NULL == (mapset = G_find_cell (argv[i], "")))
	    die (argv[i], " - not found");
	if(0 > (fd[nfiles] = G_open_cell_old (argv[i], mapset)))
	    die ("can't open", argv[i]);
	if (withcats && G_read_cats (argv[i], mapset, &cats[nfiles]) < 0)
	    die (argv[i], " - can't read category file");
	nfiles++;
    }
    cell = G_allocate_cell_buf();
    G_get_window (&window);

    line = 0;
    if (isatty(0))
	fprintf (stderr, "enter points, \"end\" to quit\n");
    while (1)
    {
	if (isatty(0))
	    fprintf (stderr, "\neast north [label] >  ");
	if (gets(buf) == NULL)
	    exit(0);
	if (strcmp (buf, "end") == 0)
	    exit(0);
	if (strcmp (buf, "exit") == 0)
	    exit(0);
	line++;

	*label = *east_buf = *north_buf = 0;
	sscanf (buf, "%s %s %[^\n]", east_buf, north_buf, label);
	if (*east_buf == 0)
	    continue;        /* skip blank lines */

	if (*north_buf == 0)
	{
	    oops (line, buf, "two coordinates (east north) required");
	    continue;
	}
	if (!G_scan_northing (north_buf, &north, window.proj)
	||  !G_scan_easting (east_buf, &east, window.proj))
	{
	    oops (line, buf, "invalid coordinate(s)");
	    continue;
	}

/* convert north, east to row and col */
	drow = G_northing_to_row (north, &window);
	dcol = G_easting_to_col (east, &window);

/* a special case.
 *   if north falls at southern edge, or east falls on eastern edge,
 *   the point will appear outside the window.
 *   So, for these edges, bring the point inside the window
 */
	if (drow == window.rows) drow--;
	if (dcol == window.cols) dcol--;

	row = (int) drow;
	col = (int) dcol;

#ifdef DEBUG
fprintf (stderr, "%s, %s at col %d, row %d\n",
	east_buf, north_buf, col, row);
#endif DEBUG

	in_window = 1;
	if (row < 0 || row >= window.rows || col < 0 || col >= window.cols)
	{	
	    in_window = 0;
	    if (isatty(0))
		fprintf (stderr,
		  "** note ** %s %s is outside your current window\n",
			east_buf, north_buf);
	}

	printf ("%s|%s|%s", east_buf, north_buf, label);
	for (i = 0; i < nfiles; i++)
	{
	    if (in_window)
	    {
		if (G_get_map_row (fd[i], cell, row) < 0)
		    die (argv[i+1], " - can't read");
		value = cell[col];
	    }
	    else
		value = 0;

	    printf ("|%ld", (long) value);
	    if (withcats)
		printf ("|%s", G_get_cat (value, &cats[i]));
	}
	printf ("\n");
    }
}

oops (line, buf, msg)
    char *buf, *msg;
{
    static int first = 1;
    if (!isatty(0))
    {
	if (first)
	{
	    fprintf (stderr, "%s: ** input errors **\n",
		G_program_name());
	    first = 0;
	}
	fprintf (stderr, "line %d: %s\n", line, buf);
    }
    fprintf (stderr, "** %s **\n", msg);
}
