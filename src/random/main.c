/* %W% %G% */
#include "gis.h"
static char *intro[] = {

"This program creates a map layer and/or a sites list file",
"with points (single cells) in random locations. The locations are",
"restricted to non-zero categories in a map layer which you can specify.",
"The number of random cells can be a specific number of cells or a",
"percentage of the number of non-zero cells.",

NULL};

main(argc, argv) char *argv[];
{
    int i;
    CELL *cell;
    int row, col;
    int nrows, ncols;
    int fd;
    int count, percent;
    int cat_zero;
    long total;
    char input[40], cellfile[40], sitefile[40];
    char *mapset, err[80];
    char info[80];

    G_gisinit (argv[0]);

    G_clear_screen();
    for (i = 0; intro[i]; i++)
	printf ("%s\n", intro[i]);
    printf ("\n");

    do
    {
	mapset = G_ask_cell_old("Enter map to locate random points on",input);
	if (mapset == NULL)
	    exit(1);
    }
    while ((fd = G_open_cell_old (input, mapset)) < 0);

    G_set_ask_return_msg ("if you don't want a cell file");
    if (G_ask_cell_new ("", cellfile) == NULL)
	*cellfile = 0;
    if (*cellfile)
	G_set_ask_return_msg ("if you don't want a site list file");
    if (G_ask_sites_new ("", sitefile) == NULL)
	*sitefile = 0;
    if (*sitefile == 0 && *cellfile == 0)
	exit(0);

/* ask about category zero */
    cat_zero = G_yes ("Do you want random sites against category zero as well? ", 0);
/* read input layer and count cells */
    nrows = G_window_rows();
    ncols = G_window_cols();
    if (cat_zero)
	total = nrows * ncols;
    else
    {
	cell = G_allocate_cell_buf();
	total = 0;
	printf ("Reading [%s in %s] ... ", input, mapset);
	for (row = 0; row < nrows; row++)
	{
	    G_percent (row, nrows, 10);
	    if (G_get_map_row (fd, cell, row) < 0)
		exit(1);
	    col = ncols;
	    while (col-- > 0)
		if (*cell++)
		    total++;
	    cell -= ncols;
	}
	G_percent (row, nrows, 10);
	free (cell);
    }
    G_close_cell (fd);

    sprintf (info, "There are %ld %scells in the current window",
	total, cat_zero?"":"non-zero ");
    *err = 0;
    count = percent = 0;

    V_clear();
    V_line (0, info);
    V_line (2, "Exact number of random locations:");
    V_line (3, "Percentage of the total number of cells:");
    V_line (6, "(only set one of the above)");
    V_line (7, err);
    V_ques (&count, 'i', 2, 51, 6);
    V_ques (&percent, 'i', 3, 51, 6);

    while (1)
    {
	V_intrpt_ok();
	if (!V_call())
	    exit(1);
	*err = 0;

	if (count == 0 && percent == 0)
		continue;
	if (count != 0 && percent != 0)
		continue;

	if (count < 0)
	{
	    strcpy(err, "** number of locations must be positive **");
	    continue;
	}

	if (count > total)
	{
	    strcpy (err, "** number of locations must not exceed the total number of cells **");
	    continue;
	}

	if (percent < 0)
	{
	    strcpy(err, "** percentage must be positive **");
	    continue;
	}

	if (percent > 100)
	{
	    strcpy(err, "** percentage must not exceed 100 **");
	    continue;
	}

	break;
    }


    random (input, cellfile, sitefile, count, percent, total, cat_zero);
}

random (input, cellfile, sitefile, count, percent, total, cat_zero)
    char *input, *cellfile, *sitefile;
    long total;
{
    char pgm[20];
    char num[20];
    char command[300];

    sprintf (command, "%s/bin/Grandom", G_gisbase());
    sprintf (pgm, "-%ld", total);
    if (percent)
	sprintf (num, "%d%%", percent);
    else
	sprintf (num, "%d", count);

    if (*cellfile == 0) cellfile = "-";
    if (cat_zero)
	execl (command, pgm, G_program_name(), "-z",
	    input, cellfile, num, *sitefile?sitefile:NULL, NULL);
    else
	execl (command, pgm, G_program_name(),
	    input, cellfile, num, *sitefile?sitefile:NULL, NULL);

    G_fatal_error ("Unable to execute Grandom");
}
