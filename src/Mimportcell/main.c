#include "gis.h"

FILE *Tmp_fd = NULL;
char *Tmp_file = NULL;

main (argc, argv) char *argv[];
{
    char *input;
    char *layer;
    char title[1024];
    FILE *fd;
    int cf;
    struct Cell_head cellhd;
    CELL *cell;
    int row, col;
    int nrows, ncols;
    int i;
    long x;


    G_gisinit (argv[0]);

    if (argc < 3 || argc > 4)
	usage (argv[0]);

    input = argv[1];
    layer = argv[2];
    *title = 0;
    if (argc > 3)
    {
	strcpy (title, argv[3]);
	for (i = 4; i < argc; i++)
	{
	    strcat (title, " ");
	    strcat (title, argv[i]);
	}
    }
    G_strip (title);


    if (!strcmp ("-", input))
    {
	Tmp_file = G_tempfile ();
	if (NULL == (Tmp_fd = fopen (Tmp_file, "w+")))
	    perror (Tmp_file), exit (1);
	unlink (Tmp_file);
	if (0 > file_cpy (stdin, Tmp_fd))
	    exit (1);
	fd = Tmp_fd;
    }
    else
	fd = fopen (input, "r");

    if (fd == NULL)
    {
	perror (input);
	usage (argv[0]);
    }

    if(!gethead (fd, &cellhd))
    {
	fprintf (stderr, "Can't proceed\n");
	exit(1);
    }

    nrows = cellhd.rows;
    ncols = cellhd.cols;
    if(G_set_window (&cellhd) < 0)
	exit(3);

    if (nrows != G_window_rows())
    {
	fprintf (stderr, "OOPS: rows changed from %d to %d\n", nrows, G_window_rows());
	exit(1);
    }
    if (ncols != G_window_cols())
    {
	fprintf (stderr, "OOPS: cols changed from %d to %d\n", ncols, G_window_cols());
	exit(1);
    }


    cell = G_allocate_cell_buf();
    cf = G_open_cell_new (layer);
    if (cf < 0)
    {
	char msg[100];
	sprintf (msg, "unable to create layer %s", layer);
	G_fatal_error (msg);
	exit(1);
    }
    for (row = 0; row < nrows; row++)
    {
	for (col = 0; col < ncols; col++)
	{
	    if (fscanf (fd, " %ld", &x) != 1)
	    {
		char msg[100];
		G_unopen_cell (cf);
		sprintf (msg, "data conversion failed at row %d, col %d\n", row+1, col+1);
		G_fatal_error (msg);
		exit(1);
	    }
	    cell[col] = (CELL)x;
	}
	G_put_map_row (cf, cell);
    }
    fprintf (stderr, "CREATING SUPPORT FILES FOR %s\n", layer);
    G_close_cell (cf);
    if (*title)
	G_put_cell_title (layer, title);

    exit (0);
}

usage (me)
    char *me;
{
    fprintf (stderr, "usage: %s inputfile layername [layertitle]\n",me);
    exit(1);
}

leave (code)
{
    if (Tmp_fd != NULL)
    {
	fclose (Tmp_fd);
	unlink (Tmp_file);
    }
    exit (code);
}

file_cpy (from, to)
    FILE *from, *to;
{
    char buf[BUFSIZ];
    long size;
    int  written = 0;

    while (1)
    {
	size = fread (buf, 1, BUFSIZ, from);
	if (!size)
	{
	    if (written)
	    {
		fflush (to);
		fseek (to, 0l, 0);
	    }
	    return (0);
	}
	if (!fwrite (buf, 1, size, to))
	{
	    perror ("file copy");
	    return (-1);
	}
	written = 1;
    }
    /* NOTREACHED */
}
