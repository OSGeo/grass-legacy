#include "gis.h"

#ifndef NOT_SUN
# include <rasterfile.h>
#else
# define RAS_MAGIC 0x59a66a95
# define RT_STANDARD 1
# define RT_BYTE_ENCODED 2
# define RMT_EQUAL_RGB 1
#endif

int verbose;

main(argc, argv) char *argv[];
{
    char rasterfile[1024];
    char cellfile[1024];
    struct Cell_head window;
    struct Colors colors;
    int nrows, ncols;
    int fd1, fd2;
    struct Option *input, *output;
    struct Flag *quiet;

    G_gisinit (argv[0]);
    G_get_window (&window);

    input = G_define_option();
    input->key = "input";
    input->type = TYPE_STRING;
    input->description = "Sun rasterfile to input";
    input->required = YES;

    output = G_define_option();
    output->key = "output";
    output->type = TYPE_STRING;
    output->description = "GRASS raster map to be created";
    output->required = YES;
    output->gisprompt = "any,cell,raster";

    quiet = G_define_flag();
    quiet->key = 'q';
    quiet->description = "Run quietly";

    if (G_parser(argc,argv))
	exit(1);

    verbose = !quiet->answer;

    fd1 = open_rasterfile (input->answer, &nrows, &ncols, &colors);
    window.north = -0.5;
    window.south = -(nrows+0.5);
    window.west = 0.5;
    window.east = ncols + 0.5;
    window.ns_res = window.ew_res = 1.0;
    G_set_window (&window);

    fd2 = open_cellfile (output->answer);

    if(rasttocell (fd1, fd2, nrows, ncols) < 0)
    {
	perror (input->answer);
	exit(1);
    }
    close (fd1);
    G_close_cell (fd2);
    G_write_colors (output->answer, G_mapset(), &colors);
    exit(0);
}

open_cellfile (name)
    char *name;
{
    int fd;

    fd = G_open_cell_new (name);
    if (fd < 0) exit(1);
    return fd;
}

open_rasterfile(name, nrows, ncols, colors)
    char *name;
    int *nrows, *ncols;
    struct Colors *colors;
{
    int fd;

    fd = open (name, 0);
    if (fd < 0)
    {
	perror (name);
	exit(1);
    }

    switch (read_header(fd, nrows, ncols, colors))
    {
    case -1:
	fprintf (stderr, "%s - not a sun rasterfile\n", name);
	exit(1);
    case 0:
	fprintf (stderr, "%s - this rasterfile format not supported\n", name);
	exit(1);
    default:
	return fd;
    }
}


read_header (fd, nrows, ncols, colors)
    int *nrows, *ncols;
    struct Colors *colors;
{
    int x;
    CELL cat;
    unsigned char red[256], grn[256], blu[256];

    if (read(fd, &x, sizeof x) != sizeof x) return -1;
    if (x != RAS_MAGIC) return -1;

    if (read(fd, &x, sizeof x) != sizeof x) return -1;
    *ncols = x;
    if (*ncols <= 0) return -1;
    if (read(fd, &x, sizeof x) != sizeof x) return -1;
    *nrows = x;
    if (*nrows <= 0) return -1;

    if (read(fd, &x, sizeof x) != sizeof x) return -1;
    if (x != 8) return 0;	/* can't do this format */

    if (read(fd, &x, sizeof x) != sizeof x) return -1; /* length - don't need */
    if (read(fd, &x, sizeof x) != sizeof x) return -1;
    if (x != RT_STANDARD) return 0; /* can't do this format */

    if (read(fd, &x, sizeof x) != sizeof x) return -1;
    if (x != RMT_EQUAL_RGB) return 0; /* can't do this format */

    if (read(fd, &x, sizeof x) != sizeof x) return -1;
    if (x%3) return 0;
    x /= 3;
    if (x > 256) return 0;

    if (read(fd, red, x) != x) return -1;
    if (read(fd, grn, x) != x) return -1;
    if (read(fd, blu, x) != x) return -1;

    G_init_colors (colors);
    for (cat = 0; cat < x; cat++)
	G_set_color (cat, (int)red[cat], (int)grn[cat], (int)blu[cat], colors);

    if (verbose)
	fprintf (stderr, "rasterfile is %d rows by %d columns\n", *nrows, *ncols);
    return 1;
}

rasttocell (rast_fd, cell_fd, nrows, ncols)
{
    unsigned char *raster;
    CELL *cell;
    int row, col;

    cell = G_allocate_cell_buf();
    raster = (unsigned char *) G_malloc(ncols);

    if (verbose)
	fprintf (stderr, "complete ... ");
    for (row = 0; row < nrows; row++)
    {
	if (verbose)
	    G_percent (row, nrows, 2);
	if (read (rast_fd, raster, ncols) != ncols)
	    return -1;
	for (col = 0; col < ncols; col++)
	    cell[col] = (CELL) raster[col];
	if (G_put_map_row (cell_fd, cell) < 0)
	    exit(1);
    }
    if (verbose)
	G_percent (row, nrows, 2);
    return 1;
}
