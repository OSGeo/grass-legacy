#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include "gis.h"
#include "rast.h"

int open_cellfile(char *);
int open_rasterfile(char *, int *, int *, int *);
int read_integer(int, int *, int);
int read_header(int, int *, int *, int *);
int rasttocell(int, int, int, int, int);

int verbose;
int adjust;

#define CUR_MAX_CLR 256
unsigned char *cats_used; /* dpg*/
unsigned char red[CUR_MAX_CLR], grn[CUR_MAX_CLR], blu[CUR_MAX_CLR];

int main (int argc, char *argv[])
{
    CELL cat;
    struct Cell_head window;
    struct Colors colors;
    int nrows, ncols, depth;
    int fd1, fd2;
	struct GModule *module;
    struct Option *input, *output;
    struct Flag *quiet, *noadjust;

    G_gisinit (argv[0]);
    G_get_window (&window);

	module = G_define_module();
	module->description =
		"Converts a SUN raster file to a GRASS raster file.";

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

    noadjust = G_define_flag();
    noadjust->key = 'a';
    noadjust->description = "Don't adjust number of rows";

    quiet = G_define_flag();
    quiet->key = 'q';
    quiet->description = "Run quietly";

    if (G_parser(argc,argv))
	exit(1);

    cats_used = (unsigned char *)G_calloc (CUR_MAX_CLR,1);	/*dpg*/

    verbose = !quiet->answer;
    adjust  = !noadjust->answer;

    fd1 = open_rasterfile (input->answer, &nrows, &ncols, &depth);
    window.north = -0.5;
    window.south = -(nrows+0.5);
    window.west = 0.5;
    window.east = ncols + 0.5;
    window.ns_res = window.ew_res = 1.0;
    G_set_window (&window);

    fd2 = open_cellfile (output->answer);

    if(rasttocell (fd1, fd2, nrows, ncols, depth) < 0)
    {
	perror (input->answer);
	exit(1);
    }
    close (fd1);
    G_close_cell (fd2);

    G_init_colors (&colors);
    for (cat = 0; cat < CUR_MAX_CLR; cat++)
      if (cats_used[cat])
	G_set_color (cat, (int)red[cat], (int)grn[cat], (int)blu[cat], &colors);

    G_write_colors (output->answer, G_mapset(), &colors);
    exit(0);
}

int open_cellfile (char *name)
{
    int fd;

    fd = G_open_cell_new (name);
    if (fd < 0) exit(1);
    return fd;
}

int open_rasterfile (char *name, int *nrows, int *ncols, int *depth)
{
    int fd;

    fd = open (name, 0);
    if (fd < 0)
    {
	perror (name);
	exit(1);
    }

    switch (read_header(fd, nrows, ncols, depth))
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


int read_integer (		/* arrange the bytes in the correct  */
    int fd,
    int *i,
    int size			/* order -- sunrast uses big-endian  */
)
{
    unsigned int temp;
    int retcode;
    unsigned char *p;

    if (size != sizeof(int)) {
	fprintf(stderr,
		"read_integer: warning: possible bogus read for size %d\n",
		size);
    }

    *i = 0;
    p = (unsigned char *) &temp;

    if ( (retcode = read(fd, &temp, sizeof(temp))) == sizeof(temp) ) {
		/* WARNING: We're assuming an int is four bytes wide! */
	*i = (p[0] <<24) + (p[1] <<16) + (p[2] <<8) + p[3];

#ifdef DEBUG
	fprintf(stderr,"read_int: read: %x|%x|%x|%x, converted to %x\n",
		p[0],p[1],p[2],p[3], *i);
#endif /* DEBUG */
    }

    return retcode;
}

int read_header (int fd, int *nrows, int *ncols, int *depth)
{
    int x;
    int colormap;

    if (read_integer(fd, &x, sizeof x) != sizeof x) return -1;
    if (x != RAS_MAGIC) return -1;

    if (read_integer(fd, &x, sizeof x) != sizeof x) return -1;
    *ncols = x;
    if (*ncols <= 0) return -1;
    if (read_integer(fd, &x, sizeof x) != sizeof x) return -1;
    *nrows = x;
    if (*nrows <= 0) return -1;

    if (read_integer(fd, &x, sizeof x) != sizeof x) return -1;
    *depth = x;
    if (*depth != 8 && *depth != 1) return 0;	/* can't do this format */

    if (read_integer(fd, &x, sizeof x) != sizeof x) return -1; /* length - don't need */
    if (read_integer(fd, &x, sizeof x) != sizeof x) return -1;
    if (x != RT_STANDARD) return 0; /* can't do this format */

    if (read_integer(fd, &x, sizeof x) != sizeof x) return -1;
    colormap = x;
    if (*depth == 8 && colormap != RMT_EQUAL_RGB) return 0; /* can't do this format */
    if (*depth == 1)
    {
	red[0] = grn[0] = blu[0] = 255;
	red[1] = grn[1] = blu[1] = 0;
    }

    if (read_integer(fd, &x, sizeof x) != sizeof x) return -1;
    if (colormap)
    {
	if (x%3) return 0;
	x /= 3;
	if (x > CUR_MAX_CLR) return 0;

	if (read(fd, red, x) != x) return -1;
	if (read(fd, grn, x) != x) return -1;
	if (read(fd, blu, x) != x) return -1;
    }

    if (verbose)
	fprintf (stderr, "rasterfile is %d rows by %d columns\n", *nrows, *ncols);
    return 1;
}

int rasttocell (int rast_fd, int cell_fd, int nrows, int ncols, int depth)
{
    unsigned char *raster;
    CELL *cell;
    int row, col, inputcols;

/* sun raster is padded to even number of cols */
    if (depth == 8)
	inputcols = ncols + (adjust?ncols%2:0);
    else
	inputcols = (ncols+7)/8;

    cell = G_allocate_cell_buf();
    raster = (unsigned char *) G_malloc(inputcols);

    if (verbose)
	fprintf (stderr, "complete ... ");
    for (row = 0; row < nrows; row++)
    {
	if (verbose)
	    G_percent (row, nrows, 2);
	if (read (rast_fd, raster, inputcols) != inputcols)
	    return -1;

	for (col = 0; col < ncols; col++)
	{
	    register unsigned char tmp;

	    if (depth == 8)
		tmp =  raster[col];
	    else
		tmp =  (raster[col>>3] & (0200 >> (col & 7))) != 0 ;
	    cats_used[tmp] = 1;		/* mark cat as existing */
	    cell[col] = (CELL) tmp;
	}

	if (G_put_map_row (cell_fd, cell) < 0)
	    exit(1);
    }
    if (verbose)
	G_percent (row, nrows, 2);
    return 1;
}
