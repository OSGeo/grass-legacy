#include <stdlib.h>
#include <unistd.h>
#include "gis.h"

FILE *Tmp_fd = NULL;
char *Tmp_file = NULL;
int file_cpy(FILE *, FILE *);

static int oldval = 0, newval = 0;

int value (unsigned char *data, int n, int sflag)
{
    int v;

    v = *data++;
    if (sflag && v > 127) v -= 256;
    while (--n > 0) v = v * 256 + *data++;

    return (v == oldval) ? newval : v;
}

int main (int argc, char *argv[])
{
	char *input;
	char *output;
	char *title;
	FILE *fd;
	int cf;
	struct Cell_head cellhd;
	CELL *cell;
	int row, col;
	int nrows, ncols;
	double mult_fact ;
	int bytes, sflag;
	unsigned char *x, *y;
	char *err;
	char dummy[2];
	struct
	{
		struct Option *input, *output, *title, *mult, *bytes,
			      *north, *south, *east, *west,
			      *rows, *cols, *subst;
	} parm;
	struct
	{
	    struct Flag *s;
	} flag;
	char *G_adjust_Cell_head();
	int G_scan_northing();
	int G_scan_easting();

	G_gisinit (argv[0]);

	parm.input = G_define_option();
	parm.input->key = "input";
	parm.input->type = TYPE_STRING;
	parm.input->required = YES;
	parm.input->description = "Bin raster file to be imported";

	parm.output = G_define_option();
	parm.output->key = "output";
	parm.output->type = TYPE_STRING;
	parm.output->required = YES;
	parm.output->description = "Name for resultant raster map";
	parm.output->gisprompt = "any,cell,raster";

	parm.title = G_define_option();
	parm.title->key = "title";
	parm.title->key_desc = "\"phrase\"";
	parm.title->type = TYPE_STRING;
	parm.title->required = NO;
	parm.title->description = "Title for resultant raster map";

	parm.mult = G_define_option();
	parm.mult->key = "mult";
	parm.mult->type = TYPE_DOUBLE;
	parm.mult->answer = "1.0";
	parm.mult->required = NO;
	parm.mult->description = "Multiplier for bin data" ;

	parm.bytes = G_define_option();
	parm.bytes->key = "bytes";
	parm.bytes->type = TYPE_INTEGER;
	parm.bytes->answer = "1";
	parm.bytes->required = NO;
	parm.bytes->description = "Number of bytes per cell (1, 2, 4)" ;

	parm.north = G_define_option();
	parm.north->key = "north";
	parm.north->type = TYPE_DOUBLE;
	parm.north->required = YES;
	parm.north->description = "Northern limit of geographic region" ;

	parm.south = G_define_option();
	parm.south->key = "south";
	parm.south->type = TYPE_DOUBLE;
	parm.south->required = YES;
	parm.south->description = "Southern limit of geographic region" ;

	parm.east = G_define_option();
	parm.east->key = "east";
	parm.east->type = TYPE_DOUBLE;
	parm.east->required = YES;
	parm.east->description = "Eastern limit of geographic region" ;

	parm.west = G_define_option();
	parm.west->key = "west";
	parm.west->type = TYPE_DOUBLE;
	parm.west->required = YES;
	parm.west->description = "Western limit of geographic region" ;

	parm.rows = G_define_option();
	parm.rows->key = "r";
	parm.rows->type = TYPE_DOUBLE;
	parm.rows->required = YES;
	parm.rows->description = "Number of rows";

	flag.s = G_define_flag();
	flag.s->key = 's';
	flag.s->description = "Signed data (high bit means negative value)";

	parm.cols = G_define_option();
	parm.cols->key = "c";
	parm.cols->type = TYPE_DOUBLE;
	parm.cols->required = YES;
	parm.cols->description = "Number of columns";

	parm.subst = G_define_option();
	parm.subst->key = "subst";
	parm.subst->key_desc = "old,new" ;
	parm.subst->type = TYPE_STRING;
	parm.subst->answer = "0,0";
	parm.subst->required = NO;
	parm.subst->description = "Replace 1st value by 2nd one.";

	if (G_parser(argc,argv))
		exit(1);
	input = parm.input->answer;
	output = parm.output->answer;
	if(title = parm.title->answer)
		G_strip (title);
	if (sscanf(parm.mult->answer,"%lf%1s",&mult_fact, dummy) != 1) return 1;
	if (sscanf(parm.bytes->answer,"%d%1s",&bytes, dummy) != 1) return 1;
	if (bytes <= 0) return 1;
	sflag = flag.s->answer;

	cellhd.zone = G_zone();
	cellhd.proj = G_projection();
	if (! G_scan_northing(parm.north->answer, &cellhd.north, cellhd.proj)) return 1;
	if (! G_scan_northing(parm.south->answer, &cellhd.south, cellhd.proj)) return 1;
	if (! G_scan_easting (parm.east->answer,  &cellhd.east,  cellhd.proj)) return 1;
	if (! G_scan_easting (parm.west->answer,  &cellhd.west,  cellhd.proj)) return 1;
	if (sscanf(parm.rows->answer,"%d%1s",&cellhd.rows, dummy) != 1
	|| cellhd.rows <= 0) return 1;
	if (sscanf(parm.cols->answer,"%d%1s",&cellhd.cols, dummy) != 1
	|| cellhd.cols <= 0) return 1;
	if (sscanf(parm.subst->answer,"%d,%d%1s",&oldval,&newval,dummy) != 2) return 1;
	if (err = G_adjust_Cell_head (&cellhd, 1, 1)) {
		fprintf(stderr, "%s\n", err);
		return 1;
	}

	if (strcmp ("-", input) == 0)
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
		G_usage();
		exit(-1) ;
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
	cf = G_open_cell_new (output);
	if (cf < 0)
	{
		char msg[100];
		sprintf (msg, "unable to create raster map %s", output);
		G_fatal_error (msg);
		exit(1);
	}
	x = malloc(ncols * bytes);
	if (x == NULL) {
		fprintf(stderr, "Impossible allocation\n");
		exit(1);
	}
	for (row = 1; row <= nrows; row++)
	{
		if (fread (x, bytes * ncols, 1, fd) != 1)
		{
			char msg[100];
			G_unopen_cell (cf);
			sprintf (msg, "data conversion failed at row %d\n", row);
			G_fatal_error (msg);
			exit(1);
		}
		for (col = 0, y = x; col < ncols; col++, y += bytes)
		 	cell[col] = (CELL)(value(y, bytes, sflag) * mult_fact);
		G_percent(row, nrows, 2);
		G_put_map_row (cf, cell);
	}
	fprintf (stderr, "CREATING SUPPORT FILES FOR %s\n", output);
	G_close_cell (cf);
	if (title)
		G_put_cell_title (output, title);

	exit (0);
}

int 
file_cpy (FILE *from, FILE *to)
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
