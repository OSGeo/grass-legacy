#include "gis.h"
#include "byte.h"
#include <fcntl.h>

FILE *Tmp_fd = NULL;
char *Tmp_file = NULL;

int main (argc, argv)
char *argv[];
{
	char *input;
	char *output;
	char title[50];
	int cf;
	FILE *fd;
	struct Cell_head cellhd;
	unsigned char *bcell;
	CELL *cell, max;
	struct Colors colors;
	struct History history;
	int row;
	int nrows, ncols;
	int i, primary;
	struct
	{
		struct Option *input, *output;
	} parm;
	struct Flag *s2nd;
	struct Flag *keephead;
	struct Categories cats;


	G_gisinit (argv[0]);

	parm.input = G_define_option();
	parm.input->key = "input";
	parm.input->type = TYPE_STRING;
	parm.input->required = YES;
	parm.input->description = "Digital OQ file to be imported";

	parm.output = G_define_option();
	parm.output->key = "output";
	parm.output->type = TYPE_STRING;
	parm.output->required = YES;
	parm.output->description = "Name for resultant raster map";
	parm.output->gisprompt = "any,cell,raster";

	s2nd = G_define_flag();
	s2nd->key = 's';
	s2nd->description = "use secondary datum";

	keephead = G_define_flag();
	keephead->key = 'h';
	keephead->description = "keep head";


	if (G_parser(argc,argv))
		exit(1);
	input = parm.input->answer;
	output = parm.output->answer;
	if (s2nd->answer)
		primary = 0;
	else
		primary = 1;

	if (strcmp ("-", input) == 0)
	{
		Tmp_file = G_tempfile ();
		if (NULL == (Tmp_fd = fopen (Tmp_file, "w+")))
			perror (Tmp_file), exit (1);
		unlink (Tmp_file);
		if (0 > file_cpy (stdin, Tmp_fd))
			exit (1);
		fclose(Tmp_fd);
		input = Tmp_file;
	}


	fd = fopen (input, "r");

	if (fd == NULL)
	{
		perror (input);
		G_usage();
		exit(-1) ;
	}

	if(!gethead (fd, &cellhd,primary,title))
	{
		fprintf (stderr, "Can't proceed\n");
		exit(1);
	}
	G_strip (title);

	nrows = cellhd.rows;
if (keephead->answer){
	nrows = nrows + bytecount;
	cellhd.north = cellhd.north + (bytecount * cellhd.ns_res);
}

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

/********* Create uncomprssed file one byte per raster *********************/
	G_set_cell_format(0);
if (!keephead->answer){
	cf = G_open_cell_new_uncompressed (output);
/************************************************************************/
	cell = G_allocate_cell_buf();
	bcell = G_malloc(ncols);
	if (cf < 0)
	{
		char msg[100];
		sprintf (msg, "unable to create raster map %s", output);
		G_fatal_error (msg);
		exit(1);
	}
	for (row = 0; row < nrows; row++)
	{
		if (fread(bcell,1,ncols,fd) != ncols) {
			char msg[100];
			sprintf(msg,"error in reading row %d",row);
			G_fatal_error (msg);
                	exit(1);
			}
		for (i=0;i<ncols;i++) {
			cell[i] = (CELL)bcell[i];
			if (max < cell[i]) max = cell[i];
			}
		G_put_map_row (cf, cell);
	}
	G_close_cell (cf);
}
else
{
	G_put_cellhd(output, &cellhd);
	G_init_cats((CELL)0, "", &cats);
	G_write_cats(output, &cats);
	max=255;
}
	fprintf (stderr, "CREATING SUPPORT FILES FOR %s\n", output);
	if (title)
		G_put_cell_title (output, title);
	G_init_colors(&colors);
	G_make_grey_scale_colors(&colors,0,max);
	if (G_write_colors (output, G_mapset(), &colors) >= 0 )
        	printf ("Color table for [%s] set to grey scale\n", output);
	G_short_history(output,"raster",&history);
	for (i=0;i < 50;i++) history.title[i] = title[i];
	history.title[i] = NULL;
	if (G_write_history(output,&history) < 0)
		fprintf(stderr,"Error in writing history file\n");

	exit (0);
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
