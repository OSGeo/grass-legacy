#include "config.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "gis.h"
#include "local_proto.h"

FILE *Tmp_fd = NULL;
char *Tmp_file = NULL;

int main (int argc, char *argv[])
{
	char *input;
	char *output;
	char *title;
	FILE *fd;
	int cf;
	struct Cell_head cellhd;
	CELL *cell;
	FCELL *fcell;
	DCELL *dcell;
	int row, col;
	int nrows, ncols;
	int i;
	static int missingval;
	int rtype;
	double mult_fact ;
	double x;
	struct GModule *module;
	struct
	{
		struct Option *input, *output, *type, *title, *mult ;
	} parm;


	G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Convert an ESRI ARC/INFO ascii raster file (GRID) "
		"into a (binary) raster map layer.";

	parm.input = G_define_option();
	parm.input->key = "input";
	parm.input->type = TYPE_STRING;
	parm.input->required = YES;
	parm.input->description = "ARC/INFO ascii raster file (GRID) to be imported";

	parm.output = G_define_option();
	parm.output->key = "output";
	parm.output->type = TYPE_STRING;
	parm.output->required = YES;
	parm.output->description = "Name for resultant raster map";
	parm.output->gisprompt = "any,cell,raster";

	parm.type = G_define_option();
	parm.type->key = "type";
	parm.type->type = TYPE_STRING;
	parm.type->required = NO;
	parm.type->options = "CELL,FCELL,DCELL";
        parm.type->answer = "FCELL";
	parm.type->description = "Storage type for resultant raster map";

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
        parm.mult->description = "Multiplier for ascii data" ;
                                                


	if (G_parser(argc,argv))
		exit(1);
	input = parm.input->answer;
	output = parm.output->answer;
	if(title = parm.title->answer)
		G_strip (title);

	sscanf(parm.mult->answer,"%lf",&mult_fact) ;
	if (strcmp ("CELL", parm.type->answer) == 0)
	    rtype = CELL_TYPE;
	else if (strcmp ("DCELL", parm.type->answer) == 0)
	    rtype = DCELL_TYPE;
	else 
	    rtype = FCELL_TYPE;
	
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

	if(!gethead (fd, &cellhd, &missingval))
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

        switch (rtype){
	    case CELL_TYPE:
		cell = G_allocate_c_raster_buf();
		break;
	    case FCELL_TYPE:
		fcell = G_allocate_f_raster_buf();
		break;
	    case DCELL_TYPE:
		dcell = G_allocate_d_raster_buf();
		break;
	}
	cf = G_open_raster_new (output, rtype);
	if (cf < 0)
	{
		char msg[100];
		sprintf (msg, "unable to create raster map %s", output);
		G_fatal_error (msg);
		exit(1);
	}
	
	for (row = 0; row < nrows; row++)
	{
		G_percent(row, nrows, 5);
		for (col = 0; col < ncols; col++)
		{
			if (fscanf (fd, "%lf", &x) != 1)
			{
				char msg[100];
				G_unopen_cell (cf);
				sprintf (msg, "data conversion failed at row %d, col %d\n", row+1, col+1);
				G_fatal_error (msg);
				exit(1);
			}
			switch (rtype){
			    case CELL_TYPE:
				if((int)x == missingval)
				   G_set_c_null_value (cell+col, 1);
				else
				   cell[col] = (CELL)x * mult_fact;
				break;
			    case FCELL_TYPE:
				if((int)x == missingval)
				   G_set_f_null_value (fcell+col, 1);
				else
				   fcell[col] = (FCELL)x * mult_fact;
				break;
			    case DCELL_TYPE:
				if((int)x == missingval)
				   G_set_d_null_value (dcell+col, 1);
				else
				   dcell[col] = (DCELL)x * mult_fact;
				break;
			}
		}
		switch (rtype){
		    case CELL_TYPE:
			G_put_c_raster_row (cf, cell);
			break;
		    case FCELL_TYPE:
			G_put_f_raster_row (cf, fcell);
			break;
		    case DCELL_TYPE:
			G_put_d_raster_row (cf, dcell);
			break;
		}
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
