
#include 	<stdio.h>
#include	<string.h>
#include 	"gis.h"
#include	"gifbase.h"
#include	"readgif.h"
#include	"gifreade.h"
#include	"expander.h"

extern	int	scan_lines;
struct Colors color;
char *layer;
int row, col;
int nrows, ncols, rowpass;
int cf;
CELL *cell;
FILE *gif_file;

short int	write_GIF_pixel(), write_GIF_pixel_lace();
unsigned char	GIF_pixel ;

main (argc, argv) char *argv[];
{
	char *input;
	char title[1024];
	struct Cell_head cellhd;
	int j;
	long x;

	char
	LetterAnswer[2],
	    input_name[25],
	    output_name[25];
	unsigned
	done;
	short
	GIF_screen_width,
	    GIF_screen_height,
	    GIF_color_res,
	    GIF_fill_color,
	    GIF_default_color_cnt,
	    GIF_left_edge,
	    GIF_top_edge,
	    GIF_width,
	    GIF_height,
	    GIF_interlaced,
	    GIF_img_color_cnt,
	    i,
	    status;

	static struct ColorEntry
	GIF_default_map[MaxColors],
	GIF_img_map[MaxColors];

	struct Option *inopt, *outopt, *titleopt;
	struct Flag *vflag;
	int Verbose = 0;
	char command[256];

	/* Access and prepare the GIF file */

	G_gisinit (argv[0]);

	inopt = G_define_option();
	outopt = G_define_option();
	titleopt = G_define_option();

	inopt->key		= "input";
	inopt->type		= TYPE_STRING;
	inopt->required	= YES;
	inopt->description	= "Name of input GIF file.";

	outopt->key		= "output";
	outopt->type		= TYPE_STRING;
	outopt->required	= YES;
	outopt->gisprompt	= "new,cell,raster";
	outopt->description	= "Name of new raster file.";

	titleopt->key	= "title";
	titleopt->type	= TYPE_STRING;
	titleopt->required	= NO;
	titleopt->description	= "Title for new raster file";

	vflag = G_define_flag();
	vflag->key		= 'v';
	vflag->description	= "Verbose mode on.";

	if(G_parser(argc, argv))
		exit(-1);

	input = inopt->answer;
	layer = outopt->answer;
	Verbose = vflag->answer;
	*title = '\0';
	if (titleopt->answer != NULL)
		G_strcpy(title,titleopt->answer);
	G_strip (title);

	gif_file = fopen (input, "rb");
	if (gif_file == NULL)
		G_fatal_error("Can't open GIF file.");
	status = ReadScreenDesc(
	    next_GIF_byte,
	    & GIF_screen_width,
	    & GIF_screen_height,
	    & GIF_color_res,
	    & GIF_fill_color,
	    & GIF_default_color_cnt,
	    GIF_default_map,
	    MaxColors
	    );
	if ( status == 0 )
	{			/* GIF data set not acceptable */
		printf( "\n1. GIF file not acceptable for conversion\n" );
		return;
	};

	/* Get character after header */
	if (next_GIF_byte() != ','){
		printf ("no match \n");
		exit (-1);
	}
	if (! ReadImageDesc( next_GIF_byte,
	    & GIF_left_edge,
	    & GIF_top_edge,
	    & GIF_width,
	    & GIF_height,
	    & GIF_interlaced,
	    & GIF_img_color_cnt,
	    GIF_img_map,
	    MaxColors ) )
	{
		printf( "\n2. GIF file not acceptable for conversion\n" );
		return;
	};
	if (Verbose)
		printf("ImageDesc l %d, t %d, w %d, h %d\n",GIF_left_edge, GIF_top_edge,GIF_width, GIF_height);

	cellhd.zone = G_zone();
	cellhd.proj = G_projection();
	nrows = cellhd.rows = cellhd.north = GIF_height;
	ncols = cellhd.cols = cellhd.east = GIF_width;
	cellhd.south = cellhd.west = 0.0;
	cellhd.ns_res = 1;
	cellhd.ew_res = 1;

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

	G_set_cell_format(0); /* open with 1 byte per cell */
	cell = G_allocate_cell_buf();
	cf = G_open_cell_new_random (layer);
	if (!cf)
	{
		char msg[100];
		sprintf (msg, "unable to create layer %s", layer);
		G_fatal_error (msg);
		exit(1);
	}
	row = col = 0;
	/* Test compatibility */

	if ( GIF_interlaced ) {
		if (Verbose)
			printf( "\nInterlaced GIF\n" );
		rowpass = 1;
		status = Expand_Data( next_GIF_byte,
		    write_GIF_pixel_lace );
	} else{
		status = Expand_Data( next_GIF_byte,
		    write_GIF_pixel);
	};

	if ( status != 0 )
	{
		printf( "\n3. GIF file not acceptable for conversion\n" );
		return;
	};


	if (Verbose)
		fprintf (stderr, "CREATING SUPPORT FILES FOR %s\n", layer);
	G_close_cell (cf);
	if (*title)
		G_put_cell_title (layer, title);
	G_write_colors(layer, G_mapset(), &color);
	/*G_strcpy(command, "r.compress map=");
	G_strcat(command, outopt->answer);
	if(Verbose)
		fprintf(stderr, "%s\n", command);
	system(command);*/
}


short int	write_GIF_pixel_lace(GIF_pixel)
unsigned char	GIF_pixel ;
{
	cell[col] = (CELL)GIF_pixel;
	if (col == ncols){
		col = 0;
		G_put_map_row_random(cf, cell, row, col, ncols);
		switch (rowpass){
		case 1:
			row += 8;
			if ( row >= nrows){
				row = 4;
				rowpass++;
			}
			break;
		case 2:
			row += 8;
			if ( row >= nrows){
				row = 2;
				rowpass++;
			}
			break;
		case 3:
			row += 4;
			if ( row >= nrows){
				row = 1;
				rowpass++;
			}
			break;
		case 4:
			row += 2;
			break;
		}
	}
	col++;
	return(0);
} /* write_GIF_pixel */


short int	write_GIF_pixel(GIF_pixel)
unsigned char	GIF_pixel ;
{
	cell[col] = (CELL)GIF_pixel;
	if (col == ncols){
		col = 0;
		G_put_map_row_random(cf, cell, row, col, ncols);
		row ++;
	}
	col++;
	return(0);
} /* write_GIF_pixel */

