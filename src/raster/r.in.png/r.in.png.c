/* code based on r.in.gif by
   Michael Shapiro, U.S.Army Construction Engineering Research Laboratory
 */

#include 	<stdio.h>
#include	<string.h>
#include 	"gis.h"
#include	"png.h"

extern	int	scan_lines;
struct Colors color;
char *layer;
int row, col;
int nrows, ncols, rowpass;
int cf;
CELL *cell;
FILE *PNG_file;

short int	write_PNG_pixel(), write_PNG_pixel_lace();
unsigned char	PNG_pixel ;

int main(int argc, char *argv[])
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
	PNG_screen_width,
	    PNG_screen_height,
	    PNG_color_res,
	    PNG_fill_color,
	    PNG_default_color_cnt,
	    PNG_left_edge,
	    PNG_top_edge,
	    PNG_width,
	    PNG_height,
	    PNG_interlaced,
	    PNG_img_color_cnt,
	    i,
	    status;

	static struct ColorEntry
	PNG_default_map[MaxColors],
	PNG_img_map[MaxColors];

	struct Option *inopt, *outopt, *titleopt;
	struct Flag *vflag;
	int Verbose = 0;
	char command[256];

	/* Access and prepare the PNG file */

	G_gisinit (argv[0]);

	inopt = G_define_option();
	outopt = G_define_option();
	titleopt = G_define_option();

	inopt->key		= "input";
	inopt->type		= TYPE_STRING;
	inopt->required	= YES;
	inopt->description	= "Name of input PNG file.";

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

	PNG_file = fopen (input, "rb");
	if (PNG_file == NULL)
		G_fatal_error("Can't open PNG file.");
	fread(header, 1, number, fp);
        is_png = !png_sig_cmp(header, 0, number);
        if (!is_png) 
	{			/* PNG data set not acceptable */
	  fprintf (stderr, "\n1. PNG file not acceptable for conversion\n" );
		return 1;
	};

	/* Get character after header */
	if (next_PNG_byte() != ','){
		fprintf (stdout,"no match \n");
		exit (-1);
	}
	if (! ReadImageDesc( next_PNG_byte,
	    & PNG_left_edge,
	    & PNG_top_edge,
	    & PNG_width,
	    & PNG_height,
	    & PNG_interlaced,
	    & PNG_img_color_cnt,
	    PNG_img_map,
	    MaxColors ) )
	{
		fprintf (stdout, "\n2. PNG file not acceptable for conversion\n" );
		return 1;
	};
	if (Verbose)
		fprintf (stdout,"ImageDesc l %d, t %d, w %d, h %d\n",PNG_left_edge, PNG_top_edge,PNG_width, PNG_height);

	cellhd.zone = G_zone();
	cellhd.proj = G_projection();
	nrows = cellhd.rows = cellhd.north = PNG_height;
	ncols = cellhd.cols = cellhd.east = PNG_width;
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
	/*cf = G_open_cell_new_random (layer);*/
	cf = G_open_cell_new(layer);
	if (!cf)
	{
		char msg[100];
		sprintf (msg, "unable to create layer %s", layer);
		G_fatal_error (msg);
		exit(1);
	}
	row = col = 0;
	/* Test compatibility */

	if ( PNG_interlaced ) {
		if (Verbose)
			fprintf (stdout, "\nInterlaced PNG\n" );
		rowpass = 1;
		status = Expand_Data( next_PNG_byte,
		    write_PNG_pixel_lace );
	} else{
		status = Expand_Data( next_PNG_byte,
		    write_PNG_pixel);
	};

	if ( status != 0 )
	{
		fprintf (stdout, "\n3. PNG file not acceptable for conversion\n" );
		return 1;
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

    exit (0);
}


short int	write_PNG_pixel_lace(PNG_pixel)
unsigned char	PNG_pixel ;
{
	cell[col] = (CELL)PNG_pixel;
	if (col == ncols){
		col = 0;
		/*G_put_map_row_random(cf, cell, row, col, ncols);*/
		G_put_c_raster_row(cf, cell);
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
} /* write_PNG_pixel */


short int write_PNG_pixel(PNG_pixel)
unsigned char	PNG_pixel ;
{
	cell[col] = (CELL)PNG_pixel;
	if (col == ncols){
		col = 0;
		G_put_c_raster_row(cf, cell);
		row ++;
	}
	col++;
	return(0);
} /* write_PNG_pixel */

