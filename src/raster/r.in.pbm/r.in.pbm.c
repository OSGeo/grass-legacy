#include <stdio.h>
#include <ctype.h>
#include "gis.h"

int get_ppm_colors(FILE *, int, int);
CELL lookup_color(int, int, int, int);

#define MAXCOLOR 32768

struct mycolor {
	int red;
	int grn;
	int blu;
} ppm_color[MAXCOLOR];

int main (int argc, char *argv[])
{
	FILE *infp;
	CELL *cell,*cellptr;
	char *name;
	int outfp;
	int row,col;
	int nrows, ncols;
	int x, c,c1, do_pbm;
	int ch;
	struct Colors colors, *pcolr;
	int red,grn,blu,num_colors;
	struct Option *inopt, *outopt;
	struct Flag *vflag;
	int Verbose = 0;
	int ppm_height, ppm_width, ppm_maxval, ppm_magic, ppm_head;
	struct Cell_head cellhd;
	long ppm_pos;

	pcolr = &colors;

	G_gisinit (argv[0]);
	inopt = G_define_option();
	outopt = G_define_option();

	inopt->key              = "input";
	inopt->type             = TYPE_STRING;
	inopt->required 	= YES;
	inopt->description      = "Name of existing PPM file.";

	outopt->key             = "output";
	outopt->type   		= TYPE_STRING;
	outopt->required        = YES;
	outopt->gisprompt	= "new,cell,raster";
	outopt->description     = "Name of new raster file.";

	vflag = G_define_flag();
	vflag->key              = 'v';
	vflag->description      = "Verbose mode on.";

	if(G_parser(argc, argv))
		exit(-1);

	Verbose = vflag->answer;

	if((infp = fopen(inopt->answer, "r")) == NULL)
		G_fatal_error("Can't open PPM file for read.");
	name = outopt->answer;

	G_init_colors(pcolr);

	ppm_height = ppm_width = ppm_maxval = ppm_magic = ppm_head = 0;

	while((c = fgetc(infp)) != EOF && !ppm_head){
		switch (c){
		case 'P':
			ppm_magic = fgetc(infp);
			do_pbm = (ppm_magic == '1' || ppm_magic == '4');
			if (Verbose)
				fprintf (stdout,"Magic = P%c\n", (char)ppm_magic);
			break;
		case '#':
			if (Verbose) fprintf (stdout,"Comment = #");
			while((c1 = fgetc(infp)) != EOF && c1 != '\n')
				if (Verbose)
					putchar(c1);
			putchar('\n');
			break;
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
		case '0':
			if (!ppm_width || !ppm_height || (!do_pbm && !ppm_maxval))
				ungetc(c, infp);
			if (!ppm_width){
				fscanf(infp, "%d", &ppm_width);
				if (Verbose)
					fprintf (stdout,"Width = %d\n", ppm_width);
				break;
			}
			if (!ppm_height){
				fscanf(infp, "%d", &ppm_height);
				if (Verbose)
					fprintf (stdout,"Height = %d\n", ppm_height);
				if(do_pbm) ppm_head = 1;
				break;
			}
			if (!do_pbm && !ppm_maxval){
				fscanf(infp, "%d", &ppm_maxval);
				if (Verbose)
					fprintf (stdout,"Maxval = %d\n", ppm_maxval);
				ppm_head = 1;
				break;
			}
			break;
		default:
			break;
		}
	}
	ppm_pos = ftell(infp);
	num_colors = get_ppm_colors(infp, ppm_width * ppm_height, ppm_magic);
	if (Verbose)fprintf (stdout,"Total colors = %d\n", num_colors);
	fseek(infp, ppm_pos, 0); /* get back where we were */

	nrows = cellhd.rows = ppm_height;
	ncols = cellhd.cols = ppm_width;
	cellhd.proj = G_projection();
	cellhd.zone = G_zone();
	cellhd.ew_res = cellhd.ns_res = 1.0;
	cellhd.north = (double) ppm_height;
	cellhd.east = (double) ppm_width;
	cellhd.west = cellhd.south = 0.0;
	G_set_window(&cellhd);
/*
	G_set_cell_format(0);
*/
	if((outfp = G_open_cell_new (name)) < 0)
		G_fatal_error("Can't open new raster file.");
	cell = G_allocate_cell_buf();

	if (Verbose) fprintf(stderr, "Percent Complete: ");
	
	for (row = 0; row < nrows; row++)
	{
		cellptr = cell;
		if(Verbose) G_percent(row, nrows, 5);
		for (col = 0; col < ncols; col++){
			switch (ppm_magic){
			case '1':   /* ascii bitmap */
				fscanf(infp, "%d", &x);
				if(x == 1)
				    *cellptr++ = 1;
				else if (x == 0)
				    *cellptr++ = 0;
				/* otherwise got junk */
				break;
			case '4':   /* raw bitmap */
				for(x=0; col < ncols; col++, x++){
				    x &= 7;
				    if(!x){
					ch = fgetc(infp);
				    }
				    *cellptr++ = (ch&0x80) ? 1 : 0;
				    ch = ch << 1;
				}
				break;
			case '6':
				red = fgetc(infp);
				grn = fgetc(infp);
				blu = fgetc(infp);
				*cellptr++ = lookup_color(red,grn,blu,num_colors);

				break;
			case '3':
				fscanf(infp, "%d %d %d", &red, &grn, &blu);
				*cellptr++ = lookup_color(red,grn,blu,num_colors);
				break;
			}
		}
		if (G_put_map_row(outfp, cell) < 0 )
			G_fatal_error("Can't write new raster row!!");
	}
	if(Verbose) G_percent(row,nrows, 5);
	G_close_cell(outfp);
	/*G_put_cellhd(outopt->answer, &cellhd);*/
	if (Verbose) 
	    fprintf (stdout,"Writing color table for %d values", num_colors);
	for(x=0;x<num_colors;x++){
	    G_set_color((CELL)x, ppm_color[x].red, ppm_color[x].grn,
			ppm_color[x].blu, pcolr);
	}
	if(G_write_colors(outopt->answer, G_mapset(), pcolr) < 0)
	    G_fatal_error("Can't write color table!!");
	exit(0);
}

int get_ppm_colors (FILE *infp, int pixels, int ppm_magic)
{
	int count,x,maxcolor,match;
	int red, grn, blu;

	match = maxcolor = 0;

	if (ppm_magic == '1' || ppm_magic == '4'){ /* bitmap */
	    ppm_color[0].red = 255;
	    ppm_color[0].grn = 255;
	    ppm_color[0].blu = 255;
	    ppm_color[1].red = 0;
	    ppm_color[1].grn = 0;
	    ppm_color[1].blu = 0;
	    return(2);
	}
	for (count=0; count < pixels; count++){
		match = 0;
		switch (ppm_magic){
		case '6':
			red = fgetc(infp);
			grn = fgetc(infp);
			blu = fgetc(infp);
			break;
		case '3':
			fscanf(infp, "%d %d %d", &red, &grn, &blu);
			break;
		default:
			G_fatal_error("Unknown ppm magic number!!");
			break;
		}
		for (x=0;x<maxcolor;x++){
			if (ppm_color[x].red == red && 
			    ppm_color[x].grn == grn &&
			    ppm_color[x].blu == blu){
				match = 1;
				break;
			}
		}
		if (match == 0){
			ppm_color[maxcolor].red = red;
			ppm_color[maxcolor].grn = grn;
			ppm_color[maxcolor].blu = blu;
			maxcolor++;
			if (maxcolor == MAXCOLOR)
				G_fatal_error("Exceeded maximum colors!!");
		}
	}
	return(maxcolor);
}

CELL 
lookup_color (int r, int g, int b, int num)
{
	int x;

	for (x=0;x<num;x++){
		if (ppm_color[x].red == r && 
		    ppm_color[x].grn == g &&
		    ppm_color[x].blu == b){
			break;
		}
	}
	return((CELL)x);
}


