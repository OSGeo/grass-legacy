/* Added flag to write a TIFF World file like r.out.arctiff
 * Eric G. Miller 4-Nov-2000
 */

/* removed LZW support 5/5000 */

/* Corrected G_set_window to G_get_window to make r.out.tiff sensitive
 * to region settings.   - Markus Neteler  (neteler@geog.uni-hannover.de
 * 8/98        
 */

/*
 * Portions Copyright (c) 1988, 1990 by Sam Leffler.
 * All rights reserved.
 *
 * This file is provided for unrestricted use provided that this
 * legend is included on all tape media and as a part of the
 * software program in whole or part.  Users may copy, modify or
 * distribute this file at will.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include "gis.h"
#include "rasterfile.h"
#include "tiffio.h"

#define	howmany(x, y)	(((x)+((y)-1))/(y))
#define	streq(a,b)	(strcmp(a,b) == 0)

u_short	config = PLANARCONFIG_CONTIG;
u_short	compression = -1;
u_short	rowsperstrip = 0;
int DEBUG = 0;

int write_tfw(char *, struct Cell_head *, int);

int 
main (int argc, char *argv[])
{
	u_char *buf, *tmpptr;
	int row, linebytes;
	TIFF *out;
	int in;
	struct rasterfile h;
	struct Option *inopt, *outopt, *compopt;
	struct Flag *pflag, *vflag, *tflag;
	CELL *cell, *cellptr;
	struct Cell_head cellhd;
	int col,verbose;
	char *mapset;
	struct Colors colors;
	int red, grn, blu, mapsize;

	G_gisinit(argv[0]);

	inopt = G_define_option();
	inopt->key             = "input";
	inopt->type            =  TYPE_STRING;
	inopt->required        =  YES;
	inopt->gisprompt	= "old,cell,raster";
	inopt->description     = "Existing raster file name";

	outopt = G_define_option();
	outopt->key             = "output";
	outopt->type            =  TYPE_STRING;
	outopt->required        =  YES;
	outopt->gisprompt	= "new,tiff,tiff";
	outopt->description     = "File name for new TIFF file.";

	compopt = G_define_option();
	compopt->key		= "compression";
	compopt->type		= TYPE_STRING;
	compopt->required       = NO;
	compopt->options        = "none,packbit,deflate";
	compopt->description    = "TIFF file compression";
	compopt->answer         = "none";
	
	pflag = G_define_flag();
	pflag->key		= 'p';
	pflag->description	= "TIFF Pallete output (8bit instead of 24bit).";

	tflag = G_define_flag();
	tflag->key		= 't';
	tflag->description      = "Output TIFF world file";

	vflag = G_define_flag();
	vflag->key		= 'v';
	vflag->description	= "Verbose mode.";


	if (G_parser (argc, argv))
		exit (-1);
	
	if (strncmp(compopt->answer,"packbit",7) == 0)
		compression = COMPRESSION_PACKBITS;
	else if (strncmp(compopt->answer,"deflate",7) == 0)
		compression = COMPRESSION_DEFLATE;
	else
		compression = COMPRESSION_NONE;
	
	verbose = vflag->answer;

	mapset = G_find_cell(inopt->answer, "");
	if (!mapset)
	{
		fprintf(stderr, "%s - raster map not found\n", inopt->answer);
		exit(1);
	}
	if ( (G_get_cellhd(inopt->answer, mapset, &cellhd) < 0)){
		fprintf(stderr, "%s - can't read raster cellhd\n", inopt->answer);
		exit(1);
	}
	if ((G_get_window(&cellhd) < 0))
		G_fatal_error("Can't set window");
	G_read_colors(inopt->answer, mapset, &colors);
	cell = G_allocate_cell_buf();
	if((in = G_open_cell_old (inopt->answer, mapset)) < 0){
		fprintf(stderr, "%s - can't open raster map\n", inopt->answer);
		exit(1);
	}
	out = TIFFOpen(outopt->answer, "w");
	if (out == NULL)
		exit(-4);
	h.ras_width = cellhd.cols;
	h.ras_height = cellhd.rows;
	h.ras_depth = 24;
	if (pflag->answer)
		h.ras_depth = 8;
	TIFFSetField(out, TIFFTAG_IMAGEWIDTH, h.ras_width);
	TIFFSetField(out, TIFFTAG_IMAGELENGTH, h.ras_height);
	TIFFSetField(out, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
	TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, h.ras_depth > 8 ? 3 : 1);
	TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, h.ras_depth > 1 ? 8 : 1);
	TIFFSetField(out, TIFFTAG_PLANARCONFIG, config);
	mapsize = 1<<h.ras_depth;

	if (pflag->answer) {
		register u_short *redp, *grnp, *blup, *mapptr;
		register int i;

		if (DEBUG)
			fprintf (stdout,"max %f min %f mapsize %d\n ",colors.cmax, colors.cmin,mapsize);
		mapptr = (u_short *)malloc(mapsize * 3 * sizeof (u_short));
		redp = mapptr;
		grnp = redp + mapsize;
		blup = redp + mapsize * 2;

		/* XXX -- set pointers up before we step through arrays */
#define	SCALE(x)	(((x)*((1L<<16)-1))/255)
		for (i = 0; i <= colors.cmax;i++,redp++,grnp++,blup++){
			G_get_color(i, &red, &grn, &blu, &colors);
			*redp = (u_short)(SCALE(red));
			*grnp = (u_short)(SCALE(grn));
			*blup = (u_short)(SCALE(blu));
			/*fprintf (stdout," %d : %d %d %d   %d %d %d\n", i,red,grn,blu, *redp, *grnp, *blup);*/
		}
		if ((i = colors.cmax) < mapsize) {
			i = mapsize - i;
			bzero(redp, i*sizeof (u_short));
			bzero(grnp, i*sizeof (u_short));
			bzero(blup, i*sizeof (u_short));
			redp += i;
			grnp += i;
			blup += i;
		}
		TIFFSetField(out, TIFFTAG_COLORMAP,
		    mapptr, mapptr + mapsize, mapptr + mapsize * 2);
		TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_PALETTE);
		TIFFSetField(out, TIFFTAG_COMPRESSION, compression);
	} else {
		/* XXX this is bogus... */
		TIFFSetField(out, TIFFTAG_PHOTOMETRIC, h.ras_depth == 24 ?
		    PHOTOMETRIC_RGB : PHOTOMETRIC_MINISBLACK);
		TIFFSetField(out, TIFFTAG_COMPRESSION, compression);
	}

	linebytes = ((h.ras_depth*h.ras_width+15) >> 3) &~ 1;
	if (DEBUG)
		fprintf (stdout,"linebytes = %d, TIFFscanlinesize = %d\n", linebytes,
		    TIFFScanlineSize(out));
	if (TIFFScanlineSize(out) > linebytes)
		buf = (u_char *)malloc(linebytes);
	else
		buf = (u_char *)malloc(TIFFScanlineSize(out));
	if (rowsperstrip != (u_short)-1)
		rowsperstrip = (u_short)(8*1024/linebytes);
	if (DEBUG)
		fprintf (stdout,"rowsperstrip = %d\n",rowsperstrip);
	TIFFSetField(out, TIFFTAG_ROWSPERSTRIP,
	    rowsperstrip == 0 ? 1 : rowsperstrip);

	if (verbose)
		fprintf (stderr, "%s: complete ... ", G_program_name());
	for (row = 0; row < h.ras_height; row++)
	{
		tmpptr = buf;
		if (verbose)
			G_percent (row, h.ras_height, 2);
		if (G_get_map_row (in, cell, row) < 0)
			exit(1);
		cellptr = cell;
		if (pflag->answer){
			for ( col=0; col < h.ras_width; col++){
				*tmpptr++ = (u_char)*cellptr++;
			}
		} else{
			for ( col=0; col < h.ras_width; col++){
				G_get_color( cell[col], &red, &grn, &blu, &colors);
				*tmpptr++ = (u_char) red;
				*tmpptr++ = (u_char) grn;
				*tmpptr++ = (u_char) blu;
			}
		}
		if (TIFFWriteScanline(out, buf, row, 0) < 0)
			break;
	}
	if (verbose)
		G_percent (row, h.ras_height, 2);

	(void) TIFFClose(out);
	
	if (tflag->answer)
		write_tfw(outopt->answer, &cellhd, verbose);
	
	return 0;
}

int
write_tfw(char *fname, struct Cell_head *win, int verbose) {
	
	int  len, i, has_suf = 0;
	char *name, *ptr, suf[4][4] = {"tif", "tiff", "TIFF", "TIF"};
	FILE *outfile;
	
	if (verbose)
		fprintf(stderr, "Writing TIFF World file ...     ");

	if (fname == NULL)
		G_fatal_error("Got null file name");
	if (win == NULL)
		G_fatal_error("Got null region struct");
	
	len = strlen(fname) + 4 + 1;
	name = G_malloc(len);
	if (name == NULL)
		G_fatal_error("Out of Memory");
	
	strncpy(name, fname, len-4);
	
	for (i = 0; i < 4; i++)
		if ((ptr = strstr(name, suf[i])) != NULL) {
			if (*(ptr-1) == '.') {
				has_suf = 1;
				break;
			}
		}

	if (has_suf) {
		strncpy(ptr,"tfw\0", 4);
	}	
	else {
		ptr = strchr(name, '\0');
		strncpy(ptr,".tfw", 4);
	}

	outfile = fopen(name, "w");
	if (outfile == NULL)
		G_fatal_error("Couldn't open TIFF world file for writing");
	
	fprintf (outfile, "%.8f\n%.8f\n%.8f\n%.8f\n%.8f\n%.8f\n",
			win->ew_res, 0.0, 0.0, -1 * win->ns_res,
			win->west + win->ew_res / 2.0, 
			win->north - win->ns_res / 2.0);

	fclose(outfile);
	if (verbose)
		fprintf(stderr,"Done\n");
	free(name);

	return 0;
}
