/* 
 * $Id$
 *
 * Added support for Tiled TIFF output ( -l switch )
 * Luca Cristelli (luca.cristelli@ies.it) 1/2001
 * 
 * Added flag to write a TIFF World file like r.out.arctiff
 * Eric G. Miller 4-Nov-2000
 *
 * removed LZW support 5/5000
 *
 * Corrected G_set_window to G_get_window to make r.out.tiff sensitive
 * to region settings.   - Markus Neteler  (neteler@geog.uni-hannover.de
 * 8/98        
 *
 * This r.tiff version uses the standard libtiff from your system.
 *  8. June 98 Marco Valagussa <marco@duffy.crcc.it>
 *
 * Original version:
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
#include <float.h>
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
	int in, len;
	struct rasterfile h;
	struct Option *inopt, *outopt, *compopt;
	struct Flag *pflag, *vflag, *lflag, *tflag;
	CELL *cell, *cellptr;
	struct Cell_head cellhd;
	int col,verbose, tfw, palette, tiled;
	char *mapset, *filename;
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

	lflag = G_define_flag();
	lflag->key		= 'l';
	lflag->description      = "Output Tiled TIFF";

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
	tiled = lflag->answer;
	palette = pflag->answer;
	tfw = tflag->answer;

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
	G_set_null_value_color (255, 255, 255, &colors);
	if (palette && (colors.cmax - colors.cmin > 255))
		G_fatal_error ("Color map for palette must have less than 256 "\
			"colors for the available range of data");
	cell = G_allocate_cell_buf();
	if((in = G_open_cell_old (inopt->answer, mapset)) < 0){
		fprintf(stderr, "%s - can't open raster map\n", inopt->answer);
		exit(1);
	}
	len = strlen (inopt->answer) + 5 ;
	if ((filename = G_malloc (len)) == NULL)
		G_fatal_error ("Memory Allocation failed.");
	sprintf (filename, "%s.tif", outopt->answer);
	out = TIFFOpen(filename, "w");
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

	if (palette) {
		register u_short *redp, *grnp, *blup, *mapptr;
		register int i;

		if (DEBUG)
			fprintf (stdout,"max %f min %f mapsize %d\n ",colors.cmax, colors.cmin,mapsize);
		mapptr = (u_short *)G_calloc(mapsize* 3, sizeof (u_short));
		redp = mapptr;
		grnp = redp + mapsize;
		blup = redp + mapsize * 2;

		/* XXX -- set pointers up before we step through arrays */
#define	SCALE(x)	(((x)*((1L<<16)-1))/255)
		for (i = colors.cmin; i <= colors.cmax;i++,redp++,grnp++,blup++){
			G_get_color(i, &red, &grn, &blu, &colors);
			*redp = (u_short)(SCALE(red));
			*grnp = (u_short)(SCALE(grn));
			*blup = (u_short)(SCALE(blu));
			if (DEBUG)
			    fprintf (stderr," %d : %d %d %d   %d %d %d\n", 
					    i,red,grn,blu, *redp, *grnp, *blup);
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
	
	if (tiled) {
	    int tilewidth = 128;
	    int tilelength = 128;
	    int imagewidth, imagelength;
	    int spp;
	    char *obuf, *bufp;
	    char *tptr;
	    
	    imagewidth = h.ras_width;
	    imagelength = h.ras_height;
	    spp = h.ras_depth;
	    
	    TIFFSetField(out, TIFFTAG_TILEWIDTH, tilewidth);
	    TIFFSetField(out, TIFFTAG_TILELENGTH, tilelength);
	    obuf = (char*)_TIFFmalloc(TIFFTileSize(out));
	    if (DEBUG)
		fprintf (stderr, "Tile buff size: %d \n ",TIFFTileSize(out) );
	    
	    /* build tiff tiles from grass buffer */
	    
            for (row = 0; row < imagelength; row += tilelength) {
                    uint32 nrow = (row+tilelength > imagelength) ? imagelength-row : tilelength;
                    uint32 colb = 0;
                    uint32 col, oskew, width, i, j;

                    for (col = 0; col < imagewidth; col += tilewidth) {
                            tsample_t s;
			    
			    i = nrow;
			    tptr = obuf;
			    spp = 1;
			    s = 0;
			    oskew = 0;
			    width = tilewidth;
			    
			    if (DEBUG)
				fprintf (stderr, "Tile #: r %d, c %d, s %d \n ", row, col, s);
                            
			    /*
                             * Tile is clipped horizontally.  Calculate
	                     * visible portion and skewing factors.
                             */
                            if (colb + tilewidth > imagewidth) {
                                   width = (imagewidth - colb);
                                   oskew = tilewidth - width;
                            }
			    
			    for (i = 0; i < nrow; i++ ) {
			        tptr += oskew;
 			    	
				if (G_get_c_raster_row (in, cell, row + i) < 0)
					exit(1);
				cellptr = cell;
				
				if (palette) {
				    cellptr += col;
				    for (j = 0; j < width; j++ ) {
					*tptr++ = (u_char)*cellptr++;
    				    }
				    tptr += oskew;
				} else {
				    for (j = 0; j < width; j++ ) {
					G_get_color( cell[col + j], &red, &grn, &blu, &colors);
					*tptr++ = (u_char) red;
					*tptr++ = (u_char) grn;
					*tptr++ = (u_char) blu;
		       	    	    }
				    tptr += oskew*3;
				}
				if (DEBUG)
				    fprintf (stderr, "\r row #: i %d tptr %d \n ", i, tptr);
					
			    }
	    
 			    if (DEBUG)
				fprintf (stderr, "\n Write Tile #: col %d row %d s %d \n ", col, row, s );
                    	    if (TIFFWriteTile(out, obuf, col, row, 0, s) < 0) {
                        	_TIFFfree(obuf);
                                return (-1);
                            }
	 		    
			    if (verbose)
				G_percent (row, h.ras_height, 2);
			    }
			    colb += tilewidth;
		    }
		    //bufp += nrow * imagewidth;
		

	} else {

	
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
			if (G_get_c_raster_row (in, cell, row) < 0)
				exit(1);
			cellptr = cell;
			if (palette){
				for ( col=0; col < h.ras_width; col++){
					*tmpptr++ = (u_char)(*cellptr++ - colors.cmin);
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
	}
	
	(void) TIFFClose(out);
	
	if (tfw)
	{
		sprintf (filename, "%s.tfw", outopt->answer);
		write_tfw(filename, &cellhd, verbose);
	}
	
	return 0;
}


int
write_tfw(char *fname, struct Cell_head *win, int verbose) {

	int  width = DBL_DIG;
	FILE *outfile;
	
	if (verbose)
		fprintf(stderr, "Writing TIFF World file ...     ");

	if (fname == NULL)
		G_fatal_error("Got null file name");
	if (win == NULL)
		G_fatal_error("Got null region struct");
	
	outfile = fopen(fname, "w");
	if (outfile == NULL)
		G_fatal_error("Couldn't open TIFF world file for writing");
	
	fprintf (outfile, "%36.*f \n", width, win->ew_res);
	fprintf (outfile, "%36.*f \n", width, 0.0);
	fprintf (outfile, "%36.*f \n", width, 0.0);
	fprintf (outfile, "%36.*f \n", width, -1 * win->ns_res);
	fprintf (outfile, "%36.*f \n", width, win->west + win->ew_res / 2.0 );
        fprintf (outfile, "%36.*f \n", width, win->north - win->ns_res / 2.0);

	fclose(outfile);
	if (verbose)
		fprintf(stderr,"Done\n");

	return 0;
}
