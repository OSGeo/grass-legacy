
/*
 * Copyright (c) 1988, 1990 by Sam Leffler.
 * All rights reserved.
 *
 * This file is provided for unrestricted use provided that this
 * legend is included on all tape media and as a part of the
 * software program in whole or part.  Users may copy, modify or
 * distribute this file at will.
 */

#include <gl.h>
#include <stdio.h>
#include "tiffio.h"

#define	streq(a,b)	(strcmp(a,b) == 0)

char	rbuf[2048];
char	gbuf[2048];
char	bbuf[2048];
char	*scanline = NULL;

int	rowsperstrip = -1;
int	compression = COMPRESSION_LZW;
int	config = PLANARCONFIG_CONTIG;

main(argc, argv)
	int argc;
	char **argv;
{

	argc--, argv++;
	for (; argc > 0 && argv[0][0] == '-'; argc--, argv++) {
		if (streq(argv[0], "-none")) {
			compression = COMPRESSION_NONE;
			continue;
		}
		if (streq(argv[0], "-packbits")) {
			compression = COMPRESSION_PACKBITS;
			continue;
		}
		if (streq(argv[0], "-lzw")) {
			compression = COMPRESSION_LZW;
			continue;
		}
		if (streq(argv[0], "-contig")) {
			config = PLANARCONFIG_CONTIG;
			continue;
		}
		if (streq(argv[0], "-separate")) {
			config = PLANARCONFIG_SEPARATE;
			continue;
		}
		if (streq(argv[0], "-rowsperstrip")) {
			argc--, argv++;
			rowsperstrip = atoi(argv[0]);
			continue;
		}
		usage();
	}
	if (config == PLANARCONFIG_SEPARATE &&
	    compression != COMPRESSION_NONE && rowsperstrip > 1)
		rowsperstrip = 1;
	if (argc != 1 && argc != 5)
		usage();
	foreground();
	noport();
	winopen("tiffsv");
	if (argc == 5)
		tiffsv(argv[0],
		    atoi(argv[1]), atoi(argv[2]), atoi(argv[3]), atoi(argv[4]));
	else
		tiffsv(argv[0], 0, XMAXSCREEN, 0, YMAXSCREEN);
}

usage()
{

	fprintf(stderr, "usage: tiffsv [options] outimage [x1 x2 y1 y2]\n");
	fprintf(stderr, "where options are:\n");
	fprintf(stderr,
	    " -contig\tpack samples contiguously (e.g. RGBRGB...)\n");
	fprintf(stderr,
	    " -separate\tstore samples separately (e.g. RRR...GGG...BBB...)\n");
	fprintf(stderr, "\n");
	fprintf(stderr,
	    " -lzw\t\tcompress output with Lempel-Ziv & Welch encoding\n");
	fprintf(stderr,
	    " -packbits\tcompress output with packbits encoding\n");
	fprintf(stderr,
	    " -none\t\tuse no compression algorithm on output\n");
	fprintf(stderr, "\n");
	fprintf(stderr,
	    " -rowsperstrip #\tmake each strip have no more than # rows\n");
	exit(-1);
}

#define	MIN(a,b)	((a)<(b)?(a):(b))
#define	ABS(x)		((x)<0?-(x):(x))

tiffsv(name, x1, x2, y1, y2)
	char *name;
	int x1, x2, y1, y2;
{
	TIFF *tif;
	int xsize, ysize;
	int xorg, yorg;
	int temp, y, i;
	int pos, togo, n;

	xorg = MIN(x1,x2);
	yorg = MIN(y1,y2);
	if (xorg<0)
		xorg = 0;
	if (yorg<0)
		yorg = 0;
	xsize = ABS(x2-x1);
	ysize = ABS(y2-y1);
	if (xorg+xsize > XMAXSCREEN)
		xsize = XMAXSCREEN-xorg;
	if (yorg+ysize > YMAXSCREEN)
		ysize = YMAXSCREEN-yorg;
	tif = TIFFOpen(name, "w");
	TIFFSetField(tif, TIFFTAG_IMAGEWIDTH, xsize+1);
	TIFFSetField(tif, TIFFTAG_IMAGELENGTH, ysize+1);
	TIFFSetField(tif, TIFFTAG_BITSPERSAMPLE, 8);
	TIFFSetField(tif, TIFFTAG_SAMPLESPERPIXEL, 3);
	TIFFSetField(tif, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
	TIFFSetField(tif, TIFFTAG_PLANARCONFIG, config);
	TIFFSetField(tif, TIFFTAG_COMPRESSION, compression);
	if (rowsperstrip <= 0)
		rowsperstrip = (8*1024)/TIFFScanlineSize(tif);
	TIFFSetField(tif, TIFFTAG_ROWSPERSTRIP,
	    rowsperstrip == 0 ? 1 : rowsperstrip);
	if (config == PLANARCONFIG_CONTIG)
		scanline = (char *)malloc(TIFFScanlineSize(tif));
	wmplanes();
	screenspace();
	for (y = 0; y <= ysize; y++) {
#define READSCREENBROKEN
#ifdef READSCREENBROKEN
		for (togo = xsize+1, pos = 0; togo > 0; togo -= n, pos += n) {
			if ((n = togo) > 256)
				n = 256;
			cmov2i(xorg+pos,yorg+(ysize-y));
			gl_readscreen(n,rbuf+pos,gbuf+pos,bbuf+pos);
		}
#else
		cmov2i(xorg,yorg+(ysize-y));
		gl_readscreen(xsize,rbuf,gbuf,bbuf);
#endif
		if (config == PLANARCONFIG_SEPARATE) {
			if (TIFFWriteScanline(tif, rbuf, y, 0) < 0 ||
			    TIFFWriteScanline(tif, gbuf, y, 1) < 0 ||
			    TIFFWriteScanline(tif, bbuf, y, 2) < 0)
				break;
		} else {
			register char *pp = scanline;
			register int x;

			for (x = 0; x <= xsize; x++) {
				pp[0] = rbuf[x];
				pp[1] = gbuf[x];
				pp[2] = bbuf[x];
				pp += 3;
			}
			if (TIFFWriteScanline(tif, scanline, y) < 0)
				break;
		}
	}
	(void) TIFFClose(tif);
}
