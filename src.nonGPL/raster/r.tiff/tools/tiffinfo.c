
/*
 * Copyright (c) 1988, 1990 by Sam Leffler.
 * All rights reserved.
 *
 * This file is provided for unrestricted use provided that this
 * legend is included on all tape media and as a part of the
 * software program in whole or part.  Users may copy, modify or
 * distribute this file at will.
 */

#include <stdio.h>
#include "tiffio.h"

int	showcolormap = 0;		/* show colormap entries if present */
int	showresponsecurves = 0;		/* show response curve[s] if present */
int	showstrips = 0;			/* show strip offsets and sizes */
int	showdata = 0;			/* show data */
int	readdata = 0;			/* read data in file */

usage(code)
	int code;
{
	fprintf(stderr, "Usage: tiffinfo [-dcs] [-#] TIFF-files\n");
	fprintf(stderr, "-D    read data\n");
	fprintf(stderr, "-d    show data\n");
	fprintf(stderr, "-c    show color/gray response curves and colormap\n");
	fprintf(stderr, "-s    show data strip offsets and byte counts\n");
	fprintf(stderr, "-#    show directory number #\n");
	exit(code);
}

main(argc, argv)
	char *argv[];
{
	int dirnum = -1, multiplefiles, c;
	char *cp;
	TIFF *tif;
	extern int optind;

	while ((c = getopt(argc, argv, "cdDs0123456789")) != -1)
		switch (c) {
		case '0': case '1': case '2': case '3':
		case '4': case '5': case '6': case '7':
		case '8': case '9':
			dirnum = atoi(argv[optind]);
			break;
		case 'd':
			showdata++;
			/* fall thru... */
		case 'D':
			readdata++;
			break;
		case 'c':
			showcolormap++;
			showresponsecurves++;
			break;
		case 's':
			showstrips++;
			break;
		case '?':
			usage(-1);
			/*NOTREACHED*/
		}
	if (optind >= argc)
		usage(-2);
	multiplefiles = (argc - optind > 1);
	for (; optind < argc; optind++) {
		if (multiplefiles)
			printf("%s:\n", argv[optind]);
		tif = TIFFOpen(argv[optind], "r");
		if (tif != NULL) {
			if (dirnum == -1) {
				do
					TIFFInfo(tif);
				while (TIFFReadDirectory(tif));
			} else {
				if (TIFFSetDirectory(tif, dirnum))
					TIFFInfo(tif);
			}
			TIFFClose(tif);
		}
	}
	exit(0);
}

static
Show(row, sample, pp, scanline)
	int row, sample;
	register u_char *pp;
	int scanline;
{
	register int cc;

	printf("[%3d", row);
	if (sample > 0)
		printf(",%d", sample);
	printf("]");
	for (cc = 0; cc < scanline; cc++) {
		printf(" %02x", *pp++);
		if (((cc+1) % 24) == 0)
			putchar('\n');
	}
	putchar('\n');
}

TIFFInfo(tif)
	TIFF *tif;
{
	u_char *scanbuf;
	u_short config, h;
	int s, row, scanline;

	TIFFPrintDirectory(tif, stdout,
	    showstrips, showcolormap, showresponsecurves);
	if (!readdata)
		return;
	TIFFGetField(tif, TIFFTAG_PLANARCONFIG, &config);
	scanbuf = (u_char *)malloc(scanline = TIFFScanlineSize(tif));
	if (scanbuf == NULL) {
		fprintf(stderr, "No space for scanline buffer\n");
		return;
	}
	TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &h);
	if (config == PLANARCONFIG_CONTIG) {
		for (row = 0; row < h; row++) {
			if (TIFFReadScanline(tif, scanbuf, row, 0) < 0)
				break;
			if (showdata)
				Show(row, -1, scanbuf, scanline);
		}
	} else {
		u_short samplesperpixel;

		TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samplesperpixel);
		for (s = 0; s < samplesperpixel; s++)
		for (row = 0; row < h; row++) {
			if (TIFFReadScanline(tif, scanbuf, row, s) < 0)
				goto done;
			if (showdata)
				Show(row, s, scanbuf, scanline);
		}
	done:
		;
	}
	free((char *)scanbuf);
}
