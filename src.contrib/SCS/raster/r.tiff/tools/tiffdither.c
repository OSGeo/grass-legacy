
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

#define	CopyField(tag, v) \
	if (TIFFGetField(in, tag, &v)) TIFFSetField(out, tag, v)

unsigned short imagewidth;
unsigned short imagelength;
int	threshold = 128;

usage()
{
	fprintf(stderr,
	    "usage: tiffdither [-fop] [-t threshold] in.tif, out.tif.\n");
	exit(-1);
}

/* 
 * Floyd-Steinberg error propragation with threshold.
 * This code is stolen from tiffmedian.
 */
static
fsdither(in, out)
	TIFF *in, *out;
{
	u_char *outline, *inputline, *inptr;
	short *thisline, *nextline, *tmpptr;
	register u_char	*outptr;
	register short *thisptr, *nextptr;
	register int i, j, tmp;
	int imax, jmax, lastline, lastpixel;
	int bit, outlinesize;

	imax = imagelength - 1;
	jmax = imagewidth - 1;
	inputline = (u_char *)malloc(TIFFScanlineSize(in));
	thisline = (short *)malloc(imagewidth * sizeof (short));
	nextline = (short *)malloc(imagewidth * sizeof (short));
	outlinesize = TIFFScanlineSize(out);
	outline = (u_char *) malloc(outlinesize);

	/*
	 * Get first line
	 */
	if (TIFFReadScanline(in, inputline, 0, 0) <= 0)
		return;
	inptr = inputline;
	nextptr = nextline;
	for (j = 0; j < imagewidth; ++j)
		*nextptr++ = *inptr++;
	for (i = 1; i < imagelength; ++i) {
		tmpptr = thisline;
		thisline = nextline;
		nextline = tmpptr;
		lastline = (i == imax);
		if (TIFFReadScanline(in, inputline, i, 0) <= 0)
			break;
		inptr = inputline;
		nextptr = nextline;
		for (j = 0; j < imagewidth; ++j)
			*nextptr++ = *inptr++;
		thisptr = thisline;
		nextptr = nextline;
		bzero(outptr = outline, outlinesize);
		bit = 0x80;
		for (j = 0; j < imagewidth; ++j) {
			register int v;

			lastpixel = (j == jmax);
			v = *thisptr++;
			if (v < 0)
				v = 0;
			else if (v > 255)
				v = 255;
			if (v > threshold) {
				*outptr |= bit;
				v -= 255;
			}
			bit >>= 1;
			if (bit == 0) {
				outptr++;
				bit = 0x80;
			}
			if (!lastpixel)
				thisptr[0] += v * 7 / 16;
			if (!lastline) {
				if (j != 0)
					nextptr[-1] += v * 3 / 16;
				*nextptr++ += v * 5 / 16;
				if (!lastpixel)
					nextptr[0] += v / 16;
			}
		}
		if (TIFFWriteScanline(out, outline, i-1, 0) < 0)
			break;
	}
	free(inputline);
	free(thisline);
	free(nextline);
	free(outline);
}

main(argc, argv)
	int argc;
	char *argv[];
{
	TIFF *in, *out;
	unsigned short samplesperpixel, bitspersample = 1, shortv, config;
	char thing[1024];
	int rowsperstrip, c, compression = COMPRESSION_LZW;
	int onestrip = 0;
	int fillorder = FILLORDER_LSB2MSB;
	int group3options = GROUP3OPT_FILLBITS;
	extern int optind;
	extern char *optarg;

	while ((c = getopt(argc, argv, "t:fops")) != -1)
		switch (c) {
		case 't':
			threshold = atoi(optarg);
			if (threshold < 0)
				threshold = 0;
			else if (threshold > 255)
				threshold = 255;
			break;
		case 'f':
			compression = COMPRESSION_CCITTFAX3;
			onestrip = 1;
			break;
		case 'o':		/* reverse bit ordering in output */
			fillorder = FILLORDER_LSB2MSB;
			break;
		case 'p':		/* zero pad scanlines before EOL */
			group3options &= ~GROUP3OPT_FILLBITS;
			break;
		case '?':
			usage();
			/*NOTREACHED*/
		}
	if (argc - optind < 2)
		usage();
	in = TIFFOpen(argv[optind], "r");
	if (in == NULL)
		exit(-1);
	TIFFGetField(in, TIFFTAG_SAMPLESPERPIXEL, &samplesperpixel);
	if (samplesperpixel != 1) {
		fprintf(stderr, "%s: Not a b&w image.\n", argv[0]);
		exit(-1);
	}
	TIFFGetField(in, TIFFTAG_BITSPERSAMPLE, &bitspersample);
	if (bitspersample != 8) {
		fprintf(stderr,
		    " %s: Sorry, only handle 8-bit samples.\n", argv[0]);
		exit(-1);
	}
	out = TIFFOpen(argv[optind+1], "w");
	if (out == NULL)
		exit(-1);
	CopyField(TIFFTAG_IMAGEWIDTH, imagewidth);
	TIFFGetField(in, TIFFTAG_IMAGELENGTH, &imagelength);
	TIFFSetField(out, TIFFTAG_IMAGELENGTH, imagelength-1);
	TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, 1);
	TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, 1);
	TIFFSetField(out, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
	TIFFSetField(out, TIFFTAG_COMPRESSION, compression);
	TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_MINISBLACK);
	TIFFSetField(out, TIFFTAG_FILLORDER, fillorder);
	sprintf(thing, "Dithered B&W version of %s", argv[optind]);
	TIFFSetField(out, TIFFTAG_IMAGEDESCRIPTION, thing);
	CopyField(TIFFTAG_ORIENTATION, shortv);
	if (onestrip)
		rowsperstrip = imagelength-1;
	else
		rowsperstrip = (8*1024)/TIFFScanlineSize(out);
	TIFFSetField(out, TIFFTAG_ROWSPERSTRIP,
	    rowsperstrip == 0 ? 1 : rowsperstrip);
	if (compression == COMPRESSION_CCITTFAX3 && group3options)
		TIFFSetField(out, TIFFTAG_GROUP3OPTIONS, group3options);
	fsdither(in, out);
	TIFFClose(in);
	TIFFClose(out);
}
