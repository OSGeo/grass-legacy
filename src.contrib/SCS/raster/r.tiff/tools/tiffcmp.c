
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

static	int stoponfirstdiff = 1;
static	int stoponfirsttag = 1;
static	u_short bitspersample = 1;
static	u_short samplesperpixel = 1;
static	u_short imagewidth;
static	u_short imagelength;

static	int tiffcmp();
static	int cmptags();
static	void ContigCompare();
static	void PrintDiff();
static	void SeparateCompare();
static	int CheckShortTag();
static	void eof();

static void
usage()
{
	fprintf(stderr, "Usage: tiffcmp [-l] [-t] file1 file2\n");
	exit(-3);
}

main(argc, argv)
	char *argv[];
{
	TIFF *tif1, *tif2;
	int c, dirnum;
	extern int optind;

	while ((c = getopt(argc, argv, "lt")) != -1)
		switch (c) {
		case 'l':
			stoponfirstdiff = 0;
			break;
		case 't':
			stoponfirsttag = 0;
			break;
		case '?':
			usage();
			/*NOTREACHED*/
		}
	if (argc - optind < 2)
		usage();
	tif1 = TIFFOpen(argv[optind], "r");
	if (tif1 == NULL)
		exit(-1);
	tif2 = TIFFOpen(argv[optind+1], "r");
	if (tif2 == NULL)
		exit(-2);
	dirnum = 0;
	while (tiffcmp(tif1, tif2)) {
		if (!TIFFReadDirectory(tif1)) {
			if (!TIFFReadDirectory(tif2))
				break;
			printf("No more directories for %s\n", tif1->tif_name);
			exit(1);
		} else if (!TIFFReadDirectory(tif2)) {
			printf("No more directories for %s\n", tif2->tif_name);
			exit(1);
		}
		printf("Directory %d:\n", ++dirnum);
	}
	exit(0);
}

#define	EOF(name, row, sample) { \
	eof(name, row, sample); \
	goto bad; \
}

static int
tiffcmp(tif1, tif2)
	TIFF *tif1, *tif2;
{
	u_short config1, config2;
	int s, size1, size2, row;
	register u_char *p1, *p2;
	u_char *buf1, *buf2;

	if (!CheckShortTag(tif1, tif2, TIFFTAG_BITSPERSAMPLE, "BitsPerSample"))
		return (0);
	if (!CheckShortTag(tif1, tif2, TIFFTAG_SAMPLESPERPIXEL, "SamplesPerPixel"))
		return (0);
	if (!CheckShortTag(tif1, tif2, TIFFTAG_IMAGEWIDTH, "ImageWidth"))
		return (0);
	if (!cmptags(tif1, tif2))
		return (1);
	(void) TIFFGetField(tif1, TIFFTAG_BITSPERSAMPLE, &bitspersample);
	(void) TIFFGetField(tif1, TIFFTAG_SAMPLESPERPIXEL, &samplesperpixel);
	(void) TIFFGetField(tif1, TIFFTAG_IMAGEWIDTH, &imagewidth);
	(void) TIFFGetField(tif1, TIFFTAG_IMAGELENGTH, &imagelength);
	(void) TIFFGetField(tif1, TIFFTAG_PLANARCONFIG, &config1);
	(void) TIFFGetField(tif2, TIFFTAG_PLANARCONFIG, &config2);
	buf1 = (u_char *)malloc(size1 = TIFFScanlineSize(tif1));
	buf2 = (u_char *)malloc(size2 = TIFFScanlineSize(tif2));
	if (buf1 == NULL || buf2 == NULL) {
		fprintf(stderr, "No space for scanline buffers\n");
		exit(-1);
	}
	if (config1 != config2 && bitspersample != 8 && samplesperpixel > 1) {
		fprintf(stderr,
"Can't handle different planar configuration w/ different bits/sample\n");
		goto bad;
	}
#define	pack(a,b)	((a)<<8)|(b)
	switch (pack(config1, config2)) {
	case pack(PLANARCONFIG_SEPARATE, PLANARCONFIG_CONTIG):
		for (row = 0; row < imagelength; row++) {
			if (TIFFReadScanline(tif2, buf2, row, 0) < 0)
				EOF(tif2->tif_name, row, -1)
			for (s = 0; s < samplesperpixel; s++) {
				if (TIFFReadScanline(tif1, buf1, row, s) < 0)
					EOF(tif1->tif_name, row, s)
				SeparateCompare(1, s, row, buf2, buf1);
			}
		}
		break;
	case pack(PLANARCONFIG_CONTIG, PLANARCONFIG_SEPARATE):
		for (row = 0; row < imagelength; row++) {
			if (TIFFReadScanline(tif1, buf1, row, 0) < 0)
				EOF(tif1->tif_name, row, -1)
			for (s = 0; s < samplesperpixel; s++) {
				if (TIFFReadScanline(tif2, buf2, row, s) < 0)
					EOF(tif2->tif_name, row, s)
				SeparateCompare(0, s, row, buf1, buf2);
			}
		}
		break;
	case pack(PLANARCONFIG_SEPARATE, PLANARCONFIG_SEPARATE):
		for (s = 0; s < samplesperpixel; s++)
			for (row = 0; row < imagelength; row++) {
				if (TIFFReadScanline(tif1, buf1, row, s) < 0)
					EOF(tif1->tif_name, row, s)
				if (TIFFReadScanline(tif2, buf2, row, s) < 0)
					EOF(tif2->tif_name, row, s)
				ContigCompare(s, row, buf1, buf2, size1);
			}
		break;
	case pack(PLANARCONFIG_CONTIG, PLANARCONFIG_CONTIG):
		for (row = 0; row < imagelength; row++) {
			if (TIFFReadScanline(tif1, buf1, row, 0) < 0)
				EOF(tif1->tif_name, row, -1)
			if (TIFFReadScanline(tif2, buf2, row, 0) < 0)
				EOF(tif2->tif_name, row, -1)
			ContigCompare(-1, row, buf1, buf2, size1);
		}
		break;
	}
done:
	if (buf1) free(buf1);
	if (buf2) free(buf2);
	return (1);
bad:
	if (stoponfirstdiff)
		exit(1);
	if (buf1) free(buf1);
	if (buf2) free(buf2);
	return (0);
}

#define	CmpShortField(tag, name) \
	if (!CheckShortTag(tif1, tif2, tag, name) && stoponfirsttag) return (0)
#define	CmpShortField2(tag, name) \
	if (!CheckShort2Tag(tif1, tif2, tag, name) && stoponfirsttag) return (0)
#define	CmpLongField(tag, name) \
	if (!CheckLongTag(tif1, tif2, tag, name) && stoponfirsttag) return (0)
#define	CmpFloatField(tag, name) \
	if (!CheckFloatTag(tif1, tif2, tag, name) && stoponfirsttag) return (0)
#define	CmpStringField(tag, name) \
	if (!CheckStringTag(tif1, tif2, tag, name) && stoponfirsttag) return (0)

static int
cmptags(tif1, tif2)
	TIFF *tif1, *tif2;
{
	CmpLongField(TIFFTAG_SUBFILETYPE,	"SubFileType");
	CmpShortField(TIFFTAG_IMAGEWIDTH,	"ImageWidth");
	CmpShortField(TIFFTAG_IMAGELENGTH,	"ImageLength");
	CmpShortField(TIFFTAG_BITSPERSAMPLE,	"BitsPerSample");
	CmpShortField(TIFFTAG_COMPRESSION,	"Compression");
	CmpShortField(TIFFTAG_PREDICTOR,	"Predictor");
	CmpShortField(TIFFTAG_PHOTOMETRIC,	"PhotometricInterpretation");
	CmpShortField(TIFFTAG_THRESHHOLDING,	"Thresholding");
	CmpShortField(TIFFTAG_FILLORDER,	"FillOrder");
	CmpShortField(TIFFTAG_ORIENTATION,	"Orientation");
	CmpShortField(TIFFTAG_SAMPLESPERPIXEL,	"SamplesPerPixel");
	CmpShortField(TIFFTAG_MINSAMPLEVALUE,	"MinSampleValue");
	CmpShortField(TIFFTAG_MAXSAMPLEVALUE,	"MaxSampleValue");
	CmpFloatField(TIFFTAG_XRESOLUTION,	"XResolution");
	CmpFloatField(TIFFTAG_YRESOLUTION,	"YResolution");
	CmpLongField(TIFFTAG_GROUP3OPTIONS,	"Group3Options");
	CmpLongField(TIFFTAG_GROUP4OPTIONS,	"Group4Options");
	CmpShortField(TIFFTAG_RESOLUTIONUNIT,	"ResolutionUnit");
	CmpShortField(TIFFTAG_PLANARCONFIG,	"PlanarConfiguration");
	CmpLongField(TIFFTAG_ROWSPERSTRIP,	"RowsPerStrip");
	CmpFloatField(TIFFTAG_XPOSITION,	"XPosition");
	CmpFloatField(TIFFTAG_YPOSITION,	"YPosition");
	CmpShortField(TIFFTAG_GRAYRESPONSEUNIT, "GrayResponseUnit");
	CmpShortField(TIFFTAG_COLORRESPONSEUNIT, "ColorResponseUnit");
#ifdef notdef
	{ u_short *graycurve;
	  CmpField(TIFFTAG_GRAYRESPONSECURVE, graycurve);
	}
	{ u_short *red, *green, *blue;
	  CmpField3(TIFFTAG_COLORRESPONSECURVE, red, green, blue);
	}
	{ u_short *red, *green, *blue;
	  CmpField3(TIFFTAG_COLORMAP, red, green, blue);
	}
#endif
	CmpShortField2(TIFFTAG_PAGENUMBER,	"PageNumber");
	CmpStringField(TIFFTAG_ARTIST,		"Artist");
	CmpStringField(TIFFTAG_IMAGEDESCRIPTION,"ImageDescription");
	CmpStringField(TIFFTAG_MAKE,		"Make");
	CmpStringField(TIFFTAG_MODEL,		"Model");
	CmpStringField(TIFFTAG_SOFTWARE,	"Software");
	CmpStringField(TIFFTAG_DATETIME,	"DateTime");
	CmpStringField(TIFFTAG_HOSTCOMPUTER,	"HostComputer");
	CmpStringField(TIFFTAG_PAGENAME,	"PageName");
	CmpStringField(TIFFTAG_DOCUMENTNAME,	"DocumentName");
	CmpShortField(TIFFTAG_MATTEING,		"Matteing");
	return (1);
}

static void
ContigCompare(sample, row, p1, p2, size)
	int sample, row;
	u_char *p1, *p2;
	int size;
{
	register int pix, ppb = 8/bitspersample;

	if (bcmp(p1, p2, size) == 0)
		return;
	switch (bitspersample) {
	case 1: case 2: case 4: case 8: {
		register u_char *pix1 = p1, *pix2 = p2;

		for (pix = 0; pix < imagewidth; pix1++, pix2++, pix += ppb)
			if (*pix1 != *pix2)
				PrintDiff(row, sample, pix,
				    *pix1, *pix2);
		break;
	}
	case 16: {
		register u_short *pix1 = (u_short *)p1, *pix2 = (u_short *)p2;

		for (pix = 0; pix < imagewidth; pix1++, pix2++, pix++)
			if (*pix1 != *pix2)
				PrintDiff(row, sample, pix,
				    *pix1, *pix2);
		break;
	}
	}
}

static void
PrintDiff(row, sample, pix, w1, w2)
	register int w1, w2;
{
	register int mask, s;

	switch (bitspersample) {
	case 1:
		mask = 1<<8;
		for (s = 0; s < 8; mask >>= 1, s++)
			if ((w1 & mask) ^ (w2 & mask)) {
				w1 = (w1 >> s) & 1;
				w2 = (w2 >> s) & 1;
				break;
			}
		break;
	case 2:
		mask = 3<<6;
		for (s = 0; s < 4; mask >>= 2, s++)
			if ((w1 & mask) ^ (w2 & mask)) {
				w1 = (w1 >> 2*s) & 3;
				w2 = (w2 >> 2*s) & 3;
				break;
			}
		break;
	case 4:
		if ((w1 & 0xf0) ^ (w2 & 0xf0)) {
			s = 0;
			w1 >>= 4;
			w2 >>= 4;
		} else {
			s = 1;
			w1 &= 0xf;
			w2 &= 0xf;
		}
		break;
	case 8:
		s = 0;
	}
	if (sample < 0)
		sample = s;
	printf("Scanline %d, pixel %d, sample %d: %02x %02x\n",
	    row, pix, sample, w1, w2);
	if (stoponfirstdiff)
		exit(1);
}

static void
SeparateCompare(reversed, sample, row, cp1, p2)
	int reversed, sample, row;
	register u_char *cp1, *p2;
{
	int npixels = imagewidth;
	register int pixel;

	cp1 += sample;
	for (pixel = 0; npixels-- > 0; pixel++, cp1 += samplesperpixel, p2++)
		if (*cp1 != *p2) {
			printf("Scanline %d, pixel %d, sample %d: ",
			    row, pixel, sample);
			if (reversed)
				printf("%02x %02x\n", *p2, *cp1);
			else
				printf("%02x %02x\n", *cp1, *p2);
			if (stoponfirstdiff)
				exit(1);
		}
}

static int
checkTag(tif1, tif2, tag, name, p1, p2)
	TIFF *tif1, *tif2;
	char *name;
	char *p1, *p2;		/* XXX should be void * */
{

	if (TIFFGetField(tif1, tag, p1)) {
		if (!TIFFGetField(tif2, tag, p2)) {
			printf("%s tag appears only in %s\n",
			    name, tif1->tif_name);
			return (0);
		}
		return (1);
	} else if (TIFFGetField(tif2, tag, p2)) {
		printf("%s tag appears only in %s\n", name, tif2->tif_name);
		return (0);
	}
	return (-1);
}

#define	CHECK(cmp, fmt)					\
	switch (checkTag(tif1,tif2,tag,name,&v1,&v2)) {	\
	case 1:	if (cmp)				\
	case -1:	return (1);			\
		printf(fmt, name, v1, v2);		\
	case 0:	return (0);				\
	}

static int
CheckShortTag(tif1, tif2, tag, name)
	TIFF *tif1, *tif2;
	int tag;
	char *name;
{
	u_short v1, v2;
	CHECK(v1 == v2, "%s: %u %u\n");
}

static int
CheckShort2Tag(tif1, tif2, tag, name)
	TIFF *tif1, *tif2;
	int tag;
	char *name;
{
	u_short v11, v12, v21, v22;

	if (TIFFGetField(tif1, tag, &v11, &v12)) {
		if (!TIFFGetField(tif2, tag, &v21, &v22)) {
			printf("%s tag appears only in %s\n",
			    name, tif1->tif_name);
			return (0);
		}
		if (v11 == v21 && v12 == v22)
			return (1);
		printf("%s: <%u,%u> <%u,%u>\n", name, v11, v12, v21, v22);
	} else if (TIFFGetField(tif2, tag, &v21, &v22))
		printf("%s tag appears only in %s\n", name, tif2->tif_name);
	else
		return (1);
	return (0);
}

static int
CheckLongTag(tif1, tif2, tag, name)
	TIFF *tif1, *tif2;
	int tag;
	char *name;
{
	u_long v1, v2;
	CHECK(v1 == v2, "%s: %lu %lu\n");
}

static int
CheckFloatTag(tif1, tif2, tag, name)
	TIFF *tif1, *tif2;
	int tag;
	char *name;
{
	float v1, v2;
	CHECK(v1 == v2, "%s: %g %g\n");
}

static int
CheckStringTag(tif1, tif2, tag, name)
	TIFF *tif1, *tif2;
	int tag;
	char *name;
{
	char *v1, *v2;
	CHECK(strcmp(v1, v2) == 0, "%s: \"%s\" \"%s\"\n");
}

static void
eof(name, row, s)
	char *name;
	int row, s;
{

	printf("%s: EOF at scanline %d", name, row);
	if (s >= 0)
		printf(", sample %d", s);
	printf("\n");
}
