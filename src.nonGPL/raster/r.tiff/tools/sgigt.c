
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
#include <gl.h>
#include <device.h>
#include "tiffio.h"

/* XXX fudge adjustment for window borders */
#define	YFUDGE	20
#define	XFUDGE	20

Cursor	hourglass = {
    0x1ff0, 0x1ff0, 0x0820, 0x0820,
    0x0820, 0x0c60, 0x06c0, 0x0100,
    0x0100, 0x06c0, 0x0c60, 0x0820,
    0x0820, 0x0820, 0x1ff0, 0x1ff0
};
u_long	*raster;			/* displayable image */
u_short	width, height;			/* image width & height */
u_short	bitspersample;
u_short	samplesperpixel;
u_short	photometric;
u_short	orientation;
u_short	*redcmap, *greencmap, *bluecmap;/* colormap for pallete images */

static void
usage()
{
	fprintf(stderr, "usage: tiffgt [-d dirnum] [-f] filename\n");
	exit(-1);
}

main(argc, argv)
	char *argv[];
{
	char *cp, *filename, *rindex();
	long max;
	TIFF *tif;
	int fg = 0, c, dirnum = -1;
	extern int optind;
	extern char *optarg;

	while ((c = getopt(argc, argv, "d:f")) != -1)
		switch (c) {
		case 'd':
			dirnum = atoi(optarg);
			break;
		case 'f':
			fg = 1;
			break;
		case '?':
			usage();
			/*NOTREACHED*/
		}
	if (argc - optind < 1)
		usage();
	filename = argv[optind];
	tif = TIFFOpen(filename, "r");
	if (tif == NULL)
		exit(-1);
	if (dirnum != -1 && !TIFFSetDirectory(tif, dirnum)) {
		fprintf(stderr, "%s: Error, seeking to directory %d.\n",
		    filename, dirnum);
		exit(-1);
	}
	if (!TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bitspersample))
		bitspersample = 1;
	switch (bitspersample) {
	case 1: case 2: case 4:
	case 8: case 16:
		break;
	default:
		fprintf(stderr, "Sorry, can't handle %d-bit pictures\n",
		    bitspersample);
		exit(-1);
	}
	if (!TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samplesperpixel))
		samplesperpixel = 1;
	switch (samplesperpixel) {
	case 1: case 3: case 4:
		break;
	default:
	        fprintf(stderr, "Sorry, can't handle %d-channel images\n",
		    samplesperpixel);
		exit(-1);
	}
	TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &width);
	max = getgdesc(GD_XPMAX) - XFUDGE;
	if (width > max)
		width = max;
	TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &height);
	max = getgdesc(GD_YPMAX) - YFUDGE;
	if (height > max)
		height = max;
	prefsize(width, height);
	cp = rindex(filename, '/');
	if (cp == NULL)
		cp = filename;
	else
		cp++;
	if (fg)
		foreground();
	if (winopen(cp) < 0) {
		fprintf(stderr, "Can't create window.\n");
		exit(-1);
	}
	raster = (u_long *)malloc(width * height * sizeof (long));
	if (raster == 0) {
		fprintf(stderr, "No space for raster buffer\n");
		exit(-1);
	}
	singlebuffer();
	pseudorgb();
	curstype(C16X1);
	defcursor(1, hourglass);
	setcursor(1, 0, 0);
	rgb(0.5,0.5,0.5);
	clear();
	if (!gt(tif, width, height, raster))
		exit(-1);
	setcursor(0, 0, 0);
	TIFFClose(tif);
	qdevice(WINCLOSE);
	qdevice(WINQUIT);
	for (;;) {
		short val;
		switch (qread(&val)) {
		case REDRAW:
			lrectwrite(0, 0, width-1, height-1, raster);
			break;
		case WINCLOSE:
		case WINQUIT:
			exit(0);
		}
	}
	/*NOTREACHED*/
}

static int
checkcmap(n, r, g, b)
	int n;
	u_short *r, *g, *b;
{
	while (n-- > 0)
	    if (*r++ >= 256 || *g++ >= 256 || *b++ >= 256)
		return (16);
	fprintf(stderr, "Warning, assuming 8-bit colormap.\n");
	return (8);
}

#define	howmany(x, y)	(((x)+((y)-1))/(y))
RGBvalue **BWmap;

static
gt(tif, w, h, raster)
	TIFF *tif;
	int w, h;
	u_long *raster;
{
	u_short minsamplevalue, maxsamplevalue, planarconfig;
	RGBvalue *Map;
	int e;

	if (!TIFFGetField(tif, TIFFTAG_PHOTOMETRIC, &photometric)) {
		switch (samplesperpixel) {
		case 1:
			photometric = PHOTOMETRIC_MINISBLACK;
			break;
		case 3: case 4:
			photometric = PHOTOMETRIC_RGB;
			break;
		default:
			fprintf(stderr, "Missing needed \"%s\" tag.\n",
			    "PhotometricInterpretation");
			return (0);
		}
		fprintf(stderr,
		    "No \"PhotometricInterpretation\" tag, assuming %s.\n",
		    photometric == PHOTOMETRIC_RGB ? "RGB" : "min-is-black");
	}
	if (!TIFFGetField(tif, TIFFTAG_MINSAMPLEVALUE, &minsamplevalue))
		minsamplevalue = 0;
	if (!TIFFGetField(tif, TIFFTAG_MAXSAMPLEVALUE, &maxsamplevalue))
		maxsamplevalue = (1<<bitspersample)-1;
	Map = NULL;
	switch (photometric) {
	case PHOTOMETRIC_RGB:
		if (minsamplevalue == 0 && maxsamplevalue == 255)
			break;
		/* fall thru... */
	case PHOTOMETRIC_MINISBLACK:
	case PHOTOMETRIC_MINISWHITE: {
		register int x, range;

		range = maxsamplevalue - minsamplevalue;
		Map = (RGBvalue *)malloc((range + 1) * sizeof (RGBvalue));
		if (Map == NULL) {
			fprintf(stderr, "%s.\n",
			    "No space for photometric conversion table");
			return (0);
		}
		if (photometric == PHOTOMETRIC_MINISWHITE) {
			for (x = 0; x <= range; x++)
				Map[x] = ((range - x) * 255) / range;
		} else {
			for (x = 0; x <= range; x++)
				Map[x] = (x * 255) / range;
		}
		if (bitspersample != 8 && photometric != PHOTOMETRIC_RGB) {
			if (!makebwmap(Map))
				return (0);
			/* no longer need Map, free it */
			free((char *)Map);
			Map = NULL;
		}
		break;
	}
	case PHOTOMETRIC_PALETTE:
		if (!TIFFGetField(tif, TIFFTAG_COLORMAP,
		    &redcmap, &greencmap, &bluecmap)) {
			fprintf(stderr, "Missing required \"Colormap\" tag.\n");
			return (0);
		}
		/*
		 * Convert 16-bit colormap to 8-bit (unless it looks
		 * like an old-style 8-bit colormap).
		 */
		if (checkcmap(1<<bitspersample, redcmap, greencmap, bluecmap) == 16) {
			int i;
			for (i = (1<<bitspersample)-1; i > 0; i--) {
#define	CVT(x)		(((x) * 256) / ((1L<<16)-1))
				redcmap[i] = CVT(redcmap[i]);
				greencmap[i] = CVT(greencmap[i]);
				bluecmap[i] = CVT(bluecmap[i]);
			}
		}
		break;
	}
	TIFFGetField(tif, TIFFTAG_PLANARCONFIG, &planarconfig);
	if (planarconfig == PLANARCONFIG_SEPARATE)
		e = gtseparate(tif, raster, Map, h, w);
	else
		e = gtcontig(tif, raster, Map, h, w);
	if (Map)
		free((char *)Map);
	return (e);
}

setorientation(tif, h)
	TIFF *tif;
	int h;
{
	int y;

	if (!TIFFGetField(tif, TIFFTAG_ORIENTATION, &orientation))
		orientation = ORIENTATION_TOPLEFT;
	switch (orientation) {
	case ORIENTATION_BOTRIGHT:
	case ORIENTATION_RIGHTBOT:	/* XXX */
	case ORIENTATION_LEFTBOT:	/* XXX */
		fprintf(stderr, "Warning, using bottom-left orientation.\n");
		orientation = ORIENTATION_BOTLEFT;
		/* fall thru... */
	case ORIENTATION_BOTLEFT:
		y = 0;
		break;
	case ORIENTATION_TOPRIGHT:
	case ORIENTATION_RIGHTTOP:	/* XXX */
	case ORIENTATION_LEFTTOP:	/* XXX */
	default:
		fprintf(stderr, "Warning, using top-left orientation.\n");
		orientation = ORIENTATION_TOPLEFT;
		/* fall thru... */
	case ORIENTATION_TOPLEFT:
		y = h-1;
		break;
	}
	return (y);
}

gtcontig(tif, raster, Map, h, w)
	TIFF *tif;
	u_long *raster;
	register RGBvalue *Map;
	int h, w;
{
	register u_char *pp;
	register u_long *cp;
	register int x;
	int scanline, row, y;
	u_char *buf;

	buf = (u_char *)malloc(TIFFScanlineSize(tif));
	if (buf == 0) {
		fprintf(stderr, "No space for scanline buffer\n");
		return (0);
	}
	y = setorientation(tif, h);
	for (row = 0; row < h; row++) {
		if (TIFFReadScanline(tif, buf, row, 0) < 0)
			break;
		pp = buf;
		cp = raster + y*w;
		switch (photometric) {
		case PHOTOMETRIC_RGB:
			switch (bitspersample) {
			case 8:
				if (Map) {
					for (x = w; x-- > 0;) {
						pp[0] = Map[pp[0]];
						pp[1] = Map[pp[1]];
						pp[2] = Map[pp[2]];
						pp += samplesperpixel;
					}
					pp = buf;
				}
				for (x = w; x-- > 0;) {
					*cp++ = rgbi(pp[0], pp[1], pp[2]);
					pp += samplesperpixel;
				}
				break;
			case 16: {
				register u_short *wp;

				if (Map) {
					wp = (u_short *)pp;
					for (x = w; x-- > 0;) {
						wp[0] = Map[wp[0]];
						wp[1] = Map[wp[1]];
						wp[2] = Map[wp[2]];
						wp += samplesperpixel;
					}
				}
				wp = (u_short *)pp;
				for (x = w; x-- > 0;) {
					*cp++ = rgbi(wp[0], wp[1], wp[2]);
					wp += samplesperpixel;
				}
				break;
			}
			}
			break;
		case PHOTOMETRIC_PALETTE:
			for (x = w; x-- > 0;) {
				RGBvalue c = *pp++;
				*cp++ = rgbi(redcmap[c],
				    greencmap[c], bluecmap[c]);
			}
			break;
		case PHOTOMETRIC_MINISWHITE:
		case PHOTOMETRIC_MINISBLACK:
			if (bitspersample == 8) {
				register RGBvalue c;

				for (x = w; x-- > 0;) {
					c = Map[*pp++];
					*cp++ = rgbi(c, c, c);
				}
			} else
				gtbw(bitspersample, w, cp, pp);
			break;
		}
		lrectwrite(0, y, w-1, y+1, raster+y*w);
		y += (orientation == ORIENTATION_TOPLEFT ? -1 : 1);
	}
	return (1);
}

gtseparate(tif, raster, Map, h, w)
	TIFF *tif;
	u_long *raster;
	register RGBvalue *Map;
	int h, w;
{
	register u_long *cp;
	register int x;
	u_char *red;
	int scanline, row, y;

	scanline = TIFFScanlineSize(tif);
	switch (samplesperpixel) {
	case 1:
		red = (u_char *)malloc(scanline);
		break;
	case 3: case 4:
		red = (u_char *)malloc(3*scanline);
		break;
	}
	y = setorientation(tif, h);
	for (row = 0; row < h; row++) {
		cp = raster + y*w;
		if (TIFFReadScanline(tif, red, row, 0) < 0)
			break;
		switch (photometric) {
		case PHOTOMETRIC_RGB: {
			register u_char *r, *g, *b;

			r = red;
			if (TIFFReadScanline(tif, g = r + scanline, row, 1) < 0)
				break;
			if (TIFFReadScanline(tif, b = g + scanline, row, 2) < 0)
				break;
			switch (bitspersample) {
			case 8:
				for (x = w; x-- > 0;)
					*cp++ = rgbi(*r++, *g++, *b++);
				break;
			case 16:
#define	wp(x)	((u_short *)(x))
				for (x = 0; x < w; x++)
					*cp++ = rgbi(
					    Map[wp(r)[x]],
					    Map[wp(g)[x]],
					    Map[wp(b)[x]]);
				break;
#undef	wp
			}
			break;
		}
		case PHOTOMETRIC_PALETTE: {
			register u_char *pp = red;
			for (x = w; x-- > 0;) {
				RGBvalue c = *pp++;
				*cp++ = rgbi(redcmap[c],
				    greencmap[c], bluecmap[c]);
			}
			break;
		}
		case PHOTOMETRIC_MINISWHITE:
		case PHOTOMETRIC_MINISBLACK:
			if (bitspersample == 8) {
				register u_short *pp = (u_short *)red;
				register RGBvalue c;

				for (x = w; x-- > 0;) {
					c = Map[*pp++];
					*cp++ = rgbi(c, c, c);
				}
			} else
				gtbw(bitspersample, w, cp, red);
			break;
		}
		lrectwrite(0, y, w-1, y+1, raster+y*w);
		y += (orientation == ORIENTATION_TOPLEFT ? -1 : 1);
	}
	if (red)
		free(red);
	return (1);
}

/*
 * Greyscale images with less than 8 bits/sample are handled
 * with a table to avoid lots of shits and masks.  The table
 * is setup so that gtbw (below) can retrieve 8/bitspersample
 * pixel values simply by indexing into the table with one
 * number.
 */
makebwmap(Map)
	RGBvalue *Map;
{
	register int i;
	int nsamples = 8 / bitspersample;
	register RGBvalue *p;

	BWmap = (RGBvalue **)malloc(
	    256*sizeof (RGBvalue *)+(256*nsamples*sizeof(RGBvalue)));
	if (BWmap == NULL) {
		fprintf(stderr, "No space for B&W mapping table.\n");
		return (0);
	}
	p = (RGBvalue *)(BWmap + 256);
	for (i = 0; i < 256; i++) {
		BWmap[i] = p;
		switch (bitspersample) {
		case 1:
			*p++ = Map[i>>7];
			*p++ = Map[(i>>6)&1];
			*p++ = Map[(i>>5)&1];
			*p++ = Map[(i>>4)&1];
			*p++ = Map[(i>>3)&1];
			*p++ = Map[(i>>2)&1];
			*p++ = Map[(i>>1)&1];
			*p++ = Map[i&1];
			break;
		case 2:
			*p++ = Map[i>>6];
			*p++ = Map[(i>>4)&3];
			*p++ = Map[(i>>2)&3];
			*p++ = Map[i&3];
			break;
		case 4:
			*p++ = Map[i>>4];
			*p++ = Map[i&0xf];
			break;
		}
	}
	return (1);
}

#define	REPEAT8(op)	REPEAT4(op); REPEAT4(op)
#define	REPEAT4(op)	REPEAT2(op); REPEAT2(op)
#define	REPEAT2(op)	op; op

gtbw(bitspersample, w, cp, pp)
	int bitspersample, w;
	register u_long *cp;
	register u_char *pp;
{
	register RGBvalue c, *bw;
	register int x;

	switch (bitspersample) {
	case 1:
		for (x = w; x >= 8; x -= 8) {
			bw = BWmap[*pp++];
			REPEAT8(c = *bw++; *cp++ = rgbi(c, c, c));
		}
		break;
	case 2:
		for (x = w; x >= 4; x -= 4) {
			bw = BWmap[*pp++];
			REPEAT4(c = *bw++; *cp++ = rgbi(c, c, c));
		}
		break;
	case 4:
		for (x = w; x >= 2; x -= 2) {
			bw = BWmap[*pp++];
			REPEAT2(c = *bw++; *cp++ = rgbi(c, c, c));
		}
		break;
	}
	bw = BWmap[*pp++];
	for (; x > 0; x--) {
		c = *bw++;
		*cp++ = rgbi(c, c, c);
	}
}
