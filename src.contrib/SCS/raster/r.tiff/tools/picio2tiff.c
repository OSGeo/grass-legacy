
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

#include <picio.h>

PFILE	*picio_pf;
typedef	struct {
	short	r, g, b, a;
} RGBAPixelType;
RGBAPixelType *picio_buffer;

#define	howmany(x, y)	(((x)+((y)-1))/(y))
#define	streq(a,b)	(strcmp(a,b) == 0)

main(argc, argv)
	char *argv[];
{
	register TIFF *out = NULL;
	short bitspersample = 0, samplesperpixel, shortv;
	short bytespersample;
	u_short compression = -1, row;
	long rowsperstrip = 128;
	register u_char *cp;
	register RGBAPixelType *p;
	register int n;
	register u_short *wp;
	u_char *buf;

	argc--, argv++;
	if (argc < 2)
		usage();
	for (; argc > 2 && argv[0][0] == '-'; argc--, argv++) {
		if (streq(argv[0], "-picio")) {
			compression = COMPRESSION_PICIO;
			continue;
		}
		if (streq(argv[0], "-none")) {
			compression = COMPRESSION_NONE;
			continue;
		}
		if (streq(argv[0], "-packbits")) {
			compression = COMPRESSION_PACKBITS;
			continue;
		}
		if (streq(argv[0], "-thunderscan")) {
			compression = COMPRESSION_THUNDERSCAN;
			continue;
		}
		if (streq(argv[0], "-rowsperstrip")) {
			argc--, argv++;
			rowsperstrip = atoi(argv[0]);
			continue;
		}
		if (streq(argv[0], "-8bit")) {
			bitspersample = 8;
			continue;
		}
		usage();
	}
	if ((picio_pf = PicOpen(argv[0], "r")) == NULL) {
		fprintf(stderr, "%s: can't open picture \"%s\".\n",
			"picio2tiff", argv[0]);
		exit(-1);
	}
	if (picio_pf->Pwidth != picio_pf->Twidth ||
	    picio_pf->Pheight != picio_pf->Theight) {
		/* can't handle tiled images yet */
		fprintf(stderr, "%s: can't handle tiled picture files.\n",
			"picio2tiff");
		PicClose(picio_pf);
		exit(-1);
	}
	switch (picio_pf->Pformat) {
	case PF_R:	/* one-channel image */
		samplesperpixel = 1;
		break;
	case PF_RGB:	/* three-channel image */
		samplesperpixel = 3;
		break;
	case PF_RGBA:	/* four-channel image */
		samplesperpixel = 4;
		break;
	default:
		fprintf(stderr, "Cannot handle that pixel format.\n");
		PicClose(picio_pf);
		exit(-1);
	}
	picio_buffer = (RGBAPixelType *)
	    malloc(picio_pf->Pwidth * sizeof(RGBAPixelType));
	if (picio_buffer == NULL) {
		fprintf(stderr, "can't allocate picio buffer.\n");
		PicClose(picio_pf);
		exit(-1);	/* error */
	}
	picPreDecodeScanline(picio_pf, 0L);
	out = TIFFOpen(argv[1], "w");
	if (out == NULL)
		exit(-2);
	TIFFSetField(out, TIFFTAG_IMAGEWIDTH, picio_pf->Pwidth);
	TIFFSetField(out, TIFFTAG_IMAGELENGTH, picio_pf->Pheight);
	if (bitspersample <= 0)
		bitspersample = (picio_pf->Pstorage == PS_8BIT ||
		    picio_pf->Pstorage == PS_8DUMP) ? 8 : 16;
	TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, bitspersample);
	if (bitspersample == 16) {
		/* XXX can't do this right XXX */
		TIFFSetField(out, TIFFTAG_MINSAMPLEVALUE, (short)0);
		TIFFSetField(out, TIFFTAG_MAXSAMPLEVALUE, (short)2047);
	}
	if (compression != (u_short)-1)
		TIFFSetField(out, TIFFTAG_COMPRESSION, compression);
	TIFFSetField(out, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
	TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, samplesperpixel);
	TIFFSetField(out, TIFFTAG_ROWSPERSTRIP, rowsperstrip);
	TIFFSetField(out, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
	bytespersample = bitspersample <= 8 ? 1 : bitspersample <= 16 ? 2 : 4;
	buf = (u_char *)malloc(
	    samplesperpixel * bytespersample * picio_pf->Pwidth);
	if (buf == NULL) {
		fprintf(stderr, "No space for scanline buffer\n");
		exit(-1);
	}
	TIFFSetField(out, TIFFTAG_XPOSITION, (float)picio_pf->Xoffset);
	TIFFSetField(out, TIFFTAG_YPOSITION, (float)picio_pf->Yoffset);
	/* XXX what about response curves? */
	TIFFSetField(out, TIFFTAG_MATTEING, picio_pf->Pformat == PF_RGBA);
	row = 0;
	while (picDecodeScanline(picio_pf, picio_buffer) != 0) {
		p = picio_buffer;
		if (bytespersample == 1) {
			cp = buf;
			for (n = picio_pf->Pwidth; n-- > 0; p++) {
				*cp++ = (p->r * 255) / 2048;
				if (samplesperpixel > 1) {
					*cp++ = (p->g * 255) / 2048;
					*cp++ = (p->b * 255) / 2048;
					if (samplesperpixel > 3)
						*cp++ = (p->a * 255) / 2048;
				}
			}
		} else {
			wp = (u_short *)buf;
			for (n = picio_pf->Pwidth; n-- > 0; p++) {
				*wp++ = p->r;
				if (samplesperpixel > 1) {
					*wp++ = p->g;
					*wp++ = p->b;
					if (samplesperpixel > 3)
						*wp++ = p->a;
				}
			}
		}
		if (TIFFWriteScanline(out, buf, row++, 0) < 0)
			break;
	}
	picPostDecodeScanline(picio_pf);
	free((char *)picio_buffer);
	PicClose(picio_pf);
	(void) TIFFClose(out);
}

usage()
{
	fprintf(stderr, "usage: picio2tiff [options] input output\n");
	fprintf(stderr, "where options are:\n");
	fprintf(stderr,
	    " -picio\t\tcompress output with picio encoding\n");
	fprintf(stderr,
	    " -packbits\tcompress output with packbits encoding\n");
	fprintf(stderr,
	    " -thunderscan\tcompress output with ThunderScan encoding\n");
	fprintf(stderr,
	    " -none\t\tuse no compression algorithm on output\n");
	fprintf(stderr, "\n");
	fprintf(stderr,
	    " -rowsperstrip #\tmake each strip have no more than # rows\n");
	exit(-1);
}
