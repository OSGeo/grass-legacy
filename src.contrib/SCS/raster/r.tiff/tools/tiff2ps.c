
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

main(argc, argv)
	char *argv[];
{
	int dirnum = -1;
	TIFF *tif;
	char *cp;

	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		for (cp = &argv[0][1]; *cp; cp++) switch (*cp) {
		case '0': case '1': case '2': case '3':
		case '4': case '5': case '6': case '7':
		case '8': case '9':
			dirnum = atoi(cp);
			goto next;
		default:
			usage(-1);
			/*NOTREACHED*/
		}
	next:
		argc--, argv++;
	}
	if (argc <= 0)
		usage(-2);
	tif = TIFFOpen(argv[0], "r");
	if (tif != NULL) {
		if (dirnum != -1 && !TIFFSetDirectory(tif, dirnum))
			exit(-1);
		TIFF2PS(argv[0], tif);
	}
	exit(0);
}

usage(code)
	int code;
{
	fprintf(stderr, "Usage: tiff2ps [-dirnum] file\n");
	exit(code);
}

#define	MAXLINE	35

TIFF2PS(file, tif)
	char *file;
	TIFF *tif;
{
	u_short config, w, h, bitspersample = 1, samplesperpixel;
	int row, bytesperrow, breaklen;
	u_char *buf;
	float ox, oy;
	long t;
	extern char *ctime();

	TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bitspersample);
	switch (bitspersample) {
	case 1: case 2:
	case 4: case 8:
		break;
	default:
		fprintf(stderr, "%s: Can not image a %d-bit/sample image.\n",
		    file, bitspersample);
		return;
	}
	if (TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samplesperpixel) &&
	    samplesperpixel != 1) {
		fprintf(stderr, "%s: Can only handle B&W images.\n");
		return;
	}
	buf = (u_char *)malloc(bytesperrow = TIFFScanlineSize(tif));
	if (buf == NULL) {
		fprintf(stderr, "No space for scanline buffer\n");
		return;
	}
	TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &w);
	TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &h);
	printf("%%!PS-Adobe-2.0 EPSF-1.2\n");
	printf("%%%%Creator: tiff2ps\n");
	printf("%%%%Title: %s\n", file);
	t = time(0);
	printf("%%%%CreationDate: %s", ctime(&t));
	if (!TIFFGetField(tif, TIFFTAG_XPOSITION, &ox))
		ox = 0;
	if (!TIFFGetField(tif, TIFFTAG_YPOSITION, &oy))
		oy = 0;
	printf("%%%%Origin: %g %g\n", ox, oy);
	printf("%%%%BoundingBox: 0 0 %d %d\n", w, h);	/* XXX */
	printf("%%%%EndComments\n");
	printf("gsave\n");
	printf("10 dict begin\n");
	printf("/picstr %d string def\n", bytesperrow);
	printf("%d %d scale\n", w, h);
	printf("%d %d %d\n", w, h, bitspersample);
	/* should set matrix according to Orientation */
	printf("[%d 0 0 -%d 0 %d]\n", w, h, h);
	printf("{currentfile picstr readhexstring pop} image\n");
	breaklen = MAXLINE;
	for (row = 0; row < h; row++) {
		register u_char *cp;
		register int cc;

		if (TIFFReadScanline(tif, buf, row, 0) < 0)
			break;
		for (cp = buf, cc = bytesperrow; cc-- > 0;) {
			if (--breaklen == 0) {
				putchar('\n');
				breaklen = MAXLINE-1;
			}
			printf("%02X", *cp++);
		}
	}
	putchar('\n');
	printf("end\n");
	printf("grestore\n");
	free((char *)buf);
}
