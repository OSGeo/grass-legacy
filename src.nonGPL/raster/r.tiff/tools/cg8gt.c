
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
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include "tiffio.h"

static	Window frame, canvas;
static	Pixwin *pw;			/* canvas drawn into */
static	Pixrect *pr;			/* scanline pixrect */
static	int width, height;
extern	char *getenv();

main(argc, argv)
	char *argv[];
{
	int xoff, yoff;
	char *origin;
	TIFFDirectory *td;
	TIFF *tif;

	tif = TIFFOpen(argv[1], "r");
	if (tif == NULL)
		exit(-1);
	td = &tif->tif_dir;
	if (td->td_bitspersample > 8) {
		fprintf(stderr,
		    "Sorry, can't handle more than 8-bits per sample\n");
		exit(-1);
	}
	if (td->td_samplesperpixel != 1 && td->td_samplesperpixel != 3 &&
	    td->td_samplesperpixel != 4) {
		fprintf(stderr,
    "Sorry, can only handle 1-channel gray scale or 3-channel color images\n");
		exit(-1);
	}
	width = td->td_imagewidth;
	height = td->td_imagelength;
	frame = window_create(NULL, FRAME,
	    FRAME_LABEL,	argv[1],
	    0);
	if (frame == NULL) {
		fprintf(stderr, "Can not open window\n");
		exit(-1);
	}
	canvas = window_create(frame, CANVAS,
	    CANVAS_FIXED_IMAGE,	TRUE,
	    CANVAS_COLOR24,	TRUE,
	    WIN_WIDTH,		width,
	    WIN_HEIGHT,		height,
	    0);
	if (canvas == NULL) {
		fprintf(stderr, "Can not open canvas\n");
		exit(-1);
	}
	window_fit(frame);
	origin = getenv("DSPYORIGIN");
	if (origin && sscanf(origin, "%d,%d", &xoff, &yoff) == 2)
		window_set(frame, WIN_X, xoff, WIN_Y, yoff, 0);
	pr = mem_create(width, 1, 32);
	window_set(frame, WIN_SHOW, TRUE, 0);
	(void) notify_dispatch();
	gt(tif, pw = canvas_pixwin(canvas), width, height);
	window_main_loop(frame);
	exit(0);
}

gt(tif, pw, w, h)
	TIFF *tif;
	register Pixwin *pw;
	int w, h;
{
	register u_char *pp;
	register unsigned pixel;
	register int x, y, c, maxcolor;
	u_char *Map = NULL, *buf;
	TIFFDirectory *td;

	td = &tif->tif_dir;
	buf = (u_char *)malloc(w * td->td_samplesperpixel * sizeof (char));
	if (buf == 0) {
		fprintf(stderr, "No space for scanline buffer\n");
		return;
	}
	if (td->td_photometric != PHOTOMETRIC_RGB) {
		maxcolor = (1<<td->td_bitspersample)-1;
		Map = (u_char *)malloc((maxcolor + 1) * sizeof (u_char));
		switch (td->td_photometric) {
		case PHOTOMETRIC_MINISBLACK:
			for (x = 0; x < maxcolor; x++)
				Map[x] = (x * 256) / 256;
			break;
		case PHOTOMETRIC_MINISWHITE:
			for (x = 0; x < maxcolor; x++)
				Map[x] = ((maxcolor - x) * 256) / 256;
			break;
		}
	}
	for (y = 0; y < h; y++) {
		if (TIFFReadScanline(tif, buf, y, 0) < 0)
			break;
		pp = buf;
		switch (td->td_photometric) {
		case PHOTOMETRIC_RGB:
			for (x = 0; x < w; x++) {
				pixel = *pp++;
				pixel |= *pp++ << 8;
				pixel |= *pp++ << 16;
				if (td->td_samplesperpixel == 4)
					pp++;
				pr_put(pr, x, 0, pixel);
			}
			break;
		case PHOTOMETRIC_MINISWHITE:
		case PHOTOMETRIC_MINISBLACK:
			for (x = 0; x < w; x++) {
				c = Map[*pp++];
				pixel = c|(c<<8)|(c<<16);
				pr_put(pr, x, 0, pixel);
			}
			break;
		}
		pw_write(pw, 0, y, width, 1, PIX_SRC, pr, 0, 0);
	}
done:
	if (Map)
		free((char *)Map);
	free((char *)buf);
}
