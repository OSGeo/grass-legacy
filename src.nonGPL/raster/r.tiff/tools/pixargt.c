
#include <stdio.h>
#include <chad.h>
#include <pixeldef.h>
#include "tiffio.h"

static	int width, height;
extern	char *getenv();
static	int length;
ChadTB	*theTB;
ChadPW	*thePW;
ChadPC	*SFxCopy;
ChadSpad *spad;
RGBAPixelType *pixels;

main(argc, argv)
	char *argv[];
{
	int i, xoff, yoff;
	char *origin, *pausetime, *cp, *rindex(), *getenv();
	short c, val;
	TIFFDirectory *td;
	TIFF *tif;

	argc--, argv++;
	tif = TIFFOpen(argv[0], "r");
	if (tif == NULL)
		exit(-1);
	if (argc > 1) {
		i = atoi(argv[1])-1;
		while (i > 0 && TIFFReadDirectory(tif))
			i--;
		if (i > 0) {
			fprintf(stderr, "%s: Not that many directories.\n",
			    argv[1]);
			exit(-1);
		}
	}
	td = &tif->tif_dir;
	if (td->td_bitspersample != 1 && td->td_bitspersample != 4 &&
	    td->td_bitspersample != 8 && td->td_bitspersample != 16) {
		fprintf(stderr,
		    "Sorry, can only handle 1, 4, and 8-bit pictures\n");
		exit(-1);
	}
	if (td->td_samplesperpixel != 1 && td->td_samplesperpixel != 3 &&
	    td->td_samplesperpixel != 4) {
		fprintf(stderr,
    "Sorry, can only handle 1-channel gray scale or 3-channel color images\n");
		exit(-1);
	}
	width = td->td_imagewidth;
	if (width > 1024)		/* big windows crash system */
		width = 1024;
	height = td->td_imagelength;
	origin = getenv("DSPYORIGIN");
	if (origin && sscanf(origin, "%d,%d", &xoff, &yoff) == 2)
		prefposition(xoff, xoff + width - 1, yoff, yoff + height - 1);
	else
		prefsize(width, height);
	pixels = (RGBAPixelType *)malloc(width * sizeof (RGBAPixelType));
	gt(tif, width, height);
	TIFFClose(tif);
	DonePixar();
	if ((pausetime = getenv("DSPYPAUSE")) && pausetime[0] != '\0')
		sleep(atoi(pausetime));
	exit(0);
}

prefposition(xm, xM, ym, yM)
	int xm, xM, ym, yM;
{
	if (xM <= xm || yM <= ym) {
		fprintf(stderr, "prefposition: degenerate window\n");
		exit(1);
	}
	OpenPixar(xm, xM, ym, yM);
}

prefsize(width, height)
	int width, height;
{
	if (width <= 0 || height <= 0) {
		fprintf(stderr, "prefsize: degenerate window\n");
		exit(1);
	}
	OpenPixar(0, width-1, 0, height-1);
}

OpenPixar(xmin,xmax,ymin,ymax)
	int xmin,xmax,ymin,ymax;
{
	int w,h;

	ASSERT(ChadBegin(CHAP0,0));
	/* allocate the PW & TB */
	ASSERT(ChadAlloc(CHAP0,
            TB, &theTB, 0, 32, 128, /* kludge... for a 1024*4095 fb... */
            PW, &thePW, &theTB, xmin, xmax, ymin, ymax,
            NIX));
	ASSERT(ChadAlloc(CHAP0,
	    RAM, &SFxCopy, "SFxCopy",
	    SPAD, &spad, 2048,
	    NIX));
}

DonePixar()
{
	ASSERT(ChadFree(CHAP0,
	  thePW, theTB, SFxCopy, spad, NIX));
}

#define	howmany(x, y)	(((x)+((y)-1))/(y))
static	int Unit2Divisor[] = { 1, 10, 100, 1000, 10000, 100000 };

static
gt(tif, w, h)
	TIFF *tif;
	int w, h;
{
	register u_char *pp;
	register RGBAPixelType *xp;
	register int x, y, range;
	register u_short c, *wp;
	u_short *Map = NULL, *red, *green, *blue;
	u_char *buf;
	TIFFDirectory *td;

	buf = (u_char *)malloc(TIFFScanlineSize(tif));
	if (buf == 0) {
		fprintf(stderr, "No space for scanline buffer\n");
		return;
	}
	td = &tif->tif_dir;
	if (td->td_minsamplevalue != 0 || td->td_maxsamplevalue != 2047) {
		range = td->td_maxsamplevalue - td->td_minsamplevalue + 1;
		Map = (u_short *)malloc(range * sizeof (u_short));
		switch (td->td_photometric) {
		case PHOTOMETRIC_RGB:
		case PHOTOMETRIC_MINISBLACK:
			for (x = 0; x < range; x++)
				Map[x] = (x * 2047) / range;
			break;
		case PHOTOMETRIC_MINISWHITE:
			for (x = 0; x < range; x++)
				Map[x] = ((range - x) * 2047) / range;
			break;
		}
	}
	if (td->td_photometric == PHOTOMETRIC_RGB &&
	    td->td_samplesperpixel == 1) {
		if (!TIFFGetField(tif, TIFFTAG_COLORRESPONSECURVE,
		    &red, &green, &blue)) {
			fprintf(stderr,
			    "What, no colormap for single channel RGB?\n");
			exit(-1);
		}
		/* XXX scale response curves in-place to use as color map */
		y = Unit2Divisor[td->td_colorresponseunit];
		for (x = 0; x < range; x++) {
			red[x] /= y;
			green[x] /= y;
			blue[x] /= y;
		}
	}
	for (y = 0; y < h; y++) {
		if (TIFFReadScanline(tif, buf, y, 0) < 0)
			break;
		xp = pixels; pp = buf;
		switch (td->td_photometric) {
		case PHOTOMETRIC_RGB:
			switch (td->td_bitspersample) {
			case 16:
				wp = (u_short *)pp;
				for (x = 0; x < w; x++) {
					xp->Red = *wp++;
					xp->Green = *wp++;
					xp->Blue = *wp++;
					if (td->td_samplesperpixel == 4)
						xp->Alpha = *wp++;
					else
						xp->Alpha = 2048;
					xp++;
				}
				break;
			case 8:
				if (td->td_samplesperpixel >= 3) {
					for (x = 0; x < w; x++) {
						xp->Red = Map[*pp++];
						xp->Green = Map[*pp++];
						xp->Blue = Map[*pp++];
						if (td->td_samplesperpixel == 4)
							xp->Alpha = Map[*pp++];
						else
							xp->Alpha = 2048;
						xp++;
					}
				} else if (td->td_samplesperpixel == 1) {
					for (x = 0; x < w; x++) {
						c = *pp++;
						xp->Red = Map[red[c]];
						xp->Green = Map[green[c]];
						xp->Blue = Map[blue[c]];
						xp->Alpha = 2048;
						xp++;
					}
				}
				break;
			}
			break;
		case PHOTOMETRIC_MINISWHITE:
		case PHOTOMETRIC_MINISBLACK:
#define	SetRGBA(xp,c) \
    xp->Red = xp->Green = xp->Blue = c; xp->Alpha = 2048; xp++
			switch (td->td_bitspersample) {
			case 1:
				for (x = 0; x < w; x += 8) {
					SetRGBA(xp, Map[(*pp >> 7) & 1]);
					SetRGBA(xp, Map[(*pp >> 6) & 1]);
					SetRGBA(xp, Map[(*pp >> 5) & 1]);
					SetRGBA(xp, Map[(*pp >> 4) & 1]);
					SetRGBA(xp, Map[(*pp >> 3) & 1]);
					SetRGBA(xp, Map[(*pp >> 2) & 1]);
					SetRGBA(xp, Map[(*pp >> 1) & 1]);
					SetRGBA(xp, Map[*pp & 1]);
				}
				break;
			case 4:
				for (x = 0; x < w; x += 2) {
					SetRGBA(xp, Map[*pp >> 4]);
					SetRGBA(xp, Map[*pp++ & 0xf]);
				}
				break;
			case 8:
				for (x = 0; x < w; x++) {
					SetRGBA(xp, Map[*pp++]);
				}
				break;
			}
			break;
		}
		ASSERT(ChadWrite(CHAP0,
		   PIXELS,spad,pixels,w,0,
		   NIX));
		ASSERT(ChadWrite(CHAP0,
		   B0,thePW->addr,
		   B1,spad->addr,
		   R0,CHAD_ALLPROCS,w,
		   R1,CHAD_ALLPROCS,0,
		   R2,CHAD_ALLPROCS,y,
		   NIX));
		ASSERT(ChadGo(SFxCopy));
		/* busy wait for the CPU */
		ChadWaitCPU(CHAP0); 
	}
done:
	if (Map)
		free((char *)Map);
	free((char *)buf);
}
