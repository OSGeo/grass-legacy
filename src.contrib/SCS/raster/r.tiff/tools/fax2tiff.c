
/*
 * Copyright (c) 1990 by Sam Leffler.
 * All rights reserved.
 *
 * This file is provided for unrestricted use provided that this
 * legend is included on all tape media and as a part of the
 * software program in whole or part.  Users may copy, modify or
 * distribute this file at will.
 */

/* 
 * Convert a CCITT Group 3 FAX file to TIFF Group 3 format.
 * Based on Paul Haeberli's fromfax program (with help from Jef Poskanzer).
 */
#include <stdio.h>
#include "tiffio.h"
#include "t.4.h"

#define DEFXSIZE	1728
#define DEFYSIZE	(2*1075)

#define MAXWIDTH	2048

#define	howmany(x, y)	(((x)+((y)-1))/(y))

int	eofflag, eols;
int	rawzeros;
int	badfaxlines;
int	badfaxrun;
int	maxbadfaxrun;
int	rows1d;
int	rows2d;
int	shdata;
int	shbit;
int	verbose;
int	stretch;
int	reversebits	= 0;
int	warnings	= 0;
int	is2D		= 0;
char	*filename;
u_char	*refline;

usage()
{
	fprintf(stderr, "usage: fax2tiff [-124fklprsvwO] [-o image.tif] faxfile ...\n");
	exit(-1);
}

int	kludge = 0;

main(argc, argv)
	int argc;
	char *argv[];
{
	FILE *in;
	TIFF *out = NULL;
	int compression = COMPRESSION_CCITTFAX3;
	int fillorder = FILLORDER_LSB2MSB;
	int group3options = GROUP3OPT_2DENCODING;
	int photometric = PHOTOMETRIC_MINISBLACK;
	int isClassF = 1;
	int rows;
	int c;
	int pn, npages;
	float yres = 200.;
	extern int optind;
	extern char *optarg;

	while ((c = getopt(argc, argv, "o:bcfklOprsvwz124")) != -1)
		switch (c) {
		case '1':
			group3options &= ~GROUP3OPT_2DENCODING;
			break;
		case '2':
			is2D = 1;
			break;
		case '4':
			compression = COMPRESSION_CCITTFAX4;
			break;
		case 'b':		/* undocumented -- for testing */
			photometric = PHOTOMETRIC_MINISWHITE;
			break;
		case 'c':		/* generate "classic" g3 format */
			isClassF = 0;
			break;
		case 'f':		/* generate Class F format */
			isClassF = 1;
			break;
		case 'k':		/* kludge to skip over input trash */
			kludge = 1;
			break;
		case 'l':		/* low resolution input */
			yres = 100.;
			break;
		case 'O':		/* reverse bit ordering in output */
			fillorder = FILLORDER_MSB2LSB;
			break;
		case 'o':
			out = TIFFOpen(optarg, "w");
			if (out == NULL)
				exit(-2);
			break;
		case 'p':		/* zero pad scanlines before EOL */
			group3options |= GROUP3OPT_FILLBITS;
			break;
		case 'r':		/* input file has reversed fillorder */
			reversebits = 1;
			break;
		case 's':		/* stretch image by dup'ng scanlines */
			stretch = 1;
			break;
		case 'w':		/* enable warnings about short lines */
			warnings = 1;
			break;
		case 'v':		/* -v for info, -vv for debugging */
			verbose++;
			break;
		case 'z':		/* undocumented -- for testing */
			compression = COMPRESSION_LZW;
			break;
		case '?':
			usage();
			/*NOTREACHED*/
		}
	if (kludge && is2D) {
		fprintf(stderr,
	"%s: combining -k and 2d decoding options does not make sense.\n",
			argv[0]);
		exit(-1);
	}
	if (stretch && yres == 100.)
		yres = 200.;
	if (out == NULL) {
		out = TIFFOpen("fax.tif", "w");
		if (out == NULL)
			exit(-2);
	}
	npages = argc - optind;
	if (npages < 1)
		usage();
	for (pn = 0; optind < argc; pn++, optind++) {
		in = fopen(filename = argv[optind], "r");
		if (in == NULL) {
			fprintf(stderr,
			    "%s: %s: Can not open\n", argv[0], filename);
			continue;
		}
		TIFFSetField(out, TIFFTAG_IMAGEWIDTH, DEFXSIZE);
		TIFFSetField(out, TIFFTAG_BITSPERSAMPLE, 1);
		TIFFSetField(out, TIFFTAG_COMPRESSION, compression);
		TIFFSetField(out, TIFFTAG_PHOTOMETRIC, photometric);
		TIFFSetField(out, TIFFTAG_ORIENTATION, ORIENTATION_TOPLEFT);
		TIFFSetField(out, TIFFTAG_SAMPLESPERPIXEL, 1);
		if (compression == COMPRESSION_CCITTFAX3) {
			TIFFSetField(out, TIFFTAG_GROUP3OPTIONS, group3options);
			TIFFModeCCITTFax3(out, isClassF);
		}
		if (compression == COMPRESSION_CCITTFAX3 ||
		    compression == COMPRESSION_CCITTFAX4)
			TIFFSetField(out, TIFFTAG_ROWSPERSTRIP, -1);
		else
			TIFFSetField(out, TIFFTAG_ROWSPERSTRIP,
			    (8*1024)/TIFFScanlineSize(out));
		TIFFSetField(out, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
		TIFFSetField(out, TIFFTAG_FILLORDER, fillorder);
		TIFFSetField(out, TIFFTAG_SOFTWARE, "fax2tiff");
		TIFFSetField(out, TIFFTAG_XRESOLUTION, 200.0);
		TIFFSetField(out, TIFFTAG_YRESOLUTION, yres);
		TIFFSetField(out, TIFFTAG_RESOLUTIONUNIT, RESUNIT_INCH);
		TIFFSetField(out, TIFFTAG_PAGENUMBER, pn, npages);

		rows = copyFaxFile(in, out);
		fclose(in);

		if (verbose) {
			fprintf(stderr, "%s:\n", argv[optind]);
			fprintf(stderr, "%d rows in input\n", rows);
			fprintf(stderr, "%d total bad rows\n", badfaxlines);
			fprintf(stderr, "%d max consecutive bad rows\n",
			    maxbadfaxrun);
			if (is2D)
				fprintf(stderr, "%d 1d rows, %d 2d rows\n",
				    rows1d, rows2d);
		}
		if (compression == COMPRESSION_CCITTFAX3 && isClassF) {
			TIFFSetField(out, TIFFTAG_BADFAXLINES, badfaxlines);
			TIFFSetField(out, TIFFTAG_CLEANFAXDATA,
			    badfaxlines ?
				CLEANFAXDATA_REGENERATED : CLEANFAXDATA_CLEAN);
			TIFFSetField(out, TIFFTAG_CONSECUTIVEBADFAXLINES,
			    maxbadfaxrun);
		}
		TIFFWriteDirectory(out);
	}
	TIFFClose(out);
	fclose(in);
	exit(0);
}

copyFaxFile(in, out)
	FILE *in;
	TIFF *out;
{
	u_char *buf;
	int row;
	int bsize;
	enum { G3_1D, G3_2D } tag;

	eofflag = 0;
	eols = 0;
	if (!is2D) {
		if (kludge) {
			/* XXX skip over some strange stuff at the beginning */
			skiptoeol(in);
			skiptoeol(in);
			skiptoeol(in);
		}
		skiptoeol(in);
	}
	tag = G3_1D;

	bsize = howmany(MAXWIDTH, 8);
	buf = (u_char *)malloc(bsize);
	refline = (u_char *)malloc(bsize);

	row = 0;
	badfaxlines = 0;
	badfaxrun = 0;
	maxbadfaxrun = 0;
	rows1d = rows2d = 0;
	while (!eofflag) {
		if (verbose > 1)
			fprintf(stderr, "row %d %s:\n",
			    row, tag == G3_1D ? "1d" : "2d");
		bzero(buf, bsize);
		if (tag == G3_1D) {
			if (!get1dfaxrow(in, row, buf, DEFXSIZE))
				break;
			rows1d++;
		} else {
			if (!get2dfaxrow(in, row, buf, DEFXSIZE))
				break;
			rows2d++;
		}
		bcopy(buf, refline, bsize);
		if (is2D)
			tag = (rawgetbit(in) ? G3_1D : G3_2D);
		if (TIFFWriteScanline(out, buf, row, 0) < 0) {
			fprintf(stderr, "%s: Problem at row %d.\n",
			    filename, row);
			break;
		}
		row++;
		if (stretch) {
			if (TIFFWriteScanline(out, buf, row, 0) < 0) {
				fprintf(stderr, "%s: Problem at row %d.\n",
				    filename, row);
				break;
			}
			row++;
		}
	}
	if (badfaxrun > maxbadfaxrun)
		maxbadfaxrun = badfaxrun;
	return (row);
}

/*
 * Fill a span with ones.
 */
static void
fillspan(cp, x, count)
	register u_char *cp;
	register int x, count;
{
	static unsigned char masks[] =
	    { 0, 0x80, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc, 0xfe, 0xff };

	cp += x>>3;
	if (x &= 7) {
		if (count < 8 - x) {
			*cp++ |= masks[count] >> x;
			return;
		}
		*cp++ |= 0xff >> x;
		count -= 8 - x;
	}
	while (count >= 8) {
		*cp++ = 0xff;
		count -= 8;
	}
	*cp |= masks[count];
}

badcodeword(inf, buf, n)
	FILE *inf;
	char *buf;
	int n;
{
	badfaxlines++;
	badfaxrun++;
	skiptoeol(inf);
	bcopy(refline, buf, howmany(n, 8));
	return (!eofflag);
}

#include <setjmp.h>

jmp_buf	EOFbuf;

get1dfaxrow(inf, row, buf, n)
	FILE *inf;
	int row;
	u_char *buf;
	int n;
{
	int x;
	int len, code, nextbit;
	int count, color;
	tableentry *te;

	x = 0;
	rawzeros = 0;
	len = 0;
	code = 0;
	color = 1;
	count = 0;
	if (setjmp(EOFbuf))
		return (0);
	while (!eofflag) {
		if (x >= n) {
			if (badfaxrun > maxbadfaxrun)
				maxbadfaxrun = badfaxrun;
			badfaxrun = 0;
			skiptoeol(inf);
			return (!eofflag); 
		}
		do {
			if (verbose > 2)
				fprintf(stderr, "[l %d] ", x);
			if (rawzeros >= 11) {
				nextbit = rawgetbit(inf);
				if (nextbit) {
					if (x == 0)
						/* XXX should be 6 */
						eofflag = (++eols == 3);
					else
						eols = 0;
					if (warnings && x && x < n)
						fprintf(stderr,
				"%s: Warning, row %d short (len %d).\n",
						    filename, row, x);
					if (badfaxrun > maxbadfaxrun)
						maxbadfaxrun = badfaxrun;
					badfaxrun = 0;
					return (!eofflag); 
				}
			} else
				nextbit = rawgetbit(inf);
			code = (code<<1) + nextbit;
			len++;
		} while (code <= 0);
		if (len > 13) {
			fprintf(stderr,
"%s: row %d: Bad 1d code word (x %d, len %d, code 0x%x), using previous line\n",
			    filename, row, x, len, code);
			return (badcodeword(inf, buf, n));
		}
		if (color) {
			u_char ix = TIFFFax3wtab[code << (13-len)];
			if (ix == 0xff)
				continue;
			te = &TIFFFax3wcodes[ix];
			if (te->length != len)
				continue;
		} else {
			u_char ix = TIFFFax3btab[code << (13-len)];
			if (ix == 0xff)
				continue;
			te = &TIFFFax3bcodes[ix];
			if (te->length != len)
				continue;
		}
		count += te->count;
		if (te->tabid < 0) {
			if (x+count > n)
				count = n-x;
			if (count > 0) {
				if (color)
					fillspan(buf, x, count);
				x += count;
			}
			count = 0;
			color = !color;
		}
		code = len = 0;
	}
	return (0);
}

/*
 * Return the offset of the next bit in the range
 * [bs..be] that is different from bs.  The end,
 * be, is returned if no such bit exists.
 */
static int
ffdiff(cp, bs, be)
	u_char *cp;
	int bs, be;
{
	u_int bc = bs + 1;		/* ``current'' bit offset */
	u_char mask = 0x80 >> (bs&7);	/* mask for starting bit */
	u_char b;

	cp += bs >> 3;			/* adjust to starting byte */
	if ((b = *cp++) & mask) {
		/*
		 * The current bit is a 1, find the next 0
		 * in the string.  We first check the remainder
		 * of the current byte.  If that fails, we
		 * then locate the first potential byte with
		 * a zero, and then check bits within the byte.
		 */
		if (mask >>= 1) {	/* check remainder of this byte */
			while ((b & mask) && bc < be)
				bc++, mask >>= 1;
			if (mask)
				return (bc);
		}
		while ((b = *cp++) == 0xff && bc < be)
			bc += 8;
		for (mask = 0x80; (b & mask) && bc < be; bc++, mask >>= 1)
			;
	} else {
		/*
		 * The current bit is a 0, find the next 1
		 * in the string.  The logic is identical to
		 * above, except that we locate a 1 instead
		 * of a 0.
		 */
		if (mask >>= 1) {
			while (mask && (b & mask) == 0 && bc < be)
				bc++, mask >>= 1;
			if (mask)
				return (bc);
		}
		while ((b = *cp++) == 0 && bc < be)
			bc += 8;
		for (mask = 0x80; (b & mask) == 0 && bc < be; bc++, mask >>= 1)
			;
	}
	return (bc < be ? bc : be);
}

#define	MODE_OTHER	0	/* !horizontal mode */
#define	MODE_HORIZ	1	/* horizontal mode, handling 1st runlen */
#define	MODE_HORIZ1	2	/* horizontal mode, handling 2d runlen */

#define	PACK(code,len)	(((len)<<2)+(code))

#define	PASS		1
#define	HORIZONTAL	2
#define	VERTICAL	3
#define	EXTENSION	4
#define	    UNCOMPRESSED	1

#define	PACKINFO(mode,v)	(((v)<<4)+mode)
#define	UNPACKMODE(v)		((v)&0xf)
#define	UNPACKINFO(v)		((v)>>4)

static short g32dtab[56] = {
       0,    0,    0,    0,    0,    3,    0,    0,	/* 0x00 - 0x07 */
       0,    0,    0,    0,    0,    2,  -13,   19,	/* 0x08 - 0x0f */
       0,    1,    0,    0,    0,    0,    0,    0,	/* 0x10 - 0x17 */
       0,    0,  -29,   35,    0,    0,  -45,   51,	/* 0x18 - 0x1f */
       0,    0,    0,    0,    0,    0,    0,    0,	/* 0x20 - 0x27 */
       0,    0,    0,    0,    0,    0,    0,    0,	/* 0x28 - 0x2f */
       0,    0,    0,    0,    0,    0,    0,   20,	/* 0x30 - 0x37 */
};
#define	NG32D	(sizeof (g32dtab) / sizeof (g32dtab[0]))

/*
 * Process one row of 2-D encoded data.
 */
get2dfaxrow(inf, row, buf, n)
	FILE* inf;
	int row;
	u_char *buf;
	int n;
{
#define	PIXEL(buf,ix)	((((buf)[(ix)>>3]) >> (7-((ix)&7))) & 1)
#define	SAMECOLOR(a,b)	(!((a) ^ (b)))
#define	COLOR(v)	((v)?"white":"black")
	int nextbit;
	short a0 = 0;
	short b1 = 0;
	short b2 = 0;
	short len = 0;
	short code = 0;
	short count;
	int mode = MODE_OTHER;
	int color = 1;

	rawzeros = 0;
	if (setjmp(EOFbuf))
		return (0);
	while (!eofflag) {
		do {
			if (rawzeros >= 11) {
				nextbit = rawgetbit(inf);
				if (nextbit) {
					if (a0 == 0)
						/* XXX should be 6 */
						eofflag = (++eols == 3);
					else
						eols = 0;
					if (warnings && a0 && a0 < n)
						fprintf(stderr,
				"%s: Warning, row %d short (len %d).\n",
						    filename, row, a0);
					if (badfaxrun > maxbadfaxrun)
						maxbadfaxrun = badfaxrun;
					badfaxrun = 0;
					return (!eofflag); 
				}
			} else
				nextbit = rawgetbit(inf);
			code = (code<<1) + nextbit;
			len++;
		} while (code <= 0);
		if (len > 13) {
			fprintf(stderr,
"%s: row %d: Bad 2d code word (x %d, len %d, code 0x%x), using previous line\n",
			    filename, row, a0, len, code);
			return (badcodeword(inf, buf, n));
		}
		if (mode != MODE_OTHER) {
			tableentry *te;
			u_int h;

			/*
			 * In horizontal mode, collect 1d code
			 * words that represent |a0a1| and |a1a2|.
			 */
			if (color) {
				u_char ix = TIFFFax3wtab[code << (13-len)];
				if (ix == 0xff)
					continue;
				te = &TIFFFax3wcodes[ix];
				if (te->length != len)
					continue;
			} else {
				u_char ix = TIFFFax3btab[code << (13-len)];
				if (ix == 0xff)
					continue;
				te = &TIFFFax3bcodes[ix];
				if (te->length != len)
					continue;
			}
			count += te->count;
			if (te->tabid < 0) {
if (verbose > 1)
fprintf(stderr, "horizontal %s (%d), a0 %d\n", COLOR(color), count, a0);
				if (a0 + count > n)
					count = n - a0;
				if (count > 0) {
					if (color)
						fillspan(buf, a0, count);
					a0 += count;
					/* XXX check a0 <= n */
				}
				if (mode == MODE_HORIZ) {
					count = 0;
					mode++;
				} else
					mode = MODE_OTHER;
				color = !color;
			}
		} else {
			int v = PACK(code,len);
			if (v >= NG32D || !(v = g32dtab[v])) {
				continue;
			}
			switch (UNPACKMODE(v)) {
			case PASS:
				b1 = (a0 == 0 && !PIXEL(refline, 0) ?
				    0 : ffdiff(refline, a0, n));
				if (SAMECOLOR(color, PIXEL(refline, b1)))
					b1 = ffdiff(refline, b1, n);
				b2 = ffdiff(refline, b1, n);
				if (color)
					fillspan(buf, a0, b2 - a0);
				a0 += b2 - a0;
if (verbose > 1)
fprintf(stderr, "pass a0 %d b1 %d b2 %d\n", a0, b1, b2);
				break;
			case HORIZONTAL:
				mode = MODE_HORIZ;
				count = 0;
				break;
			case VERTICAL:
				b1 = (a0 == 0 && !PIXEL(refline, 0) ?
				    0 : ffdiff(refline, a0, n));
				if (SAMECOLOR(color, PIXEL(refline, b1)))
					b1 = ffdiff(refline, b1, n);
				b1 += UNPACKINFO(v);
				if (color)
					fillspan(buf, a0, b1 - a0);
if (verbose > 1)
fprintf(stderr, "V%c(%d) %s, a0 %d b1 %d\n", " \0lr"[code], UNPACKINFO(v), COLOR(color), a0, b1);
				a0 += b1 - a0;
				color = !color;
				break;
			case EXTENSION:
if (verbose > 1)
fprintf(stderr, "**uncompressed data**\n");
				break;
			}
		}
		code = len = 0;
	}
	return (0);
#undef	SAMECOLOR
#undef	PIXEL
}

skiptoeol(file)
	FILE *file;
{

	while (rawzeros < 11)
		(void) rawgetbit(file);
	while (!rawgetbit(file))
		;
}

int
rawgetbit(file)
	FILE *file;
{
	if ((shbit&0xff) == 0) {
		shdata = getc(file);
		if (shdata == EOF) {
			fprintf(stderr, "%s: Premature EOF\n", filename);
			eofflag++;
			longjmp(EOFbuf, 1);
		}
		shbit = reversebits ? 0x01 : 0x80; 
	}
	if (shdata&shbit) {
		rawzeros = 0;
		if (reversebits)
			shbit <<= 1; 
		else
			shbit >>= 1;
		return (1);
	} else {
		rawzeros++;
		if (reversebits)
			shbit <<= 1; 
		else
			shbit >>= 1;
		return (0);
	}
}
