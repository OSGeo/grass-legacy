
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
#include <ctype.h>
#include "tiffio.h"

/*
 * Histogram info, one per
 * possible tile flags value.
 */
struct picHistogram {
	int	h_count;		/* raw appearance count */
	int	h_maxcount;		/* max count */
	int	h_maxlen;		/* max run length */
	int	h_totlen;		/* average run length */
	int	h_totcount;		/* average count */
} hstats[16];

#define	PT_FILL		0		/* fill input buffer */
#define PT_FULLDUMP	1		/* full dump, use count */
#define PT_FULLRUN	2		/* full run, length precedes pixel */
#define PT_PARTDUMP	3		/* part dump, alpha constant */
#define PT_PARTRUN	4		/* part run, alpha constant */

main(argc, argv)
	char *argv[];
{
	TIFF *tif;
	int dirnum = -9999, y;
	u_short h, compression;
	int PicioHistogram();

	for (argc--, argv++; argc > 0 && argv[0][0] == '-'; argc--, argv++)
		if (isdigit(argv[0][1]))
			dirnum = atoi(&argv[0][1]);
	if (argc <= 0) {
		fprintf(stderr, "usage: hist [-#] TIFF-file\n");
		exit(-1);
	}
	tif = TIFFOpen(argv[0], "r");
	if (tif == NULL)
		exit(-1);
	if (dirnum != -9999)
		TIFFSetDirectory(tif, dirnum);
	TIFFGetField(tif, TIFFTAG_COMPRESSION, &compression);
	if (compression != COMPRESSION_PICIO) {
		fprintf(stderr, "%s: not compressed with picio.\n", argv[0]);
		exit(-1);
	}
	bzero(hstats, sizeof (hstats));
	tif->tif_stripdecode = NULL;
	tif->tif_decoderow = PicioHistogram;
	TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &h);
	for (y = 0; y < h; y++)
		if (TIFFReadScanline(tif, NULL, y, 0) < 0)
			break;
	PrintStats();
}

char *picNames[16] = {
    "FILL",	"FULLDUMP",	"FULLRUN",	"PARTDUMP",
    "PARTRUN",	"#5",		"#6",		"#7",
    "#8",	"#9",		"#10",		"#11",
    "#12",	"#13",		"#14",		"#15",
};

PicioHistogram(tif, buf, cc)
	TIFF *tif;
	u_char *buf;
{
	static char module[] = "PicioHistogram";
	register u_char *bp, *sp, *ep;
	register int npixels, inc;
	TIFFDirectory *td = &tif->tif_dir;
	short nc, flag, word, count, length;
	u_char spare[4];
	register struct picHistogram *hp;

	bp = (u_char *)tif->tif_rawcp;
	ep = bp + tif->tif_rawcc;
	npixels = td->td_imagewidth;
	nc = td->td_samplesperpixel;
	inc = td->td_bitspersample / 8;
	while (bp < ep && npixels > 0) {
		word = (bp[1] << 8) | bp[0], bp += 2;
		count = (word & 0xfff) + 1;
		flag = word >> 12;
		hp = &hstats[flag];
		hp->h_count++;
		if (count > hp->h_maxcount)
			hp->h_maxcount = count;
		hp->h_totcount += count;
		switch (flag) {
		case PT_FULLRUN:
			while (count-- > 0) {
				/*
				 * The first byte holds a count. 
				 * The next bytes hold data to be
				 * repeated.
				 */
				if (inc == 2) {
					length = *bp++;
					length |= *bp++ << 8;
					length++;
				} else
					length = (*bp++) + 1;
				if (length > hp->h_maxlen)
					hp->h_maxlen = length;
				hp->h_totlen += length;
				bp += nc * inc;
				npixels -= length;
			}
			break;
		case PT_FULLDUMP:
			npixels -= count;
			bp += count * nc * inc;
			break;
		case PT_PARTRUN:
			bp += inc;
			while (count-- > 0) {
				/*
				 * The first byte is a count. 
				 * The next bytes are data to
				 * be repeated.
				 */
				if (inc == 2) {
					length = *bp++;
					length |= *bp++ << 8;
					length++;
				} else
					length = (*bp++) + 1;
				if (length > hp->h_maxlen)
					hp->h_maxlen = length;
				hp->h_totlen += length;
				bp += (nc-1) * inc;
				npixels -= length;
			}
			break;
		case PT_PARTDUMP:
			/*
			 * The fixed value (normally alpha) is
			 * stored in the first byte.
			 */
			bp += inc;
			npixels -= count;
			bp += count * (nc-1) * inc;
			break;
		default:
			TIFFError(module, "%s: Unknown flag 0x%x",
			    tif->tif_name, flag);
			return (0);
		}
	}
	tif->tif_rawcc -= bp - (u_char *)tif->tif_rawcp;
	tif->tif_rawcp = (char *)bp;
	if (npixels > 0) {
		TIFFError(module, "%s: Not enough data for scanline %d",
		    tif->tif_name, tif->tif_row);
		return (0);
	}
	return (1);
}

PrintStats()
{
	int total;
	register struct picHistogram *hp;

	total = 0;
	for (hp = hstats; hp < &hstats[16]; hp++)
		total += hp->h_count;
	printf("%-9s          Count        Length\n", "");
	printf("%-9s   Total  Max   Avg   Max   Avg\n", "Type");
	/* we ignore PT_FILL statistics */
	for (hp = &hstats[1]; hp < &hstats[16]; hp++) {
		if (hp->h_count == 0)
			continue;
		printf("%-9s: %6d %4d %5.1f  %4d %5.1f  %4.1f%%\n",
		    picNames[hp - hstats], hp->h_count,
		    hp->h_maxcount,
#define	nz(x)	((x) == 0 ? 1 : (x))
		    (float)hp->h_totcount / nz(hp->h_count),
		    hp->h_maxlen,
		    (float)hp->h_totlen / nz(hp->h_count),
		    (100.0 * hp->h_count) / total);
	}
}
