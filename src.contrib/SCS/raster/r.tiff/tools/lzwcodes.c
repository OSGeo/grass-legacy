
/*
 * Copyright (c) 1990 by Sam Leffler.
 * All rights reserved.
 *
 * This file is provided for unrestricted use provided that this
 * legend is included on all tape media and as a part of the
 * software program in whole or part.  Users may copy, modify or
 * distribute this file at will.
 */

#include "tiffio.h"
#include <stdio.h>
#include <assert.h>

/*
 * Print a trace of the LZW decoding algorithm.
 */
#define MAXCODE(n)	((1 << (n)) - 1)
#define	BITS_MIN	9		/* start with 9 bits */
#define	BITS_MAX	12		/* max of 12 bit strings */
/* predefined codes */
#define	CODE_CLEAR	256		/* code to clear string table */
#define	CODE_EOI	257		/* end-of-information code */
#define CODE_FIRST	258		/* first free code entry */
#define	CODE_MAX	MAXCODE(BITS_MAX)
#define	HSIZE		5003		/* 80% occupancy */
#define	HSHIFT		(8-(16-12))
#define	STACK_SIZE	(10*(HSIZE - (CODE_MAX+1)))

typedef	struct {
	int	lzw_oldcode;		/* last code encountered */
	u_short	lzw_flags;
#define	LZW_COMPAT	0x04		/* read old bit-reversed codes */
	u_short	lzw_nbits;		/* number of bits/code */
	int	lzw_maxcode;		/* maximum code for lzw_nbits */
	long	lzw_bitoff;		/* bit offset into data */
	long	lzw_bitsize;		/* size of strip in bits */
	int	lzw_free_ent;		/* next free entry in hash table */
	short	dec_prefix[HSIZE];	/* prefix(code) */
	u_char	dec_suffix[CODE_MAX+1];	/* suffix(code) */
	u_char	dec_stack[STACK_SIZE];
	u_char	*dec_stackp;		/* stack pointer */
	int	dec_firstchar;		/* of string associated w/ last code */
} LZWState;

/* masks for extracting/inserting variable length bit codes */
static	u_char rmask[9] =
    { 0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff };
static	u_char lmask[9] =
    { 0x00, 0x80, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc, 0xfe, 0xff };

int	cc;
int	strip = 0;
u_char*	buf;

main(argc, argv)
    int argc;
    char** argv;
{
    TIFF* tif = TIFFOpen(argv[1], "r");
    if (!tif)
	exit(-1);
    for (;;) {
	buf = (u_char*) malloc(cc = tif->tif_dir.td_stripbytecount[strip]);
	if (TIFFReadRawStrip(tif, strip++, buf, cc) < 0)
	    break;
	{ LZWState lzw;
	  register int code;
	  register u_char *stackp;
	  int firstchar;
	  int oldcode;
	  int incode;

	  LZWPreDecode(&lzw);
	  oldcode = lzw.lzw_oldcode;
	  firstchar = lzw.dec_firstchar;
	  stackp = lzw.dec_stackp;
	  while ((incode = GetNextCode(&lzw)) != CODE_EOI) {
	      printf("=> %d\n", incode);
	      if (incode == CODE_CLEAR) {
		  printf("CLEAR\n");
		  lzw.lzw_free_ent = CODE_FIRST;
		  lzw.lzw_maxcode = MAXCODE(lzw.lzw_nbits = BITS_MIN);
		  bzero(lzw.dec_prefix, sizeof (lzw.dec_prefix));
		  printf("= USE %d-bit codes\n", lzw.lzw_nbits);
		  if ((incode = GetNextCode(&lzw)) == CODE_EOI)
			break;
		  printf("=> %d\n<= %d\n", incode, incode);
		  firstchar = incode;
	      } else {
		  code = incode;
		  /*
		   * When a code is not in the table we use (as spec'd):
		   *    StringFromCode(oldcode) +
		   *        FirstChar(StringFromCode(oldcode))
		   */
		  if (code >= lzw.lzw_free_ent) {	/* code not in table */
		      *stackp++ = firstchar;
		      code = oldcode;
		  }

		  /*
		   * Generate output string (first in reverse).
		   */
		  if (code < lzw.lzw_free_ent) {
		      for (; code >= 256; code = lzw.dec_prefix[code])
			  *stackp++ = lzw.dec_suffix[code];
		      *stackp++ = firstchar = lzw.dec_suffix[code];
		  } else {
		      printf("*** NON-EXISTENT STRING ***\n");
		      firstchar = code;
		  }
		  printf("<=");
		  do {
		      printf(" %d", *--stackp);
		  } while (stackp > lzw.dec_stack);
		  printf("\n");

		  /*
		   * Add the new entry to the code table.
		   */
		  if ((code = lzw.lzw_free_ent) < CODE_MAX) {
		      printf("ENTER[%d]=(%d,%d)\n", code, oldcode, firstchar);
		      /*
		       * This assert is to insure we don't loop
		       * in the above for-loop.  It is an error
		       * as it means that a code maps to itself --
		       * something that should only be true if
		       * the code <= 256.
		       */
		      if (oldcode == lzw.lzw_free_ent)
			  printf("*** LOOP IN CODE TABLE ***\n");
		      lzw.dec_prefix[code] = (u_short)oldcode;
		      lzw.dec_suffix[code] = firstchar;
		      lzw.lzw_free_ent++;
		      /*
		       * If the next entry is too big for the
		       * current code size, then increase the
		       * size up to the maximum possible.
		       */
		      if (lzw.lzw_free_ent > lzw.lzw_maxcode) {
			  lzw.lzw_nbits++;
			  if (lzw.lzw_nbits > BITS_MAX)
			      lzw.lzw_nbits = BITS_MAX;
			  printf("= USE %d-bit codes\n", lzw.lzw_nbits);
			  lzw.lzw_maxcode = MAXCODE(lzw.lzw_nbits);
		      }
		  } 
	      }
	      oldcode = incode;
	  }
	  printf("=> %d\n", code);
	}
	free((char*) buf);
    }
    TIFFClose(tif);
}

/*
 * Setup state for decoding a strip.
 */
static
LZWPreDecode(sp)
	LZWState* sp;
{
	int code;

	for (code = 255; code >= 0; code--)
		sp->dec_suffix[code] = (u_char)code;
	sp->lzw_flags = 0;
	sp->lzw_nbits = BITS_MIN;
	sp->lzw_maxcode = MAXCODE(BITS_MIN);
	sp->lzw_bitoff = 0;
	sp->lzw_free_ent = CODE_FIRST;
	/* calculate data size in bits */
	sp->lzw_bitsize = (cc << 3) - (BITS_MAX-1);
	sp->dec_stackp = sp->dec_stack;
	sp->lzw_oldcode = -1;
	sp->dec_firstchar = -1;
	/*
	 * Check for old bit-reversed codes.  All the flag
	 * manipulations are to insure only one warning is
	 * given for a file.
	 */
	if (buf[0] == 0 && buf[1] == 0x1)
		sp->lzw_flags |= LZW_COMPAT;
	else
		sp->lzw_flags &= ~LZW_COMPAT;
	return (1);
}

/*
 * Get the next code from the raw data buffer.
 */
static
GetNextCode(sp)
	register LZWState *sp;
{
	register int code, bits;
	register long r_off;
	register u_char *bp;

	/*
	 * This check shouldn't be necessary because each
	 * strip is suppose to be terminated with CODE_EOI.
	 * At worst it's a substitute for the CODE_EOI that's
	 * supposed to be there (see calculation of lzw_bitsize
	 * in LZWPreDecode()).
	 */
	if (sp->lzw_bitoff > sp->lzw_bitsize) {
		fprintf(stderr, "strip %d: out of data\n", strip-1);
		return (CODE_EOI);
	}
	r_off = sp->lzw_bitoff;
	bits = sp->lzw_nbits;
	/*
	 * Get to the first byte.
	 */
	bp = (u_char *)buf + (r_off >> 3);
	r_off &= 7;
	if (sp->lzw_flags & LZW_COMPAT) {
		/* Get first part (low order bits) */
		code = (*bp++ >> r_off);
		r_off = 8 - r_off;		/* now, offset into code word */
		bits -= r_off;
		/* Get any 8 bit parts in the middle (<=1 for up to 16 bits). */
		if (bits >= 8) {
			code |= *bp++ << r_off;
			r_off += 8;
			bits -= 8;
		}
		/* high order bits. */
		code |= (*bp & rmask[bits]) << r_off;
	} else {
		r_off = 8 - r_off;		/* convert offset to count */
		code = *bp++ & rmask[r_off];	/* high order bits */
		bits -= r_off;
		if (bits >= 8) {
			code = (code<<8) | *bp++;
			bits -= 8;
		}
		/* low order bits */
		code = (code << bits) | ((*bp & lmask[bits]) >> (8 - bits));
	}
	sp->lzw_bitoff += sp->lzw_nbits;
	return (code);
}
