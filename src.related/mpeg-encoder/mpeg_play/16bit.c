/*
 * Copyright (c) 1995 The Regents of the University of California.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/*
 * Copyright (c) 1995 Erik Corry
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL ERIK CORRY BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 * SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF
 * THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF ERIK CORRY HAS BEEN ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * ERIK CORRY SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS"
 * BASIS, AND ERIK CORRY HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT,
 * UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#include "video.h"
#include "dither.h"
#include "proto.h"

#define INTERPOLATE

/*
 * Erik Corry's multi-byte dither routines.
 *
 * The basic idea is that the Init generates all the necessary tables.
 * The tables incorporate the information about the layout of pixels
 * in the XImage, so that it should be able to cope with 15-bit, 16-bit
 * 24-bit (non-packed) and 32-bit (10-11 bits per color!) screens.
 * At present it cannot cope with 24-bit packed mode, since this involves
 * getting down to byte level again. It is assumed that the bits for each
 * color are contiguous in the longword.
 * 
 * Writing to memory is done in shorts or ints. (Unfortunately, short is not
 * very fast on Alpha, so there is room for improvement here). There is no
 * dither time check for overflow - instead the tables have slack at
 * each end. This is likely to be faster than an 'if' test as many modern
 * architectures are really bad at ifs. Potentially, each '&&' causes a 
 * pipeline flush!
 *
 * There is no shifting and fixed point arithmetic, as I really doubt you
 * can see the difference, and it costs. This may be just my bias, since I
 * heard that Intel is really bad at shifting.
 */

/*
 * How many 1 bits are there in the longword.
 * Low performance, do not call often.
 */
static int
number_of_bits_set(unsigned long a)
{
    if(!a) return 0;
    if(a & 1) return 1 + number_of_bits_set(a >> 1);
    return(number_of_bits_set(a >> 1));
}

/*
 * Shift the 0s in the least significant end out of the longword.
 * Low performance, do not call often.
 */
static unsigned long
shifted_down(unsigned long a)
{
    if(!a) return 0ul;
    if(a & 1) return a;
    return a >> 1;
}

/*
 * How many 0 bits are there at most significant end of longword.
 * Low performance, do not call often.
 */
static int
free_bits_at_top(unsigned long a)
{
      /* assume char is 8 bits */
    if(!a) return sizeof(unsigned long) * 8;
        /* assume twos complement */
    if(((long)a) < 0l) return 0;
    return 1 + free_bits_at_top ( a << 1);
}

/*
 * How many 0 bits are there at least significant end of longword.
 * Low performance, do not call often.
 */
static int
free_bits_at_bottom(unsigned long a)
{
      /* assume char is 8 bits */
    if(!a) return sizeof(unsigned long) * 8;
    if(((long)a) & 1l) return 0;
    return 1 + free_bits_at_bottom ( a >> 1);
}

static int *L_tab, *Cr_r_tab, *Cr_g_tab, *Cb_g_tab, *Cb_b_tab;

/*
 * We define tables that convert a color value between -256 and 512
 * into the R, G and B parts of the pixel. The normal range is 0-255.
 */

static long *r_2_pix;
static long *g_2_pix;
static long *b_2_pix;
static long *r_2_pix_alloc;
static long *g_2_pix_alloc;
static long *b_2_pix_alloc;



/*
 *--------------------------------------------------------------
 *
 * InitColor16Dither --
 *
 *	To get rid of the multiply and other conversions in color
 *	dither, we use a lookup table.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The lookup tables are initialized.
 *
 *--------------------------------------------------------------
 */

void
InitColorDither(thirty2)
int thirty2;
{
    /*
     * misuse of the wpixel array for the pixel masks. Note that this
     * implies that the window is created before this routine is called
     */
    unsigned long red_mask = wpixel[0];
    unsigned long green_mask = wpixel[1];
    unsigned long blue_mask = wpixel[2];

    int L, CR, CB, i;

    L_tab    = (int *)malloc(256*sizeof(int)); 
    Cr_r_tab = (int *)malloc(256*sizeof(int));
    Cr_g_tab = (int *)malloc(256*sizeof(int));
    Cb_g_tab = (int *)malloc(256*sizeof(int));
    Cb_b_tab = (int *)malloc(256*sizeof(int));

    r_2_pix_alloc = (long *)malloc(768*sizeof(long));
    g_2_pix_alloc = (long *)malloc(768*sizeof(long));
    b_2_pix_alloc = (long *)malloc(768*sizeof(long));

    for (i=0; i<256; i++) {
      L = i - 16;
      L_tab[i] = 1.164 * L;
      if (gammaCorrectFlag) {
        L_tab[i] = pow(L_tab[i] / 255.0, 1 / gammaCorrect) * 255.0 + 0.5;
      }

      CB = CR = i;

      CB -= 128; CR -= 128;
/* was
      Cr_r_tab[i] =  1.596 * CR;
      Cr_g_tab[i] = -0.813 * CR;
      Cb_g_tab[i] = -0.391 * CB;   
      Cb_b_tab[i] =  2.018 * CB;
*/
      Cr_r_tab[i] =  1.366 * CR;
      Cr_g_tab[i] = -0.700 * CR;
      Cb_g_tab[i] = -0.334 * CB;   
      Cb_b_tab[i] =  1.732 * CB;

    }

    /* 
     * Set up entries 0-255 in rgb-to-pixel value tables.
     */
    for (i = 0; i < 256; i++) {
      r_2_pix_alloc[i + 256] = i >> (8 - number_of_bits_set(red_mask));
      r_2_pix_alloc[i + 256] <<= free_bits_at_bottom(red_mask);
      g_2_pix_alloc[i + 256] = i >> (8 - number_of_bits_set(green_mask));
      g_2_pix_alloc[i + 256] <<= free_bits_at_bottom(green_mask);
      b_2_pix_alloc[i + 256] = i >> (8 - number_of_bits_set(blue_mask));
      b_2_pix_alloc[i + 256] <<= free_bits_at_bottom(blue_mask);
      /*
       * If we have 16-bit output depth, then we double the value
       * in the top word. This means that we can write out both
       * pixels in the pixel doubling mode with one op. It is 
       * harmless in the normal case as storing a 32-bit value
       * through a short pointer will lose the top bits anyway.
       * A similar optimisation for Alpha for 64 bit has been
       * prepared for, but is not yet implemented.
       */
      if(!thirty2) {

	r_2_pix_alloc[i + 256] |= (r_2_pix_alloc[i + 256]) << 16;
	g_2_pix_alloc[i + 256] |= (g_2_pix_alloc[i + 256]) << 16;
	b_2_pix_alloc[i + 256] |= (b_2_pix_alloc[i + 256]) << 16;

      }
#ifdef SIXTYFOUR_BIT
      if(thirty2) {

	r_2_pix_alloc[i + 256] |= (r_2_pix_alloc[i + 256]) << 32;
	g_2_pix_alloc[i + 256] |= (g_2_pix_alloc[i + 256]) << 32;
	b_2_pix_alloc[i + 256] |= (b_2_pix_alloc[i + 256]) << 32;

      }
#endif
    }

    /*
     * Spread out the values we have to the rest of the array so that
     * we do not need to check for overflow.
     */
    for (i = 0; i < 256; i++) {
      r_2_pix_alloc[i] = r_2_pix_alloc[256];
      r_2_pix_alloc[i+ 512] = r_2_pix_alloc[511];
      g_2_pix_alloc[i] = g_2_pix_alloc[256];
      g_2_pix_alloc[i+ 512] = g_2_pix_alloc[511];
      b_2_pix_alloc[i] = b_2_pix_alloc[256];
      b_2_pix_alloc[i+ 512] = b_2_pix_alloc[511];
    }

    r_2_pix = r_2_pix_alloc + 256;
    g_2_pix = g_2_pix_alloc + 256;
    b_2_pix = b_2_pix_alloc + 256;

}


/*
 *--------------------------------------------------------------
 *
 * Color16DitherImage --
 *
 *	Converts image into 16 bit color.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

void
Color16DitherImage(lum, cr, cb, out, rows, cols)
  unsigned char *lum;
  unsigned char *cr;
  unsigned char *cb;
  unsigned char *out;
  int cols, rows;

{
    int L, CR, CB;
    unsigned short *row1, *row2;
    unsigned char *lum2;
    int x, y;
    unsigned int r, b, g;
    int cr_r;
    int cr_g;
    int cb_g;
    int cb_b;
    int cols_2 = cols/2;

    row1 = (unsigned short *)out;
    row2 = row1 + cols_2 + cols_2;
    lum2 = lum + cols_2 + cols_2;

    for (y=0; y<rows; y+=2) {
	for (x=0; x<cols_2; x++) {
	    int R, G, B;

	    CR = *cr++;
	    CB = *cb++;
	    cr_r = Cr_r_tab[CR];
	    cr_g = Cr_g_tab[CR];
	    cb_g = Cb_g_tab[CB];
	    cb_b = Cb_b_tab[CB];

            L = L_tab[(int) *lum++];

	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    *row1++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);

#ifdef INTERPOLATE
            if(x != cols_2 - 1) {
	      CR = (CR + *cr) >> 1;
	      CB = (CB + *cb) >> 1;
	      cr_r = Cr_r_tab[CR];
	      cr_g = Cr_g_tab[CR];
	      cb_g = Cb_g_tab[CB];
	      cb_b = Cb_b_tab[CB];
            }
#endif

            L = L_tab[(int) *lum++];

	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    *row1++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);

	    /*
	     * Now, do second row.
	     */
#ifdef INTERPOLATE
            if(y != rows - 2) {
	      CR = (CR + *(cr + cols_2 - 1)) >> 1;
	      CB = (CB + *(cb + cols_2 - 1)) >> 1;
	      cr_r = Cr_r_tab[CR];
	      cr_g = Cr_g_tab[CR];
	      cb_g = Cb_g_tab[CB];
	      cb_b = Cb_b_tab[CB];
            }
#endif

	    L = L_tab[(int) *lum2++];
	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    *row2++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);

	    L = L_tab[(int) *lum2++];
	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    *row2++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);
	}
        /*
         * These values are at the start of the next line, (due
         * to the ++'s above),but they need to be at the start
         * of the line after that.
         */
	lum += cols_2 + cols_2;
	lum2 += cols_2 + cols_2;
	row1 += cols_2 + cols_2;
	row2 += cols_2 + cols_2;
    }
}




/*
 *--------------------------------------------------------------
 *
 * Color32DitherImage --
 *
 *	Converts image into 32 bit color (or 24-bit non-packed).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

/*
 * This is a copysoft version of the function above with ints instead
 * of shorts to cause a 4-byte pixel size
 */

void
Color32DitherImage(lum, cr, cb, out, rows, cols)
  unsigned char *lum;
  unsigned char *cr;
  unsigned char *cb;
  unsigned char *out;
  int cols, rows;

{
    int L, CR, CB;
    unsigned int *row1, *row2;
    unsigned char *lum2;
    int x, y;
    unsigned int r, b, g;
    int cr_r;
    int cr_g;
    int cb_g;
    int cb_b;
    int cols_2 = cols / 2;

    row1 = (unsigned int *)out;
    row2 = row1 + cols_2 + cols_2;
    lum2 = lum + cols_2 + cols_2;
    for (y=0; y<rows; y+=2) {
	for (x=0; x<cols_2; x++) {
	    int R, G, B;

	    CR = *cr++;
	    CB = *cb++;
	    cr_r = Cr_r_tab[CR];
	    cr_g = Cr_g_tab[CR];
	    cb_g = Cb_g_tab[CB];
	    cb_b = Cb_b_tab[CB];

            L = L_tab[(int) *lum++];

	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    *row1++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);

#ifdef INTERPOLATE
            if(x != cols_2 - 1) {
	      CR = (CR + *cr) >> 1;
	      CB = (CB + *cb) >> 1;
	      cr_r = Cr_r_tab[CR];
	      cr_g = Cr_g_tab[CR];
	      cb_g = Cb_g_tab[CB];
	      cb_b = Cb_b_tab[CB];
            }
#endif

            L = L_tab[(int) *lum++];

	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    *row1++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);

	    /*
	     * Now, do second row.
	     */

#ifdef INTERPOLATE
            if(y != rows - 2) {
	      CR = (CR + *(cr + cols_2 - 1)) >> 1;
	      CB = (CB + *(cb + cols_2 - 1)) >> 1;
	      cr_r = Cr_r_tab[CR];
	      cr_g = Cr_g_tab[CR];
	      cb_g = Cb_g_tab[CB];
	      cb_b = Cb_b_tab[CB];
            }
#endif

	    L = L_tab [(int) *lum2++];
	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    *row2++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);

	    L = L_tab [(int) *lum2++];
	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    *row2++ = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);
	}
	lum += cols_2 + cols_2;
	lum2 += cols_2 + cols_2;
	row1 += cols_2 + cols_2;
	row2 += cols_2 + cols_2;
    }
}

/*
 * Erik Corry's pixel doubling routines for 15/16/24/32 bit screens.
 */



/*
 *--------------------------------------------------------------
 *
 * Twox2Color16DitherImage --
 *
 *	Converts image into 16 bit color at double size.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

/*
 * In this function I make use of a nasty trick. The tables have the lower
 * 16 bits replicated in the upper 16. This means I can write ints and get
 * the horisontal doubling for free (almost).
 */

void
Twox2Color16DitherImage(lum, cr, cb, out, rows, cols)
  unsigned char *lum;
  unsigned char *cr;
  unsigned char *cb;
  unsigned char *out;
  int cols, rows;

{
    int L, CR, CB;
    unsigned int *row1 = (unsigned int *)out;
    unsigned int *row2 = row1 + cols;
    unsigned int *row3 = row2 + cols;
    unsigned int *row4 = row3 + cols;
    unsigned char *lum2;
    int x, y;
    unsigned int r, b, g;
    int cr_r;
    int cr_g;
    int cb_g;
    int cb_b;
    int cols_2 = cols/2;

    lum2 = lum + cols_2 + cols_2;
    for (y=0; y<rows; y+=2) {
	for (x=0; x<cols_2; x++) {
	    int R, G, B;
            int t;

	    CR = *cr++;
	    CB = *cb++;
	    cr_r = Cr_r_tab[CR];
	    cr_g = Cr_g_tab[CR];
	    cb_g = Cb_g_tab[CB];
	    cb_b = Cb_b_tab[CB];

            L = L_tab[(int) *lum++];

	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    t = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);
	    row1[0] = t;
	    row1++;
	    row2[0] = t;
	    row2++;

#ifdef INTERPOLATE
            if(x != cols_2 - 1) {
	      CR = (CR + *cr) >> 1;
	      CB = (CB + *cb) >> 1;
	      cr_r = Cr_r_tab[CR];
	      cr_g = Cr_g_tab[CR];
	      cb_g = Cb_g_tab[CB];
	      cb_b = Cb_b_tab[CB];
            }
#endif
            L = L_tab[(int) *lum++];

	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    t = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);
	    row1[0] = t;
	    row1++;
	    row2[0] = t;
	    row2++;

	    /*
	     * Now, do second row.
	     */
#ifdef INTERPOLATE
            if(y != rows - 2) {
	      CR = (CR + *(cr + cols_2 - 1)) >> 1;
	      CB = (CB + *(cb + cols_2 - 1)) >> 1;
	      cr_r = Cr_r_tab[CR];
	      cr_g = Cr_g_tab[CR];
	      cb_g = Cb_g_tab[CB];
	      cb_b = Cb_b_tab[CB];
            }
#endif
	    L = L_tab[(int) *lum2++];
	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    t = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);
	    row3[0] = t;
	    row3++;
	    row4[0] = t;
	    row4++;

	    L = L_tab[(int) *lum2++];
	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    t = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);
	    row3[0] = t;
	    row3++;
	    row4[0] = t;
	    row4++;
	}
	lum += cols_2 + cols_2;
	lum2 += cols_2 + cols_2;
	row1 += 6 * cols_2;
	row3 += 6 * cols_2;
	row2 += 6 * cols_2;
	row4 += 6 * cols_2;
    }
}




/*
 *--------------------------------------------------------------
 *
 * Twox2Color32 --
 *
 *	Converts image into 24/32 bit color.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

#ifdef SIXTYFOUR_BIT
#define ONE_TWO 1
#else
#define ONE_TWO 2
#endif

void
Twox2Color32DitherImage(lum, cr, cb, out, rows, cols)
  unsigned char *lum;
  unsigned char *cr;
  unsigned char *cb;
  unsigned char *out;
  int cols, rows;

{
    int L, CR, CB;
    unsigned long *row1 = (unsigned long *)out;
    unsigned long *row2 = row1 + cols * ONE_TWO;
    unsigned long *row3 = row2 + cols * ONE_TWO;
    unsigned long *row4 = row3 + cols * ONE_TWO;
    unsigned char *lum2;
    int x, y;
    unsigned int r, b, g;
    int cr_r;
    int cr_g;
    int cb_g;
    int cb_b;
    int cols_2 = cols/2;

    lum2 = lum + cols_2 + cols_2;
    for (y=0; y<rows; y+=2) {
	for (x=0; x<cols_2; x++) {
	    int R, G, B;
            long t;

	    CR = *cr++;
	    CB = *cb++;
	    cr_r = Cr_r_tab[CR];
	    cr_g = Cr_g_tab[CR];
	    cb_g = Cb_g_tab[CB];
	    cb_b = Cb_b_tab[CB];

            L = L_tab[ (int) *lum++];

	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    t = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);
	    row1[0] = t;
	    row2[0] = t;
#ifndef SIXTYFOUR_BIT
	    row1[1] = t;
	    row2[1] = t;
#endif
	    row1 += ONE_TWO;
	    row2 += ONE_TWO;

#ifdef INTERPOLATE
            if(x != cols_2 - 1) {
	      CR = (CR + *cr) >> 1;
	      CB = (CB + *cb) >> 1;
	      cr_r = Cr_r_tab[CR];
	      cr_g = Cr_g_tab[CR];
	      cb_g = Cb_g_tab[CB];
	      cb_b = Cb_b_tab[CB];
            }
#endif
            L = L_tab[ (int) *lum++];

	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    t = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);
	    row1[0] = t;
	    row2[0] = t;
#ifndef SIXTYFOUR_BIT
	    row1[1] = t;
	    row2[1] = t;
#endif
	    row1 += ONE_TWO;
	    row2 += ONE_TWO;

	    /*
	     * Now, do second row.
	     */
#ifdef INTERPOLATE
            if(y != rows - 2) {
	      CR = (CR + *(cr + cols_2 - 1)) >> 1;
	      CB = (CB + *(cb + cols_2 - 1)) >> 1;
	      cr_r = Cr_r_tab[CR];
	      cr_g = Cr_g_tab[CR];
	      cb_g = Cb_g_tab[CB];
	      cb_b = Cb_b_tab[CB];
            }
#endif
	    L = L_tab[ (int) *lum2++];
	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    t = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);
	    row3[0] = t;
	    row4[0] = t;
#ifndef SIXTYFOUR_BIT
	    row3[1] = t;
	    row4[1] = t;
#endif
	    row3 += ONE_TWO;
	    row4 += ONE_TWO;

	    L = L_tab[(int) *lum2++];
	    R = L + cr_r;
	    G = L + cr_g + cb_g;
	    B = L + cb_b;

	    t = (r_2_pix[R] | g_2_pix[G] | b_2_pix[B]);
	    row3[0] = t;
	    row4[0] = t;
#ifndef SIXTYFOUR_BIT
	    row3[1] = t;
	    row4[1] = t;
#endif
	    row3 += ONE_TWO;
	    row4 += ONE_TWO;
	}
	lum += cols_2 + cols_2;
	lum2 += cols_2 + cols_2;

	row1 += ONE_TWO * 6 *cols_2;
	row3 += ONE_TWO * 6 *cols_2;
	row2 += ONE_TWO * 6 *cols_2;
	row4 += ONE_TWO * 6 *cols_2;
    }
}

