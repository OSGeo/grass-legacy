/*
 * 24bit.c --
 *
 *      This file implements conversions from images to 24 bit color.
 *
 */

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

#include "video.h"
#include "dither.h"
#include "proto.h"

BOOLEAN rgb = TRUE;

/*
 *    R = L + 1.366*Cr;
 *    G = L - 0.700*Cb - 0.334*Cr;
 *    B = L + 1.732*Cb;
 *
 * We'll use fixed point by adding two extra bits after the decimal.
 * We also don't bother doing the -.002*Cb for R or the -.006*Cr for B because
 * they create a difference of at most 1, virtually indistinguishable
 */

#define BITS    8
#define ONE     ((int) 1)
#define CONST_SCALE    (ONE << BITS)
#define ROUND_FACTOR    (ONE << (BITS-1))

/* Macro to convert integer to fixed. */
#define UP(x)    (((int)(x)) << BITS)

/* Macro to convert fixed to integer (with rounding). */
#define DOWN(x)    (((x) + ROUND_FACTOR) >> BITS)

/* Macro to convert a float to a fixed */
#define FIX(x)  ((int) ((x)*CONST_SCALE + 0.5))
#define CLAMP(ll,x,ul)    ( ((x)<(ll)) ?(ll):( ((x)>(ul)) ?(ul):(x)))
/* Those above arent too fast, here is a user-supplied alternative: */
#define UP_255 UP(255)
#define DO_ASSIGN(row,C1,C2,C3)                                       \
         if ((unsigned) C1 < (unsigned) UP_255 &&                     \
             (unsigned) C2 < (unsigned) UP_255 &&                     \
             (unsigned) C3 < (unsigned) UP_255)                       \
           /* Fast common case (no overflow). */                      \
           *row++ = (C1 >> BITS) + (C2 & 0xff00) + ((C3 & 0xff00) <<BITS);\
         else {                                                       \
           /* Overflow in C3, C2 or C1. */                            \
          v = 0;                                                      \
            if (C1 >= 0)                                              \
             if (C1 > UP_255)                                         \
               v = 255;                                               \
             else                                                     \
                 v = C1 >> BITS;                                      \
           if (C2 >= 0)                                               \
             if (C2 > UP_255)                                         \
               v += 255 << BITS;                                      \
             else                                                     \
                 v += C2 & 0xff00;                                    \
           if (C3 >= 0)                                               \
             if (C3 > UP_255)                                         \
               v += 255 << (BITS + BITS);                             \
             else                                                     \
                 v += (C3 & 0xff00) << BITS;                          \
           *row++ = v;}        
/* This is dumb, but a little faster than anything else I've thought up */
#define GET_ASSIGN_VAL(C1,C2,C3)                                      \
  if ((unsigned) C1 < (unsigned) UP_255 &&                            \
      (unsigned) C2 < (unsigned) UP_255 &&                            \
             (unsigned) C3 < (unsigned) UP_255)                       \
  /* Fast common case (no overflow). */                               \
  v = (C1 >> BITS) + (C2 & 0xff00) + ((C3 & 0xff00) <<BITS);          \
  else {  /* Overflow in C3, C2 or C1. */                             \
    v = 0;                                                            \
    if (C1 >= 0)                                                      \
       if (C1 > UP_255)                                               \
	 v = 255;                                                     \
       else                                                           \
	 v = C1 >> BITS;                                              \
    if (C2 >= 0)                                                      \
       if (C2 > UP_255)                                               \
	 v += 255 << BITS;                                            \
       else                                                           \
	 v += C2 & 0xff00;                                            \
    if (C3 >= 0)                                                      \
       if (C3 > UP_255)                                               \
	 v += 255 << (BITS + BITS);                                   \
       else                                                           \
	 v += (C3 & 0xff00) << BITS;                                  \
	 }        

static int *Cr_r_tab, *Cr_g_tab, *Cb_g_tab, *Cb_b_tab;

/*
 *--------------------------------------------------------------
 *
 * InitColorDither --
 *
 *    To get rid of the multiply and other conversions in color
 *    dither, we use a lookup table.
 *
 * Results:
 *    None.
 *
 * Side effects:
 *    The lookup tables are initialized.
 *
 *    R = L + 1.366*Cr;
 *    G = L - 0.700*Cb - 0.334*Cr;
 *    B = L + 1.732*Cb;
 * 
 *    Equations streamlined due to imperceptable difference in output.
 *
 * We'll use fixed point by adding two extra bits after the decimal.
 *
 *--------------------------------------------------------------
 */

void
  InitColorDither()
{
  int CR, CB, i;

  Cb_b_tab = (int *)malloc(256*sizeof(int));
  Cr_g_tab = (int *)malloc(256*sizeof(int));
  Cb_g_tab = (int *)malloc(256*sizeof(int));
  Cr_r_tab = (int *)malloc(256*sizeof(int));

  for (i=0; i<256; i++) {
    CB = CR = i;

    CB -= 128; CR -= 128;

    Cr_r_tab[i] = FIX(1.366) * CR;
    Cr_g_tab[i] = -FIX(0.700) * CR;
    Cb_g_tab[i] = -FIX(0.334) * CB;   
 
    Cb_b_tab[i] = FIX(1.732) * CB;
  }
}


/*
 *--------------------------------------------------------------
 *
 * ColorDitherImage --
 *
 *    Converts image into 24 bit color.
 *
 * Results:
 *    None.
 *
 * Side effects:
 *    None.
 *
 *--------------------------------------------------------------
 */

void
  ColorDitherImage(lum, cr, cb, out, rows, cols)
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
  int cr_r, cr_g, cb_g, cb_b;
  int R, G, B;
  unsigned int v;		/* used in DO_ASSIGN */

  row1 = (unsigned int *)out;
  row2 = row1 + cols;
  lum2 = lum + cols;

  /* Code in these two halves of the if should be identical except for the DO_ASSIGN */
  if (!rgb) {
    for (y=0;  y<rows;  y+=2) {
      for (x=0;  x<cols;  x+=2) {
	CR = *cr++; 
	CB = *cb++;
	cr_r = Cr_r_tab[CR];
	cr_g = Cr_g_tab[CR];
	cb_g = Cb_g_tab[CB];
	cb_b = Cb_b_tab[CB];
        
	L = *lum++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
if (R<0 || G<0 || B<0) {
  printf("%d %d %d\n", R, G, B);
}
	DO_ASSIGN(row1,R,G,B);

	if (x != cols - 1) {
	  CR = (CR + *cr) >> 1;
	  CB = (CB + *cb) >> 1;
	  cr_r = Cr_r_tab[CR];
	  cr_g = Cr_g_tab[CR];
	  cb_g = Cb_g_tab[CB];
	  cb_b = Cb_b_tab[CB];
	}

	L = *lum++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
	DO_ASSIGN(row1,R,G,B);

	/*
	 * Now, do second row.
	 */

	if (y != rows - 1) {
	  CR = (CR + *(cr - 1 + cols/2)) >> 1;
	  CB = (CB + *(cb - 1 + cols/2)) >> 1;
	  cr_r = Cr_r_tab[CR];
	  cr_g = Cr_g_tab[CR];
	  cb_g = Cb_g_tab[CB];
	  cb_b = Cb_b_tab[CB];
	}

	L = *lum2++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
	DO_ASSIGN(row2,R,G,B);

	L = *lum2++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
	DO_ASSIGN(row2,R,G,B);
      }
      lum += cols;
      lum2 += cols;
      row1 += cols;
      row2 += cols;
    }
  } else {
    for (y=0;  y<rows;  y+=2) {
      for (x=0;  x<cols;  x+=2) {
	CR = *cr++; 
	CB = *cb++;
	cr_r = Cr_r_tab[CR];
	cr_g = Cr_g_tab[CR];
	cb_g = Cb_g_tab[CB];
	cb_b = Cb_b_tab[CB];

	L = *lum++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
	DO_ASSIGN(row1,B,G,R);

	if (x != cols - 1) {
	  CR = (CR + *cr) >> 1;
	  CB = (CB + *cb) >> 1;
	  cr_r = Cr_r_tab[CR];
	  cr_g = Cr_g_tab[CR];
	  cb_g = Cb_g_tab[CB];
	  cb_b = Cb_b_tab[CB];
	}

	L = *lum++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
	DO_ASSIGN(row1,B,G,R);
	/*
	 * Now, do second row.
	 */

	if (y != rows - 1) {
	  CR = (CR + *(cr - 1 + cols/2)) >> 1;
	  CB = (CB + *(cb - 1 + cols/2)) >> 1;
	  cr_r = Cr_r_tab[CR];
	  cr_g = Cr_g_tab[CR];
	  cb_g = Cb_g_tab[CB];
	  cb_b = Cb_b_tab[CB];
	}

	L = *lum2++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
	DO_ASSIGN(row2,B,G,R);

	L = *lum2++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
	DO_ASSIGN(row2,B,G,R);
      }
      lum += cols;
      lum2 += cols;
      row1 += cols;
      row2 += cols;
    }
  }
  
}




/*
 *--------------------------------------------------------------
 *
 * Color2DitherImage --
 *
 *    Converts image into 24 bit color.
 *
 * Results:
 *    None.
 *
 * Side effects:
 *    None.
 *
 *--------------------------------------------------------------
 */

void
  Color2DitherImage(lum, cr, cb, out, rows, cols)
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
  unsigned int v		/* set in DO_ASSIGN */;
  int cr_r;
  int cr_g;
  int cb_g;
  int cb_b;
  
  /* In this version row1 and row2 each point to 2 rows. */
  /* Also, each row is twice as long. */
  row1 = (unsigned int *)out;
  row2 = row1 + cols*4;
  lum2 = lum + cols;

  /* the following code should be the same in each branch, except for 
   * GET_ASSIGN_VAL */
  if (!rgb) {
    for (y = 0;  y < rows;  y += 2) {
      for (x = 0;  x < cols;  x += 2) {
	int R, G, B;
      
	CR = *cr++;
	CB = *cb++;
	cr_r = Cr_r_tab[CR];
	cr_g = Cr_g_tab[CR];
	cb_g = Cb_g_tab[CB];
	cb_b = Cb_b_tab[CB];
      
	L = *lum++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
	GET_ASSIGN_VAL(R,G,B);
	*row1 = v;		/* v set in GET_ASSIGN_VAL */
	*(row1+1) = v;
	*(row1+cols*2) = v;
	*(row1+cols*2+1) = v;
	row1+=2;
      
	if (x != cols - 2) {
	  CR = (CR + *cr) >> 1;
	  CB = (CB + *cb) >> 1;
	  cr_r = Cr_r_tab[CR];
	  cr_g = Cr_g_tab[CR];
	  cb_g = Cb_g_tab[CB];
	  cb_b = Cb_b_tab[CB];
	}
	L = *lum++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
	GET_ASSIGN_VAL(R,G,B);
	*row1 = v;
	*(row1+1) = v;
	*(row1+cols*2) = v;
	*(row1+cols*2+1) = v;
	row1+=2;
      
	/*
	 * Now, do second row.
	 */
	if (y != rows - 2) {
	  CR = (CR + *(cr - 1 + cols/2)) >> 1;
	  CB = (CB + *(cb - 1 + cols/2)) >> 1;
	  cr_r = Cr_r_tab[CR];
	  cr_g = Cr_g_tab[CR];
	  cb_g = Cb_g_tab[CB];
	  cb_b = Cb_b_tab[CB];
	}
	L = *lum2++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
	GET_ASSIGN_VAL(R,G,B);
	*row2 = v;
	*(row2+1) = v;
	*(row2+cols*2) = v;
	*(row2+cols*2+1) = v;
	row2+=2;
      
	L = *lum2++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
	GET_ASSIGN_VAL(R,G,B);
	*row2 = v;
	*(row2+1) = v;
	*(row2+cols*2) = v;
	*(row2+cols*2+1) = v;
	row2+=2;
      }
      lum += cols;
      lum2 += cols;
      row1 += cols*6;
      row2 += cols*6;
    }
  } else {
    for (y=0; y<rows; y+=2) {
      for (x=0; x<cols; x+=2) {
	int R, G, B;
      
	CR = *cr++;
	CB = *cb++;
	cr_r = Cr_r_tab[CR];
	cr_g = Cr_g_tab[CR];
	cb_g = Cb_g_tab[CB];
	cb_b = Cb_b_tab[CB];
 
	L = *lum++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
	GET_ASSIGN_VAL(B,G,R);
	*row1 = v;		/* v set in GET_ASSIGN_VAL */
	*(row1+1) = v;
	*(row1+cols*2) = v;
	*(row1+cols*2+1) = v;
	row1+=2;
 
	if (x != cols - 2) {
	  CR = (CR + *cr) >> 1;
	  CB = (CB + *cb) >> 1;
	  cr_r = Cr_r_tab[CR];
	  cr_g = Cr_g_tab[CR];
	  cb_g = Cb_g_tab[CB];
	  cb_b = Cb_b_tab[CB];
	}
	L = *lum++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
	GET_ASSIGN_VAL(B,G,R);
	*row1 = v;
	*(row1+1) = v;
	*(row1+cols*2) = v;
	*(row1+cols*2+1) = v;
	row1+=2;
      
	/*
	 * Now, do second row.
	 */
	if (y != rows - 2) {
	  CR = (CR + *(cr - 1 + cols/2)) >> 1;
	  CB = (CB + *(cb - 1 + cols/2)) >> 1;
	  cr_r = Cr_r_tab[CR];
	  cr_g = Cr_g_tab[CR];
	  cb_g = Cb_g_tab[CB];
	  cb_b = Cb_b_tab[CB];
	}
	L = *lum2++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
	GET_ASSIGN_VAL(B,G,R);
	*row2 = v;
	*(row2+1) = v;
	*(row2+cols*2) = v;
	*(row2+cols*2+1) = v;
	row2+=2;
      
	L = *lum2++;
	L = UP(L);
	R = L + cr_r;
	G = L + cr_g + cb_g;
	B = L + cb_b;
	GET_ASSIGN_VAL(B,G,R);
	*row2 = v;
	*(row2+1) = v;
	*(row2+cols*2) = v;
	*(row2+cols*2+1) = v;
	row2+=2;
      }
      lum += cols;
      lum2 += cols;
      row1 += cols*6;
      row2 += cols*6;
    }
  }
}
