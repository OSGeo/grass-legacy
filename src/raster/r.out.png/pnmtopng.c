/*
** pnmtopng.c -
** read a portable anymap and produce a Portable Network Graphics file
**
** derived from pnmtorast.c (c) 1990,1991 by Jef Poskanzer and some
** parts derived from ppmtogif.c by Marcel Wijkstra <wijkstra@fwi.uva.nl>
** thanks to Greg Roelofs <newt@pobox.com> for contributions and bug-fixes
**
** Copyright (C) 1995-1998 by Alexander Lehmann <alex@hal.rhein-main.de>
**                        and Willem van Schaik <willem@schaik.com>
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

/* GRR 19991203:  incorporated fix by Rafal Rzeczkowski <rzeczkrg@mcmaster.ca>:
 *  gray images with exactly 16 shades were being to 8-bit grayscale rather
 *  than 4-bit palette due to misuse of the pm_maxvaltobits() function.  Also
 *  moved VERSION to new version.h header file. */

/* GRR 19990713:  fixed redundant freeing of png_ptr and info_ptr in setjmp()
 *  blocks and added png_destroy_write_struct() and pm_close(ifp) in each
 *  pm_error() block.  */

/* GRR 19990308:  declared "clobberable" automatic variables in convertpnm()
 *  static to fix Solaris/gcc stack-corruption bug.  Also installed custom
 *  error-handler to avoid jmp_buf size-related problems (i.e., jmp_buf
 *  compiled with one size in libpng and another size here).  */

/* GRR 19980621:  moved some if-tests out of full-image loops; added fix for
 *  following problem discovered by Magnus Holmgren and debugged by Glenn:
 *
 *    The pnm has three colors:  0 0 0, 204 204 204, and 255 255 255.
 *    These were apparently scaled to 0 0 0, 12 12 12, and 15 15 15.
 *    That would have been OK if the file had been written as color-type
 *    0 (grayscale), but it was written as an indexed-color file, in
 *    which the colors from the pnm file should have been used rather
 *    than the scaled colors.  What appears in the PLTE chunk is
 *    0 0 0, 12 12 12, and 15 15 15, which of course results in a
 *    very dark image.
 *
 *  (temporarily ifdef'd for easier inspection and before/after testing)
 */
#define GRR_GRAY_PALETTE_FIX

#include "pnm.h"
#include "png.h"	/* includes zlib.h and setjmp.h */
#include "version.h"	/* VERSION macro */

#include "ppmcmap.h"

typedef struct _jmpbuf_wrapper {
  jmp_buf jmpbuf;
} jmpbuf_wrapper;

/* GRR 19991205:  this is used as a test for pre-1999 versions of netpbm and
 *   pbmplus vs. 1999 or later (in which pm_close was split into two) 
 */
#ifdef PBMPLUS_RAWBITS
#  define pm_closer pm_close
#  define pm_closew pm_close
#endif

#ifndef TRUE
#  define TRUE 1
#endif
#ifndef FALSE
#  define FALSE 0
#endif
#ifndef NONE
#  define NONE 0
#endif
#define MAXCOLORS 256
#define MAXCOMMENTS 256

/* function prototypes */
#ifdef __STDC__
static int closestcolor (pixel color, colorhist_vector chv, int colors,
                         xelval maxval);
static void read_text (png_info *info_ptr, FILE *tfp);
static void pnmtopng_error_handler (png_structp png_ptr, png_const_charp msg);
static void convertpnm (FILE *ifp, FILE *afp, FILE *tfp);
int main (int argc, char *argv[]);
#endif

static int verbose = FALSE;
static int interlace = FALSE;
static int downscale = FALSE;
static int transparent = -1;
static char *transstring;
static int alpha = FALSE;
static char *alpha_file;
static int background = -1;
static char *backstring;
static float gamma = -1.0;
static int hist = FALSE;
static float chroma_wx = -1.0;
static float chroma_wy = -1.0;
static float chroma_rx = -1.0;
static float chroma_ry = -1.0;
static float chroma_gx = -1.0;
static float chroma_gy = -1.0;
static float chroma_bx = -1.0;
static float chroma_by = -1.0;
static int phys_x = -1.0;
static int phys_y = -1.0;
static int phys_unit = -1.0;
static int text = FALSE;
static int ztxt = FALSE;
static char *text_file;
static int mtime = FALSE;
static char *date_string;
static char *time_string;
static struct tm time_struct;
static int filter = -1;
static int compression = -1;
static int force = FALSE;
static jmpbuf_wrapper pnmtopng_jmpbuf_struct;

#ifdef __STDC__
static int closestcolor (pixel color, colorhist_vector chv, int colors, xelval maxval)
#else
static int closestcolor (color, chv, colors, maxval)
pixel color;
colorhist_vector chv;
int colors;
xelval maxval;
#endif
{
  int i, r, g, b, d;
  int imin, dmin;

  r = (int)PPM_GETR (color) * 255 / maxval;
  g = (int)PPM_GETG (color) * 255 / maxval;
  b = (int)PPM_GETB (color) * 255 / maxval;

  imin = 0;
  dmin = 1000000;
  for (i = 0 ; i < colors ; i++) {
    d = (r - PPM_GETR (chv[i].color)) * (r - PPM_GETR (chv[i].color))+
        (g - PPM_GETG (chv[i].color)) * (g - PPM_GETG (chv[i].color))+
        (b - PPM_GETB (chv[i].color)) * (b - PPM_GETB (chv[i].color));
    if (d < dmin) {
      dmin = d;
      imin = i;
    }
  }
  return imin;
}

#ifdef __STDC__
static void read_text (png_info *info_ptr, FILE *tfp)
#else
static void read_text (info_ptr, tfp)
png_info *info_ptr;
FILE *tfp;
#endif
{
  char textline[256];
  int textpos;
  int i, j;
  int c;
  char *cp;

  /* GRR:  need to check for malloc failure here */
  info_ptr->text = (png_text *)malloc (MAXCOMMENTS * sizeof (png_text));
  j = 0;
  textpos = 0;
  while ((c = getc (tfp)) != EOF) {
    if (c != '\n' && c != EOF) {
      textline[textpos++] = c;
    } else {
      textline[textpos++] = '\0';
      if ((textline[0] != ' ') && (textline[0] != '\t')) {
        /* the following is a not that accurate check on Author or Title */
        if ((!ztxt) || (textline[0] == 'A') || (textline[0] == 'T'))
          info_ptr->text[j].compression = -1;
        else
          info_ptr->text[j].compression = 0;
        cp = malloc (textpos);
        info_ptr->text[j].key = cp;
        i = 0;
        if (textline[0] == '"') {
          i++;
          while (textline[i] != '"' && textline[i] != '\n')
            *(cp++) = textline[i++];
          i++;
        } else {
          while (textline[i] != ' ' && textline[i] != '\t' && textline[i] != '\n')
            *(cp++) = textline[i++];
        }
        *(cp++) = '\0';
        cp = malloc (textpos);
        info_ptr->text[j].text = cp;
        while (textline[i] == ' ' || textline[i] == '\t')
          i++;
        strcpy (cp, &textline[i]);
        info_ptr->text[j].text_length = strlen (cp);
        j++;
      } else {
        j--;
        cp = malloc (info_ptr->text[j].text_length + textpos);
        strcpy (cp, info_ptr->text[j].text);
        strcat (cp, "\n");
        info_ptr->text[j].text = cp;
        i = 0;
        while (textline[i] == ' ' || textline[i] == '\t')
        i++;
        strcat (cp, &textline[i]);
        info_ptr->text[j].text_length = strlen (cp);
        j++;
      }
      textpos = 0;
    }
  } /* end while */
  info_ptr->num_text = j;
}

#ifdef __STDC__
static void pnmtopng_error_handler (png_structp png_ptr, png_const_charp msg)
#else
static void pnmtopng_error_handler (png_ptr, msg)
png_structp png_ptr;
png_const_charp msg;
#endif
{
  jmpbuf_wrapper  *jmpbuf_ptr;

  /* this function, aside from the extra step of retrieving the "error
   * pointer" (below) and the fact that it exists within the application
   * rather than within libpng, is essentially identical to libpng's
   * default error handler.  The second point is critical:  since both
   * setjmp() and longjmp() are called from the same code, they are
   * guaranteed to have compatible notions of how big a jmp_buf is,
   * regardless of whether _BSD_SOURCE or anything else has (or has not)
   * been defined. */

  fprintf(stderr, "pnmtopng:  fatal libpng error: %s\n", msg);
  fflush(stderr);

  jmpbuf_ptr = png_get_error_ptr(png_ptr);
  if (jmpbuf_ptr == NULL) {         /* we are completely hosed now */
    fprintf(stderr,
      "pnmtopng:  EXTREMELY fatal error: jmpbuf unrecoverable; terminating.\n");
    fflush(stderr);
    exit(99);
  }

  longjmp(jmpbuf_ptr->jmpbuf, 1);
}

#ifdef __STDC__
static void convertpnm (FILE *ifp, FILE *afp, FILE *tfp)
#else
static void convertpnm (ifp, afp, tfp)
FILE *ifp;
FILE *afp;
FILE *tfp;
#endif
{
  xel **xels;	/* GRR:  a xel is always a pixel; pixel may be ulg or struct */
  xel p;	/*        (pnm.h)                  (ppm.h)  */
  int rows, cols, format;
  xelval maxval;
  xelval scaleval;
  xelval value;
  pixel transcolor;
  int mayscale;
  pixel backcolor;

  png_struct *png_ptr;
  png_info *info_ptr;

  png_color palette[MAXCOLORS];
  png_byte trans[MAXCOLORS];
  png_uint_16 histogram[MAXCOLORS];
  png_byte *line;
  png_byte *pp;
  int pass;
  int color;
  gray alpha_maxval;
  int alpha_rows;
  int alpha_cols;
  int alpha_trans;
  gray *alphas_of_color[MAXCOLORS];
  int alphas_of_color_cnt[MAXCOLORS];
  int alphas_first_index[MAXCOLORS+1];
  int mapping[MAXCOLORS];
  int colors;
  int fulldepth;
  int x, y;
  int i, j;

  /* these variables are declared static because gcc wasn't kidding
   * about "variable XXX might be clobbered by `longjmp' or `vfork'"
   * (stack corruption observed on Solaris 2.6 with gcc 2.8.1, even
   * in the absence of any other error condition) */
  static int pnm_type;
  static xelval maxmaxval;
  static int sbitval;
  static gray **alpha_mask;
  static int alpha_sbitval;
  static int num_alphas_of_color;
  static int palette_size;
  static colorhist_vector chv;
  static colorhash_table cht;
  static int depth;


  /* these guys are initialized to quiet compiler warnings: */
  maxmaxval = 255;
  num_alphas_of_color = 0;
  alpha_mask = NULL;
  alpha_sbitval = 0;
  palette_size = 0;
  cht = NULL;
  depth = 0;

  png_ptr = png_create_write_struct (PNG_LIBPNG_VER_STRING,
    &pnmtopng_jmpbuf_struct, pnmtopng_error_handler, NULL);
  if (png_ptr == NULL) {
    pm_closer (ifp);
    pm_error ("cannot allocate LIBPNG structure");
  }

  info_ptr = png_create_info_struct (png_ptr);
  if (info_ptr == NULL) {
    png_destroy_write_struct (&png_ptr, (png_infopp)NULL);
    pm_closer (ifp);
    pm_error ("cannot allocate LIBPNG structures");
  }

  if (setjmp (pnmtopng_jmpbuf_struct.jmpbuf)) {
    png_destroy_write_struct (&png_ptr, &info_ptr);
    pm_closer (ifp);
    pm_error ("setjmp returns error condition (1)");
  }

  xels = pnm_readpnm (ifp, &cols, &rows, &maxval, &format);
  pnm_type = PNM_FORMAT_TYPE (format);

  if (verbose) {
    if (pnm_type == PBM_TYPE)    
      pm_message ("reading a PBM file (maxval=%d)", maxval);
    else if (pnm_type == PGM_TYPE)    
      pm_message ("reading a PGM file (maxval=%d)", maxval);
    else if (pnm_type == PPM_TYPE)    
      pm_message ("reading a PPM file (maxval=%d)", maxval);
  }

  if (pnm_type == PGM_TYPE)
    maxmaxval = PGM_MAXMAXVAL;
  else if (pnm_type == PPM_TYPE)
    maxmaxval = PPM_MAXMAXVAL;

  if (transparent > 0)   /* -1 or 1 are the only possibilities so far */
    transcolor = ppm_parsecolor (transstring, maxmaxval);

  if (alpha) {
    alpha_mask = pgm_readpgm (afp, &alpha_cols, &alpha_rows, &alpha_maxval);
    if (alpha_cols != cols || alpha_rows != rows) {
      png_destroy_write_struct (&png_ptr, &info_ptr);
      pm_closer (ifp);
      pm_error ("dimensions for image and alpha mask do not agree");
    }
    /* check if the alpha mask can be represented by a single transparency
       value (i.e. all colors fully opaque except one fully transparent;
       the transparent color may not also occur as fully opaque.
       we have to do this before any scaling occurs, since alpha is only
       possible with 8 and 16-bit */
    /* first find the possible candidate */
    alpha_trans = FALSE;
    for (y = 0 ; y < rows && !alpha_trans ; y++)
      for (x = 0 ; x < cols && !alpha_trans ; x++) {
        if (alpha_mask[y][x] == 0) {
          if (transparent < 0) {
            alpha_trans = TRUE;
            transparent = 2;
            transcolor = xels[y][x];
          }
        }
      }
    /* if alpha_trans is TRUE check the whole image */
    for (y = 0 ; y < rows && alpha_trans ; y++)
      for (x = 0 ; x < cols && alpha_trans ; x++) {
        if (alpha_mask[y][x] == 0) { /* transparent one */
          if (pnm_type == PPM_TYPE) {
            if (!PPM_EQUAL (xels[y][x], transcolor))
              alpha_trans = FALSE;
          } else {
            if (PNM_GET1 (xels[y][x]) != PNM_GET1 (transcolor))
              alpha_trans = FALSE;
          }
        } else /* is it fully opaque ? */
        if (alpha_mask[y][x] != alpha_maxval) {
          alpha_trans = FALSE;
        } else /* does the transparent color also exists fully opaque */
        if (pnm_type == PPM_TYPE) {
          if (PPM_EQUAL (xels[y][x], transcolor))
            alpha_trans = FALSE;
        } else {
          if (PNM_GET1 (xels[y][x]) == PNM_GET1 (transcolor))
            alpha_trans = FALSE;
        }
      }
    if (alpha_trans && !force) {
      if (verbose)
        pm_message ("converting alpha mask to transparency index");
      alpha = FALSE;
    } else {
      transparent = -1;
    }
  }

  /* gcc 2.7.0 -fomit-frame-pointer causes stack corruption here */
  if (background > -1)  /* scale to maxval later: */
    backcolor = ppm_parsecolor (backstring, maxmaxval);

  /* first of all, check if we have a grayscale image written as PPM */

  if (pnm_type == PPM_TYPE && !force) {
    int isgray = TRUE;

    for (y = 0 ; y < rows && isgray ; y++)
      for (x = 0 ; x < cols && isgray ; x++) {
        p = xels[y][x];
        if (PPM_GETR (p) != PPM_GETG (p) || PPM_GETG (p) != PPM_GETB (p))
          isgray = FALSE;
      }
    if (isgray)
      pnm_type = PGM_TYPE;
  }

  /* handle `odd' maxvalues */

  sbitval = 0;
  if (pnm_type != PBM_TYPE || alpha) {
    if (maxval > 65535 && !downscale) {
      png_destroy_write_struct (&png_ptr, &info_ptr);
      pm_closer (ifp);
      pm_error ("can only handle files up to 16-bit (use -downscale to override");
    }

    if (maxval < 65536) {
      sbitval = pm_maxvaltobits (maxval);
      if (maxval != pm_bitstomaxval (sbitval))
        sbitval = 0;
    }

    if (maxval != 255 && maxval != 65535 &&
        (alpha || pnm_type != PGM_TYPE ||
         (maxval != 1 && maxval != 3 && maxval != 15))) {
      if (!alpha && maxval == 7 && pnm_type == PGM_TYPE) {
        if (verbose)
          pm_message ("rescaling to 4-bit");
        scaleval = 15;
      } else
      if (maxval < 255) {
        if (verbose)
          pm_message ("rescaling to 8-bit");
        scaleval = 255;
      } else {
        /* to do:  this can fail; xels aren't big enough - larger pixels &
         * maxvals are needed */
        if (sizeof(scaleval) < 2) {   /* unsigned char by default */
          pm_message ("cannot rescale to 16-bit; rescaling to 8-bit instead (maxval = %d)",
            maxval);
          scaleval = 255;
        } else {
          if (verbose)
            pm_message ("rescaling to 16-bit");
          scaleval = 65535;
        }
      }
      for (y = 0 ; y < rows ; y++)
        for (x = 0 ; x < cols ; x++) {
          p = xels[y][x];
          PPM_DEPTH (xels[y][x], p, maxval, scaleval);
        }
      if (transparent == 2) {   /* "1" case (-transparent) handled below */
        PPM_DEPTH (transcolor, transcolor, maxval, scaleval);
      }
      maxval = scaleval;
    } else {
      sbitval = 0; /* no scaling happened */
    }
  }

  /* now do a real scaling (better than ppm_parsecolor()) */

  if (maxval != 65535) {
    if (background > -1)
      PPM_DEPTH (backcolor, backcolor, maxmaxval, maxval);
    if (transparent == 1)   /* "2" case (-alpha) already done */
      PPM_DEPTH (transcolor, transcolor, maxmaxval, maxval);
  }

  /* check for 16-bit entries that are just scaled 8-bit entries, e.g.,
     when converting an 8-bit palette TIFF to PPM */

  if (pnm_type != PBM_TYPE && maxval == 65535 && !force) {
    mayscale = TRUE;
    if (pnm_type == PGM_TYPE)
      for (y = 0 ; y < rows && mayscale ; y++)
        for (x = 0 ; x < cols && mayscale ; x++) {
          p = xels[y][x];
          if ( (PNM_GET1 (p)&0xff)*0x101 != PNM_GET1 (p) )
            mayscale = FALSE;
        }
    else /* PPM_TYPE */
      for (y = 0 ; y < rows && mayscale ; y++)
        for (x = 0 ; x < cols && mayscale ; x++) {
          p = xels[y][x];
          if ( (PPM_GETR (p)&0xff)*0x101 != PPM_GETR (p) ||
               (PPM_GETG (p)&0xff)*0x101 != PPM_GETG (p) ||
               (PPM_GETB (p)&0xff)*0x101 != PPM_GETB (p) )
            mayscale = FALSE;
        }
    if (mayscale) {
      if (verbose)
        pm_message ("scaling to 8-bit (superfluous 16-bit data)");
      maxval = 255;
      if (pnm_type == PGM_TYPE) {
        for (y = 0 ; y < rows ; y++)
          for (x = 0 ; x < cols ; x++) {
            p = xels[y][x];
            PNM_ASSIGN1 (xels[y][x], PNM_GET1 (p)&0xff);
          }
      } else { /* PPM_TYPE */
        for (y = 0 ; y < rows ; y++)
          for (x = 0 ; x < cols ; x++) {
            p = xels[y][x];
            PPM_ASSIGN (xels[y][x], PPM_GETR (p)&0xff,  PPM_GETG (p)&0xff,
                        PPM_GETB (p)&0xff);
          }
      }
      if (transparent > 0) {
        p = transcolor;
        if (pnm_type == PGM_TYPE) {
          PNM_ASSIGN1 (transcolor, PNM_GET1 (p)&0xff);
        } else {
          PPM_ASSIGN (transcolor, PPM_GETR (p)&0xff,  PPM_GETG (p)&0xff,
                      PPM_GETB (p)&0xff);
        }
      }

      if (sbitval > 0) sbitval >>= 1;
    }
  }

  /* scale alpha mask to match bit depth of image */

  if (alpha) {
    if (alpha_maxval < 65536) {
      alpha_sbitval = pm_maxvaltobits (alpha_maxval);
      if (alpha_maxval != pm_bitstomaxval (alpha_sbitval))
        alpha_sbitval = 0;
    } else
      alpha_sbitval = 0;

    if (alpha_maxval != maxval) {
      if (verbose)
        pm_message ("rescaling alpha mask to match image bit depth");
      for (y = 0 ; y < rows ; y++)
        for (x = 0 ; x < cols ; x++)
          alpha_mask[y][x] = (alpha_mask[y][x] * maxval + alpha_maxval / 2) /
                                                                 alpha_maxval;
      alpha_maxval = maxval;
    } else {
      alpha_sbitval = 0; /* no scaling happened */
    }
  }

  /* now do scaling for grayscale pics of bit-depth 4, 2 and 1, but only
     when we don't have an alpha channel */

  if (!alpha && pnm_type == PGM_TYPE && !force) {
    if (maxval == 255) {
      mayscale = TRUE;
      for (y = 0 ; y < rows && mayscale ; y++)
        for (x = 0 ; x < cols && mayscale ; x++) {
          if ((PNM_GET1 (xels[y][x]) & 0xf) * 0x11 != PNM_GET1 (xels[y][x]))
            mayscale = FALSE;
        }
      if (mayscale) {
        for (y = 0 ; y < rows ; y++)
          for (x = 0 ; x < cols ; x++) {
            PNM_ASSIGN1 (xels[y][x], PNM_GET1 (xels[y][x])&0xf);
          }

        if (transparent > 0) {
          PNM_ASSIGN1 (transcolor, PNM_GET1 (transcolor)&0xf);
        }

        maxval = 15;
        if (verbose)
          pm_message ("scaling to 4-bit (grayscale superfluous data)");
        if (sbitval > 0) sbitval >>= 1;
      }
    }

    if (maxval == 15) {
      mayscale = TRUE;
      for (y = 0 ; y < rows && mayscale ; y++)
        for (x = 0 ; x < cols && mayscale ; x++) {
          if ((PNM_GET1 (xels[y][x])&3) * 5 != PNM_GET1 (xels[y][x]))
            mayscale = FALSE;
        }
      if (mayscale) {
        for (y = 0 ; y < rows ; y++)
          for (x = 0 ; x < cols ; x++) {
            PNM_ASSIGN1 (xels[y][x], PNM_GET1 (xels[y][x]) & 3);
          }
        if (transparent > 0) {
          PNM_ASSIGN1 (transcolor, PNM_GET1 (transcolor) & 3);
        }
        maxval = 3;
        if (verbose)
          pm_message ("scaling to 2-bit (grayscale superfluous data)");
        if (sbitval > 0) sbitval >>= 1;
      }
    }

    if (maxval == 3) {
      mayscale = TRUE;
      for (y = 0 ; y < rows && mayscale ; y++)
        for (x = 0 ; x < cols && mayscale ; x++) {
          if ((PNM_GET1 (xels[y][x])&1) * 3 != PNM_GET1 (xels[y][x]))
            mayscale = FALSE;
        }
      if (mayscale) {
        for (y = 0 ; y < rows ; y++)
          for (x = 0 ; x < cols ; x++) {
            PNM_ASSIGN1 (xels[y][x], PNM_GET1 (xels[y][x])&1);
          }
        if (transparent > 0) {
          PNM_ASSIGN1 (transcolor, PNM_GET1 (transcolor) & 1);
        }
        maxval = 1;
        if (verbose)
          pm_message ("scaling to 1-bit (grayscale superfluous data)");
        sbitval = 0;
      }
    }
  }

  /* 
     We can write a palette file if maxval <= 255 and one of the following is
     true:
       - for ppm files if we have <= 256 colors
       - for alpha if we have <= 256 color/transparency pairs
       - for pgm files (with or without alpha) if the number of bits needed for
         the gray-transparency pairs is smaller than the number of bits needed
         for maxval
     When maxval > 255, we never write a paletted image.
  */

  if ((!force) && (pnm_type != PBM_TYPE) && (maxval <= 255)) {
    if (verbose) {
      pm_message ("maxval is %d", maxval);
      pm_message ("computing colormap...");
    }
    /* for the palette check, we have to copy the grayscale values to the RGB
        channels if we want to apply ppm-functions to grayscale images */
    if (pnm_type == PGM_TYPE) {
      for (y = 0 ; y < rows ; y++)
        for (x = 0 ; x < cols ; x++) {
          value = PNM_GET1 (xels[y][x]);
          PPM_ASSIGN (xels[y][x], value, value, value);
        }
    }
    chv = ppm_computecolorhist (xels, cols, rows, MAXCOLORS, &colors);
    if (verbose) {
      pm_message ("%d colors found", colors);
    }
    /* RR:  note that if colors == 16, maxval == 15 (i.e., range == 0-15) */
    if ((chv == (colorhist_vector) NULL) ||    /* GRR:      v-- was `=' here */
        (pnm_type == PGM_TYPE && pm_maxvaltobits(colors-1) > 
                                (pm_maxvaltobits(maxval) / 2)) ||
        (pnm_type == PPM_TYPE && maxval > 255)) {
      chv = NULL;
      if (verbose) {
        pm_message ("too many colors for writing a colormapped image");
      }      
    }

    if (chv != (colorhist_vector) NULL) {

      if (alpha) {
        /* now check if there are different alpha values for the same color
           and if all pairs still fit into 256 (MAXCOLORS) entries; malloc
           one extra for possible background color */
        cht = ppm_colorhisttocolorhash (chv, colors);
        for (i = 0 ; i < colors + 1 ; i++) {
          if ((alphas_of_color[i] = (gray *)malloc (MAXCOLORS * sizeof (int)))
              == NULL)
          {
            png_destroy_write_struct (&png_ptr, &info_ptr);
            pm_closer (ifp);
            pm_error ("out of memory allocating alpha/palette entries");
          }
          alphas_of_color_cnt[i] = 0;
        }
        num_alphas_of_color = colors + 1;
        for (y = 0 ; y < rows ; y++) {
          for (x = 0 ; x < cols ; x++) {
            color = ppm_lookupcolor (cht, &xels[y][x]);
            for (i = 0 ; i < alphas_of_color_cnt[color] ; i++) {
              if (alpha_mask[y][x] == alphas_of_color[color][i])
                break;
            }
            if (i == alphas_of_color_cnt[color]) {
              alphas_of_color[color][i] = alpha_mask[y][x];
              alphas_of_color_cnt[color]++;
            }
          }
        }
        alphas_first_index[0] = 0;
        for (i = 0 ; i < colors ; i++)
          alphas_first_index[i+1] = alphas_first_index[i] +
                                    alphas_of_color_cnt[i];
        if (verbose)
          pm_message ("number of color/transparency pairs is %d",
            alphas_first_index[colors]);
        if (alphas_first_index[colors] > MAXCOLORS) {
          ppm_freecolorhist (chv);
          chv = NULL;
          if (verbose)
            pm_message (
              "too many color/transparency pairs, writing a non-mapped file");
        }
      } /* if alpha */

      /* add possible background color to palette */
      if (background > -1) {
        cht = ppm_colorhisttocolorhash (chv, colors);
        background = ppm_lookupcolor (cht, &backcolor);
        if (background == -1) {
          if ((!alpha && colors < MAXCOLORS) ||
               (alpha && alphas_first_index[colors] < MAXCOLORS))
          {
            background = colors;
            ppm_addtocolorhist (chv, &colors, MAXCOLORS, &backcolor, colors,
              colors);
            if (alpha) {
              alphas_of_color[background][0] = 255;   /* opaque */
              alphas_of_color_cnt[background] = 1;    /* unique */
              alphas_first_index[colors] = alphas_first_index[background] + 1;
            }
            if (verbose)
              pm_message ("added background color to palette");
          } else {
            background = closestcolor (backcolor, chv, colors, maxval);
            if (verbose)
              pm_message ("no room in palette for background color; using closest match instead");
          }
        }
        ppm_freecolorhash (cht);   /* built again about 110 lines below */
      }

    }
  } else {
    chv = NULL;
  }

  if (chv) {
    if (alpha)
      palette_size = alphas_first_index[colors];
    else
      palette_size = colors;

#ifdef GRR_GRAY_PALETTE_FIX
    if (maxval < 255) {
      if (verbose)
        pm_message ("rescaling palette values to 8-bit");
      for (i = 0 ; i < colors ; i++) {
        p = chv[i].color;
        PPM_DEPTH (chv[i].color, p, maxval, 255);
      }
      /* ...also image pixels so they still match the palette [inefficient!] */
      for (y = 0 ; y < rows ; y++)
        for (x = 0 ; x < cols ; x++) {
          p = xels[y][x];
          PPM_DEPTH (xels[y][x], p, maxval, 255);
        }
      maxval = 255;  /* necessary for transparent case, at least */
    }
#endif

    if (palette_size <= 2)
      depth = 1;
    else if (palette_size <= 4)
      depth = 2;
    else if (palette_size <= 16)
      depth = 4;
    else
      depth = 8;
    fulldepth = depth;
  } else {
    /* non-mapped color or grayscale */

    if (maxval == 65535)
      depth = 16;
    else if (maxval == 255)
      depth = 8;
    else if (maxval == 15)
      depth = 4;
    else if (maxval == 3)
      depth = 2;
    else if (maxval == 1)
      depth = 1;
    else {
      png_destroy_write_struct (&png_ptr, &info_ptr);
      pm_closer (ifp);
      pm_error (" (can't happen) undefined maxval");
    }

    if (alpha) {
      if (pnm_type == PPM_TYPE)
        fulldepth = 4 * depth;
      else
        fulldepth = 2 * depth;
    } else {
      if (pnm_type == PPM_TYPE)
        fulldepth = 3 * depth;
      else
        fulldepth = depth;
    }
  }

  if (verbose)
    pm_message ("writing a%s %d-bit %s%s file%s",
                fulldepth == 8 ? "n" : "", fulldepth,
                chv ? "palette": (pnm_type == PPM_TYPE ? "RGB" : "gray"),
                alpha ? (chv ? "+transparency" : "+alpha") : "",
                interlace? " (interlaced)" : "");

  /* now write the file */

  if (setjmp (pnmtopng_jmpbuf_struct.jmpbuf)) {
    png_destroy_write_struct (&png_ptr, &info_ptr);
    pm_closer (ifp);
    pm_error ("setjmp returns error condition (2)");
  }

#ifdef OLDPNG
  png_write_init (png_ptr);
  png_info_init (info_ptr);
#endif
  png_init_io (png_ptr, stdout);
  info_ptr->width = cols;
  info_ptr->height = rows;
  info_ptr->bit_depth = depth;

  if (chv != NULL)
    info_ptr->color_type = PNG_COLOR_TYPE_PALETTE;
  else if (pnm_type == PPM_TYPE)
    info_ptr->color_type = PNG_COLOR_TYPE_RGB;
  else
    info_ptr->color_type = PNG_COLOR_TYPE_GRAY;

  if (alpha && info_ptr->color_type != PNG_COLOR_TYPE_PALETTE)
    info_ptr->color_type |= PNG_COLOR_MASK_ALPHA;

  info_ptr->interlace_type = interlace;

  /* gAMA chunk */
  if (gamma != -1.0) {
    info_ptr->valid |= PNG_INFO_gAMA;
    info_ptr->gamma = gamma;
  }

  /* cHRM chunk */
  if (chroma_wx != -1.0) {
    info_ptr->valid |= PNG_INFO_cHRM;
    info_ptr->x_white = chroma_wx;
    info_ptr->y_white = chroma_wy;
    info_ptr->x_red = chroma_rx;
    info_ptr->y_red = chroma_ry;
    info_ptr->x_green = chroma_gx;
    info_ptr->y_green = chroma_gy;
    info_ptr->x_blue = chroma_bx;
    info_ptr->y_blue = chroma_by;
  }

  /* pHYS chunk */
  if (phys_unit != -1.0) {
    info_ptr->valid |= PNG_INFO_pHYs;
    info_ptr->x_pixels_per_unit = phys_x;
    info_ptr->y_pixels_per_unit = phys_y;
    info_ptr->phys_unit_type = phys_unit;
  }

  /* PLTE chunk */
  if (info_ptr->color_type == PNG_COLOR_TYPE_PALETTE) {
    cht = ppm_colorhisttocolorhash (chv, colors);
    /* before creating palette figure out the transparent color */
    if (transparent > 0) {
      transparent = ppm_lookupcolor (cht, &transcolor);
      if (transparent == -1) {
        transparent = closestcolor (transcolor, chv, colors, maxval);
        transcolor = chv[transparent].color;
      }
      /* now put transparent color in entry 0 by swapping */
      chv[transparent].color = chv[0].color;
      chv[0].color = transcolor;
      /* check if background color was by bad luck part of swap */
      if (background == transparent)
        background = 0;
      else if (background == 0)
        background = transparent;
      /* rebuild hashtable */
      ppm_freecolorhash (cht);
      cht = ppm_colorhisttocolorhash (chv, colors);
      transparent = 0;
      trans[0] = 0; /* fully transparent */
      info_ptr->valid |= PNG_INFO_tRNS;
      info_ptr->trans = trans;
      info_ptr->num_trans = 1;
    }

    /* creating PNG palette (tRNS *not* yet valid) */
    if (alpha) {
      int bot_idx = 0;
      int top_idx = alphas_first_index[colors] - 1;

      /* remap palette indices so opaque entries are last (omittable) */
      for (i = 0;  i < colors;  ++i) {
        for (j = alphas_first_index[i];  j < alphas_first_index[i+1];  ++j) {
          if (alphas_of_color[i][j-alphas_first_index[i]] == 255)
            mapping[j] = top_idx--;
          else
            mapping[j] = bot_idx++;
        }
      }
      /* indices should have just crossed paths */
      if (bot_idx != top_idx + 1) {
        png_destroy_write_struct (&png_ptr, &info_ptr);
        pm_closer (ifp);
        pm_error ("internal inconsistency: remapped bot_idx = %d, top_idx = %d",
          bot_idx, top_idx);
      }
      for (i = 0 ; i < colors ; i++) {
        for (j = alphas_first_index[i] ; j < alphas_first_index[i+1] ; j++) {
          palette[mapping[j]].red   = PPM_GETR (chv[i].color);
          palette[mapping[j]].green = PPM_GETG (chv[i].color);
          palette[mapping[j]].blue  = PPM_GETB (chv[i].color);
          trans[mapping[j]] = alphas_of_color[i][j-alphas_first_index[i]];
        }
      }
      info_ptr->valid |= PNG_INFO_tRNS;
      info_ptr->trans = trans;
      info_ptr->num_trans = bot_idx;   /* omit opaque values */
      pm_message ("writing %d non-opaque transparency values", bot_idx);
    } else {
      for (i = 0 ; i < MAXCOLORS ; i++) {
        palette[i].red = PPM_GETR (chv[i].color);
        palette[i].green = PPM_GETG (chv[i].color);
        palette[i].blue = PPM_GETB (chv[i].color);
      }
    }
    info_ptr->valid |= PNG_INFO_PLTE;
    info_ptr->palette = palette;
    info_ptr->num_palette = palette_size;

    /* creating hIST chunk */
    if (hist) {
      for (i = 0 ; i < MAXCOLORS ; i++)
        histogram[i] = chv[i].value;
      info_ptr->valid |= PNG_INFO_hIST;
      info_ptr->hist = histogram;
      if (verbose)
        pm_message ("histogram created");
    }
    ppm_freecolorhist (chv);

  } else /* color_type != PNG_COLOR_TYPE_PALETTE */
    if (info_ptr->color_type == PNG_COLOR_TYPE_GRAY) {
      if (transparent > 0) {
        info_ptr->valid |= PNG_INFO_tRNS;
        info_ptr->trans_values.gray = PNM_GET1 (transcolor);
      }
    } else
    if (info_ptr->color_type == PNG_COLOR_TYPE_RGB) {
      if (transparent > 0) {
        info_ptr->valid |= PNG_INFO_tRNS;
        info_ptr->trans_values.red = PPM_GETR (transcolor);
        info_ptr->trans_values.green = PPM_GETG (transcolor);
        info_ptr->trans_values.blue = PPM_GETB (transcolor);
      }
    } else {
      if (transparent > 0) {
        png_destroy_write_struct (&png_ptr, &info_ptr);
        pm_closer (ifp);
        pm_error (" (can't happen) transparency AND alpha");
      }
    }

  /* bKGD chunk */
  if (background > -1) {
    info_ptr->valid |= PNG_INFO_bKGD;
    if (info_ptr->color_type == PNG_COLOR_TYPE_PALETTE) {
      if (alpha)
        info_ptr->background.index = mapping[alphas_first_index[background]];
      else
        info_ptr->background.index = background;
    } else
    if (info_ptr->color_type == PNG_COLOR_TYPE_RGB ||
        info_ptr->color_type == PNG_COLOR_TYPE_RGB_ALPHA) {
      info_ptr->background.red = PPM_GETR (backcolor);
      info_ptr->background.green = PPM_GETG (backcolor);
      info_ptr->background.blue = PPM_GETB (backcolor);
    } else
    if (info_ptr->color_type == PNG_COLOR_TYPE_GRAY ||
        info_ptr->color_type == PNG_COLOR_TYPE_GRAY_ALPHA) {
      info_ptr->background.gray = PNM_GET1 (backcolor);
    }
  }

  /* sBIT chunk */
  if ((sbitval != 0) || (alpha && (alpha_sbitval != 0))) {
    info_ptr->valid |= PNG_INFO_sBIT;

    if (sbitval == 0)
      sbitval = pm_maxvaltobits (maxval);
    if (alpha_sbitval == 0)
      alpha_sbitval = pm_maxvaltobits (maxval);

    if (info_ptr->color_type & PNG_COLOR_MASK_COLOR) {
      info_ptr->sig_bit.red   = sbitval;
      info_ptr->sig_bit.green = sbitval;
      info_ptr->sig_bit.blue  = sbitval;
    } else {
      info_ptr->sig_bit.gray = sbitval;
    }

    if (info_ptr->color_type & PNG_COLOR_MASK_ALPHA) {
      info_ptr->sig_bit.alpha = alpha_sbitval;
    }
  }

  /* tEXT and zTXT chunks */
  if ((text) || (ztxt)) {
    read_text (info_ptr, tfp);
  }

  /* tIME chunk */
  if (mtime) {
    info_ptr->valid |= PNG_INFO_tIME;
    sscanf (date_string, "%d-%d-%d", &time_struct.tm_year,
                                     &time_struct.tm_mon,
                                     &time_struct.tm_mday);
    if (time_struct.tm_year > 1900)
      time_struct.tm_year -= 1900;
    time_struct.tm_mon--; /* tm has monthes 0..11 */
    sscanf (time_string, "%d:%d:%d", &time_struct.tm_hour,
                                     &time_struct.tm_min,
                                     &time_struct.tm_sec);
    png_convert_from_struct_tm (&info_ptr->mod_time, &time_struct);
  }

  /* explicit filter-type (or none) required */
  if ((filter >= 0) && (filter <= 4))
  {
    png_set_filter (png_ptr, 0, filter);
  }

  /* zlib compression-level (or none) required */
  if ((compression >= 0) && (compression <= 9))
  {
    png_set_compression_level (png_ptr, compression);
  }

  /* write the png-info struct */
  png_write_info (png_ptr, info_ptr);

  if ((text) || (ztxt))
    /* prevent from being written twice with png_write_end */
    info_ptr->num_text = 0;

  if (mtime)
    /* prevent from being written twice with png_write_end */
    info_ptr->valid &= ~PNG_INFO_tIME;

  /* let libpng take care of, e.g., bit-depth conversions */
  png_set_packing (png_ptr);

  /* max: 3 color channels, one alpha channel, 16-bit */
  if ((line = (png_byte *) malloc (cols*8)) == NULL)
  {
    png_destroy_write_struct (&png_ptr, &info_ptr);
    pm_closer (ifp);
    pm_error ("out of memory allocating PNG row buffer");
  }

  for (pass = 0 ; pass < png_set_interlace_handling (png_ptr) ; pass++) {
    for (y = 0 ; y < rows ; y++) {
      pp = line;
      for (x = 0 ; x < cols ; x++) {
        p = xels[y][x];
        if (info_ptr->color_type == PNG_COLOR_TYPE_GRAY ||
            info_ptr->color_type == PNG_COLOR_TYPE_GRAY_ALPHA) {
          if (depth == 16)
            *pp++ = PNM_GET1 (p) >> 8;
          *pp++ = PNM_GET1 (p)&0xff;
        } else if (info_ptr->color_type == PNG_COLOR_TYPE_PALETTE) {
          color = ppm_lookupcolor (cht, &p);
          if (alpha) {
            for (i = alphas_first_index[color] ;
                 i < alphas_first_index[color+1] ; i++)
              if (alpha_mask[y][x] ==
                  alphas_of_color[color][i - alphas_first_index[color]])
              {
                color = mapping[i];
                break;
              }
          }
          *pp++ = color;
        } else if (info_ptr->color_type == PNG_COLOR_TYPE_RGB ||
                   info_ptr->color_type == PNG_COLOR_TYPE_RGB_ALPHA) {
          if (depth == 16)
            *pp++ = PPM_GETR (p) >> 8;
          *pp++ = PPM_GETR (p)&0xff;
          if (depth == 16)
            *pp++ = PPM_GETG (p) >> 8;
          *pp++ = PPM_GETG (p)&0xff;
          if (depth == 16)
            *pp++ = PPM_GETB (p) >> 8;
          *pp++ = PPM_GETB (p)&0xff;
        } else {
          png_destroy_write_struct (&png_ptr, &info_ptr);
          pm_closer (ifp);
          pm_error (" (can't happen) undefined color_type");
        }
        if (info_ptr->color_type & PNG_COLOR_MASK_ALPHA) {
          if (depth == 16)
            *pp++ = alpha_mask[y][x] >> 8;
          *pp++ = alpha_mask[y][x]&0xff;
        }
      }
      png_write_row (png_ptr, line);
    }
  }
  png_write_end (png_ptr, info_ptr);
  png_write_destroy (png_ptr);
  /* flush first because free(png_ptr) can segfault due to jmpbuf problems
     in png_write_destroy */
  fflush (stdout);
  free (png_ptr);
  free (info_ptr);
  for (i = 0 ; i < num_alphas_of_color ; i++)
    free(alphas_of_color[i]);
}

#ifdef __STDC__
int main (int argc, char *argv[])
#else
int main (argc, argv)
int argc;
char *argv[];
#endif
{
  FILE *ifp, *tfp, *afp;
  int argn;

  char *usage = "[-verbose] [-downscale] [-interlace] [-alpha file] ...\n\
             ... [-transparent color] [-background color] [-gamma value] ...\n\
             ... [-hist] [-chroma wx wy rx ry gx gy bx by] [-phys x y unit] ...\n\
             ... [-text file] [-ztxt file] [-time [yy]yy-mm-dd hh:mm:ss] ...\n\
             ... [-filter 0..4] [-compression 0..9] [-force] [pnmfile]";

  pnm_init (&argc, argv);
  argn = 1;

  while (argn < argc && argv[argn][0] == '-' && argv[argn][1] != '\0') {
    if (pm_keymatch (argv[argn], "-verbose", 2)) {
      verbose = TRUE;
    } else
    if (pm_keymatch (argv[argn], "-downscale", 2)) {
      downscale = TRUE;
    } else
    if (pm_keymatch (argv[argn], "-interlace", 2)) {
      interlace = TRUE;
    } else
    if (pm_keymatch (argv[argn], "-alpha", 2)) {
      if (transparent > 0)
        pm_error ("-alpha and -transparent are mutually exclusive");
      alpha = TRUE;
      if (++argn < argc)
        alpha_file = argv[argn];
      else
        pm_usage (usage);
    } else
    if (pm_keymatch (argv[argn], "-transparent", 3)) {
      if (alpha)
        pm_error ("-alpha and -transparent are mutually exclusive");
      transparent = 1;
      if (++argn < argc)
        transstring = argv[argn];
      else
        pm_usage (usage);
    } else
    if (pm_keymatch (argv[argn], "-background", 2)) {
      background = 1;
      if (++argn < argc)
        backstring = argv[argn];
      else
        pm_usage (usage);
    } else
    if (pm_keymatch (argv[argn], "-gamma", 2)) {
      if (++argn < argc)
        sscanf (argv[argn], "%f", &gamma);
      else
        pm_usage (usage);
    } else
    if (pm_keymatch (argv[argn], "-hist", 2)) {
      hist = TRUE;
    } else
    if (pm_keymatch (argv[argn], "-chroma", 3)) {
      if (++argn < argc)
        sscanf (argv[argn], "%f", &chroma_wx);
      else
        pm_usage (usage);
      if (++argn < argc)
        sscanf (argv[argn], "%f", &chroma_wy);
      else
        pm_usage (usage);
      if (++argn < argc)
        sscanf (argv[argn], "%f", &chroma_rx);
      else
        pm_usage (usage);
      if (++argn < argc)
        sscanf (argv[argn], "%f", &chroma_ry);
      else
        pm_usage (usage);
      if (++argn < argc)
        sscanf (argv[argn], "%f", &chroma_gx);
      else
        pm_usage (usage);
      if (++argn < argc)
        sscanf (argv[argn], "%f", &chroma_gy);
      else
        pm_usage (usage);
      if (++argn < argc)
        sscanf (argv[argn], "%f", &chroma_bx);
      else
        pm_usage (usage);
      if (++argn < argc)
        sscanf (argv[argn], "%f", &chroma_by);
      else
        pm_usage (usage);
    } else
    if (pm_keymatch (argv[argn], "-phys", 3)) {
      if (++argn < argc)
        sscanf (argv[argn], "%d", &phys_x);
      else
        pm_usage (usage);
      if (++argn < argc)
        sscanf (argv[argn], "%d", &phys_y);
      else
        pm_usage (usage);
      if (++argn < argc)
        sscanf (argv[argn], "%d", &phys_unit);
      else
        pm_usage (usage);
    } else
    if (pm_keymatch (argv[argn], "-text", 3)) {
      text = TRUE;
      if (++argn < argc)
        text_file = argv[argn];
      else
        pm_usage (usage);
    } else
    if (pm_keymatch (argv[argn], "-ztxt", 2)) {
      ztxt = TRUE;
      if (++argn < argc)
        text_file = argv[argn];
      else
        pm_usage (usage);
    } else
    if (pm_keymatch (argv[argn], "-time", 3)) {
      mtime = TRUE;
      if (++argn < argc) {
        date_string = argv[argn];
        if (++argn < argc)
          time_string = argv[argn];
        else
          pm_usage (usage);
      } else {
        pm_usage (usage);
      }
    } else 
    if (pm_keymatch (argv[argn], "-filter", 3)) {
      if (++argn < argc)
      {
        sscanf (argv[argn], "%d", &filter);
        if ((filter < 0) || (filter > 4))
        {
          pm_message
            ("filter must be 0 (none), 1 (sub), 2 (up), 3 (avg) or 4 (paeth)");
          pm_usage (usage);
        }
      }
      else
        pm_usage (usage);
    } else
    if (pm_keymatch (argv[argn], "-compression", 3)) {
      if (++argn < argc)
      {
        sscanf (argv[argn], "%d", &compression);
        if ((compression < 0) || (compression > 9))
        {
          pm_message ("zlib compression must be between 0 (none) and 9 (max)");
          pm_usage (usage);
        }
      }
      else
        pm_usage (usage);
    } else
    if (pm_keymatch (argv[argn], "-force", 3)) {
      force = TRUE;
    } else {
      fprintf(stderr,"pnmtopng version %s.\n", VERSION);
      fprintf(stderr, "   Compiled with libpng %s; using libpng %s.\n",
        PNG_LIBPNG_VER_STRING, png_libpng_ver);
      fprintf(stderr, "   Compiled with zlib %s; using zlib %s.\n",
        ZLIB_VERSION, zlib_version);
#ifdef PGM_BIGGRAYS
      fprintf(stderr,
        "   Compiled with 16-bit netpbm support (PGM_BIGGRAYS defined).\n");
#else
      fprintf(stderr,
        "   Compiled with %d-bit netpbm support (PPM_MAXMAXVAL = %d).\n",
        pm_maxvaltobits (PPM_MAXMAXVAL), PPM_MAXMAXVAL);
#endif
      fprintf(stderr, "\n");
      pm_usage (usage);
    }
    argn++;
  }

  if (argn != argc) {
    ifp = pm_openr (argv[argn]);
    ++argn;
  } else {
    ifp = stdin;
  }
  if (argn != argc)
    pm_usage (usage);

  if (alpha)
    afp = pm_openr (alpha_file);
  else
    afp = NULL;

  if ((text) || (ztxt))
    tfp = pm_openr (text_file);
  else
    tfp = NULL;

  convertpnm (ifp, afp, tfp);

  if (alpha)
    pm_closer (afp);
  if ((text) || (ztxt))
    pm_closer (tfp);

  pm_closer (ifp);
  pm_closew (stdout);

  exit (0);
}

