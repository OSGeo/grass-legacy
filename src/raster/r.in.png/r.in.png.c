/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       r.in.png
 * AUTHOR(S):    Michael Shapiro - CERL
 *               Alex Shevlakov - sixote@yahoo.com
 * PURPOSE:      Import non-georeferenced Images in PNG format. 
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

/* code based on r.in.gif by
   Michael Shapiro, U.S.Army Construction Engineering Research Laboratory
 */
/*
*		Alex Shevlakov, sixote@yahoo.com, 03/2000
*               Added -h option MN 6/2000
*/

#include 	<stdio.h>
#include	<string.h>
#include 	"gis.h"

#ifndef _MYINCLUDE_H
#define _MYINCLUDE_H
#include	"png.h"
#include	"pngfunc.h"
#endif /* _MYINCLUDE_H */

#include	<math.h>


  
extern	int	scan_lines;
struct Colors color;
char *layer;
int irow, icol;
int nrows, ncols;
int cf;
CELL *cell;
FILE *ifp;

long int num_colors;
long int maxcolor = 0;
char tmpbuf[80];

short int	write_PNG_pixel();
short int 	write_PNG_rgbpixel();
unsigned char	PNG_pixel ;

#define MaxColors	256 /* that's for palette*/

#define TRUECOLOR24	167772168
#define TRUECOLOR16	65536

long int longcolor;

struct ColorEntry
    {
    unsigned char red, green, blue;
    };
    

  typedef struct {
	int red;
	int grn;
	int blu;
  } mycolor;
  
  mycolor * ppm_color;
  
  int knum = 0;

#define SIG_CHECK_SIZE 4

#define get_png_val(p) _get_png_val (&(p), info_ptr->bit_depth)

#ifdef __STDC__
static png_uint_16 _get_png_val (png_byte **pp, int bit_depth)
#else
static png_uint_16 _get_png_val (pp, bit_depth)
png_byte **pp;
int bit_depth;
#endif
{
  png_uint_16 c = 0;

  if (bit_depth == 16) {
    c = (*((*pp)++)) << 8;
  }
  c |= (*((*pp)++));
  
  if (maxval > maxmaxval)
    c /= ((maxval + 1) / (maxmaxval + 1));

  return c;
}

#ifdef __STDC__
static png_uint_16 gamma_correct (png_uint_16 v, float g)
#else
static png_uint_16 gamma_correct (v, g)
png_uint_16 v;
float g;
#endif
{
  if (g != -1.0)
    return (png_uint_16) (pow ((double) v / maxval, 
			       (1.0 / g)) * maxval + 0.5);
  else
    return v;
}

#ifdef __STDC__
static int iscolor (png_color c)
#else
static int iscolor (c)
png_color c;
#endif
{
  return c.red != c.green || c.green != c.blue;
}

#ifdef __STDC__
static void pngtopnm_error_handler (png_structp png_ptr, png_const_charp msg)
#else
static void pngtopnm_error_handler (png_ptr, msg)
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


int main(int argc, char *argv[])
{
	char *input;
	char title[1024];
	struct Cell_head cellhd;
	int j;

	char
	LetterAnswer[2],
	    input_name[25],
	    output_name[25];
	unsigned
	done;
	short
	    is_png,
	    status;

	static struct ColorEntry
	PNG_default_map[MaxColors],
	PNG_img_map[MaxColors];

	struct Option *inopt, *outopt, *titleopt;
	struct Flag *vflag;
	struct Flag *hflag;
	struct GModule *module;

	int Verbose = 0;
	int Header = 0; /* added for r.in.geopng MN */
	char command[256];
	
  char sig_buf [SIG_CHECK_SIZE];
  png_struct *png_ptr;
  png_info *info_ptr;
  pixel *row;
  png_byte **png_image;
  png_byte *png_pixel;
  pixel *pnm_pixel;
  int x, y;
  int linesize;
  png_uint_16 c, c2, c3, a, rgb_pixel;
  int pnm_type;
  int i;
  int trans_mix;
  pixel backcolor;
  char gamma_string[80];
  static char *type_string;
  static char *alpha_string;
  
  
  
  
  
  type_string = alpha_string = "";
  

/* *********************************************************************
 * Access and prepare the PNG file 
 */

	G_gisinit (argv[0]);

	module = G_define_module();
	module->description = "Import non-georeferenced PNG format Image "
	  "into GRASS raster file.";

	inopt = G_define_option();
	outopt = G_define_option();
	titleopt = G_define_option();

	inopt->key		= "input";
	inopt->type		= TYPE_STRING;
	inopt->required	= YES;
	inopt->description	= "Name of input PNG file.";

	outopt->key		= "output";
	outopt->type		= TYPE_STRING;
	outopt->required	= YES;
	outopt->gisprompt	= "new,cell,raster";
	outopt->description	= "Name of new raster file.";

	titleopt->key	= "title";
	titleopt->type	= TYPE_STRING;
	titleopt->required	= NO;
	titleopt->description	= "Title for new raster file";

	hflag = G_define_flag();
	hflag->key		= 'h';
	hflag->description	= "Output image file header only.";

	vflag = G_define_flag();
	vflag->key		= 'v';
	vflag->description	= "Verbose mode on.";

	if(G_parser(argc, argv))
		exit(-1);

	input = inopt->answer;
	layer = outopt->answer;
	Verbose = vflag->answer;
	Header = hflag->answer;
	*title = '\0';
	if (titleopt->answer != NULL)
		G_strcpy(title,titleopt->answer);
	G_strip (title);
	
/***************************************************/
/* pngtopnm.c stuff -A.Sh.*/
/*
** pngtopnm.c -
** read a Portable Network Graphics file and produce a portable anymap
**
** Copyright (C) 1995,1998 by Alexander Lehmann <alex@hal.rhein-main.de>
**                        and Willem van Schaik <willem@schaik.com>
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
**
** modeled after giftopnm by David Koblas and
** with lots of bits pasted from libpng.txt by Guy Eric Schalnat
*/
/*****************************************************/	


  ifp = fopen (input, "rb");
  if (ifp == NULL)
		G_fatal_error("Can't open PNG file.");
		
  if (fread (sig_buf, 1, SIG_CHECK_SIZE, ifp) != SIG_CHECK_SIZE) {
    fclose (ifp);
    fprintf(stderr,"\ninput file empty or too short");
    exit(-1);
  }
  if (png_sig_cmp (sig_buf, (png_size_t) 0, (png_size_t) SIG_CHECK_SIZE) != 0) {
    fclose (ifp);
    fprintf(stderr,"\ninput file not a PNG file");
    exit(-1);
  }

  png_ptr = png_create_read_struct (PNG_LIBPNG_VER_STRING,
    &pngtopnm_jmpbuf_struct, pngtopnm_error_handler, NULL);
  if (png_ptr == NULL) {
    fclose (ifp);
    fprintf(stderr,"\ncannot allocate LIBPNG structure");
    exit(-1);
  }

  info_ptr = png_create_info_struct (png_ptr);
  if (info_ptr == NULL) {
    png_destroy_read_struct (&png_ptr, (png_infopp)NULL, (png_infopp)NULL);
    fclose (ifp);
    fprintf(stderr,"\ncannot allocate LIBPNG structures");
    exit(-1);
  }


  if (setjmp (pngtopnm_jmpbuf_struct.jmpbuf)) {
    png_destroy_read_struct (&png_ptr, &info_ptr, (png_infopp)NULL);
    fclose (ifp);
    fprintf(stderr,"\nsetjmp returns error condition");
    exit(-1);
  }

  png_init_io (png_ptr, ifp);
  png_set_sig_bytes (png_ptr, SIG_CHECK_SIZE);
  png_read_info (png_ptr, info_ptr);

  /* output header only for r.in.geopng and exit: MN 6/2000 */
   if (Header)
   {
    switch (info_ptr->color_type) {
      case PNG_COLOR_TYPE_GRAY:
        type_string = "gray";
        alpha_string = "";
        break;

      case PNG_COLOR_TYPE_GRAY_ALPHA:
        type_string = "gray";
        alpha_string = "+alpha";
        break;

      case PNG_COLOR_TYPE_PALETTE:
        type_string = "palette";
        alpha_string = "";
        break;

      case PNG_COLOR_TYPE_RGB:
        type_string = "truecolor";
        alpha_string = "";
        break;

      case PNG_COLOR_TYPE_RGB_ALPHA:
        type_string = "truecolor";
        alpha_string = "+alpha";
        break;
    }
    fprintf(stderr,"%d x %d image, %d bit%s %s%s\n",
		  info_ptr->width, info_ptr->height,
		  info_ptr->bit_depth, info_ptr->bit_depth > 1 ? "s" : "",
		  type_string, alpha_string, gamma_string);

    fclose (ifp);    
    exit(-1);
   } /* HEADER */

   if (Verbose) {
    switch (info_ptr->color_type) {
      case PNG_COLOR_TYPE_GRAY:
        type_string = "gray";
	fprintf(stderr,"\nType of png image is grey\n");
        alpha_string = "";
        break;

      case PNG_COLOR_TYPE_GRAY_ALPHA:
        type_string = "gray";
	fprintf(stderr,"\nType of png image is grey with alpha\n");
        alpha_string = "+alpha";
        break;

      case PNG_COLOR_TYPE_PALETTE:
        type_string = "palette";
	fprintf(stderr,"\nType of png image is palette\n");
        alpha_string = "";
        break;

      case PNG_COLOR_TYPE_RGB:
        type_string = "truecolor";
	fprintf(stderr,"\nType of png image is rgb\n");
        alpha_string = "";
        break;

      case PNG_COLOR_TYPE_RGB_ALPHA:
        type_string = "truecolor";
	fprintf(stderr,"\nType of png image is rgb with alpha\n");
        alpha_string = "+alpha";
        break;
    }
   if (info_ptr->color_type == PNG_COLOR_TYPE_RGB) {
   
    fprintf (stdout,"\n MAXCOLOR value:\n");
		fprintf (stdout,"Enter \"24\" or \"16\" for bits\n");
		fprintf (stdout,"Hit RETURN if you're not sure\n");
		fprintf (stdout,"> ");
		fgets(tmpbuf,80,stdin);
   /*************************************************************************/
   /*****!!!!!! hier eine Zeile eingefuegt,ebenso bei allen anderen fgets *****/

              tmpbuf[strlen(tmpbuf)-1]='\0';
   /********************************************************************/
                

		if (!strlen(tmpbuf)) {
			longcolor=TRUECOLOR16;
		}
		else if (!strncmp(tmpbuf,"24",2)) { 
			longcolor=TRUECOLOR24;
			fprintf(stdout,"\nMAXCOLOR set to 24 bit\n");
		}
		else if (!strncmp(tmpbuf,"16",2)) { 
			longcolor=TRUECOLOR16;
			fprintf(stdout,"\nMAXCOLOR set to 16 bit\n");
		}
		else {
			longcolor=TRUECOLOR16;
			fprintf(stdout,"\nMAXCOLOR set to 16 bit\n");
		}
	
/*	ppm_color = (mycolor*) malloc(longcolor * sizeof(mycolor));
                
  	if (ppm_color == NULL) {
    		fprintf(stderr,"\nCouldn't allocate space for image.\n Try reduce MAXCOLOR to 16bit,\nor - increase your comp's RAM\n");
    	exit(-1);
  	}
*/
	knum = (int) floor(longcolor/100);
	

	ppm_color = (mycolor*) malloc(knum * sizeof(mycolor));
  
  	if (ppm_color == NULL) {
    		fprintf(stderr,"\nCouldn't allocate space for image.\n Try increase your comp's RAM (256 Mb should do)\n");
    		exit(-1);
  	}
    }
    
    if (info_ptr->valid & PNG_INFO_tRNS) {
      alpha_string = "+transparency";
    }

    if (info_ptr->valid & PNG_INFO_gAMA) {
      sprintf (gamma_string, ", image gamma = %4.2f", info_ptr->gamma);
    } else {
      strcpy (gamma_string, "");
    }

    if (Verbose)
      fprintf(stderr,"\nreading a %d x %d image, %d bit%s %s%s%s%s",
		  info_ptr->width, info_ptr->height,
		  info_ptr->bit_depth, info_ptr->bit_depth > 1 ? "s" : "",
		  type_string, alpha_string, gamma_string,
		  info_ptr->interlace_type ? ", Adam7 interlaced" : "");

  }

  png_image = (png_byte **)malloc (info_ptr->height * sizeof (png_byte*));
  if (png_image == NULL) {
    png_destroy_read_struct (&png_ptr, &info_ptr, (png_infopp)NULL);
    fclose (ifp);
    fprintf(stderr,"\ncouldn't allocate space for image");
    exit(-1);
  }

  if (info_ptr->bit_depth == 16)
    linesize = 2 * info_ptr->width;
  else
    linesize = info_ptr->width;

  if (info_ptr->color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
    linesize *= 2;
  else
  if (info_ptr->color_type == PNG_COLOR_TYPE_RGB)
    linesize *= 3;
  else
  if (info_ptr->color_type == PNG_COLOR_TYPE_RGB_ALPHA)
    linesize *= 4;

  for (y = 0 ; y < info_ptr->height ; y++) {
    png_image[y] = malloc (linesize);
    if (png_image[y] == NULL) {
      for (x = 0 ; x < y ; x++)
        free (png_image[x]);
      free (png_image);
      png_destroy_read_struct (&png_ptr, &info_ptr, (png_infopp)NULL);
      fclose (ifp);
      fprintf(stderr,"\ncouldn't allocate space for image");
      exit(-1);
    }
  }

  if (info_ptr->bit_depth < 8)
    png_set_packing (png_ptr);

  if (info_ptr->color_type == PNG_COLOR_TYPE_PALETTE) {
    maxval = 255;
  } else {
    maxval = (1l << info_ptr->bit_depth) - 1;
  }

  /* gamma-correction */
  if (displaygamma != -1.0) {
    if (info_ptr->valid & PNG_INFO_gAMA) {
      if (displaygamma != info_ptr->gamma) {
        png_set_gamma (png_ptr, displaygamma, info_ptr->gamma);
	totalgamma = (double) info_ptr->gamma * (double) displaygamma;
	/* in case of gamma-corrections, sBIT's as in the PNG-file are not valid anymore */
	info_ptr->valid &= ~PNG_INFO_sBIT;
        if (verbose)
          fprintf(stderr,"\nimage gamma is %4.2f, converted for display gamma of %4.2f",
                    info_ptr->gamma, displaygamma);
      }
    } else {
      if (displaygamma != info_ptr->gamma) {
	png_set_gamma (png_ptr, displaygamma, 1.0);
	totalgamma = (double) displaygamma;
	info_ptr->valid &= ~PNG_INFO_sBIT;
	if (verbose)
	  fprintf(stderr,"\nimage gamma assumed 1.0, converted for display gamma of %4.2f",
		      displaygamma);
      }
    }
  }

  /* sBIT handling is very tricky. If we are extracting only the image, we
     can use the sBIT info for grayscale and color images, if the three
     values agree. If we extract the transparency/alpha mask, sBIT is
     irrelevant for trans and valid for alpha. If we mix both, the
     multiplication may result in values that require the normal bit depth,
     so we will use the sBIT info only for transparency, if we know that only
     solid and fully transparent is used */

  if (info_ptr->valid & PNG_INFO_sBIT) {
    switch (alpha) {
      case mix:
        if (info_ptr->color_type == PNG_COLOR_TYPE_RGB_ALPHA ||
            info_ptr->color_type == PNG_COLOR_TYPE_GRAY_ALPHA)
          break;
        if (info_ptr->color_type == PNG_COLOR_TYPE_PALETTE &&
            (info_ptr->valid & PNG_INFO_tRNS)) {
          trans_mix = TRUE;
          for (i = 0 ; i < info_ptr->num_trans ; i++)
            if (info_ptr->trans[i] != 0 && info_ptr->trans[i] != 255) {
              trans_mix = FALSE;
              break;
            }
          if (!trans_mix)
            break;
        }

        /* else fall though to normal case */

      case none:
        if ((info_ptr->color_type == PNG_COLOR_TYPE_PALETTE ||
             info_ptr->color_type == PNG_COLOR_TYPE_RGB ||
             info_ptr->color_type == PNG_COLOR_TYPE_RGB_ALPHA) &&
            (info_ptr->sig_bit.red != info_ptr->sig_bit.green ||
             info_ptr->sig_bit.red != info_ptr->sig_bit.blue) &&
            alpha == none) {
	  fprintf(stderr,"\ndifferent bit depths for color channels not supported");
	  fprintf(stderr,"\nwriting file with %d bit resolution", info_ptr->bit_depth);
        } else {
          if ((info_ptr->color_type == PNG_COLOR_TYPE_PALETTE) &&
	      (info_ptr->sig_bit.red < 255)) {
	    for (i = 0 ; i < info_ptr->num_palette ; i++) {
	      info_ptr->palette[i].red   >>= (8 - info_ptr->sig_bit.red);
	      info_ptr->palette[i].green >>= (8 - info_ptr->sig_bit.green);
	      info_ptr->palette[i].blue  >>= (8 - info_ptr->sig_bit.blue);
	    }
	    maxval = (1l << info_ptr->sig_bit.red) - 1;
	    if (verbose)
	      fprintf(stderr,"\nimage has fewer significant bits, writing file with %d bits per channel", 
		info_ptr->sig_bit.red);
          } else
          if ((info_ptr->color_type == PNG_COLOR_TYPE_RGB ||
               info_ptr->color_type == PNG_COLOR_TYPE_RGB_ALPHA) &&
	      (info_ptr->sig_bit.red < info_ptr->bit_depth)) {
	    png_set_shift (png_ptr, &(info_ptr->sig_bit));
	    maxval = (1l << info_ptr->sig_bit.red) - 1;
	    if (verbose)
	      fprintf(stderr,"\nimage has fewer significant bits, writing file with %d bits per channel", 
		info_ptr->sig_bit.red);
          } else 
          if ((info_ptr->color_type == PNG_COLOR_TYPE_GRAY ||
               info_ptr->color_type == PNG_COLOR_TYPE_GRAY_ALPHA) &&
	      (info_ptr->sig_bit.gray < info_ptr->bit_depth)) {
	    png_set_shift (png_ptr, &(info_ptr->sig_bit));
	    maxval = (1l << info_ptr->sig_bit.gray) - 1;
	    if (verbose)
	      fprintf(stderr,"\nimage has fewer significant bits, writing file with %d bits",
		info_ptr->sig_bit.gray);
          }
        }
        break;

      case alpha_only:
        if ((info_ptr->color_type == PNG_COLOR_TYPE_RGB_ALPHA ||
             info_ptr->color_type == PNG_COLOR_TYPE_GRAY_ALPHA) && 
	    (info_ptr->sig_bit.gray < info_ptr->bit_depth)) {
	  png_set_shift (png_ptr, &(info_ptr->sig_bit));
	  if (verbose)
	    fprintf(stderr,"\nimage has fewer significant bits, writing file with %d bits", 
		info_ptr->sig_bit.alpha);
	  maxval = (1l << info_ptr->sig_bit.alpha) - 1;
        }
        break;

      }
  }

  /* didn't manage to get libpng to work (bugs?) concerning background */
  /* processing, therefore we do our own using bgr, bgg and bgb        */
  if (info_ptr->valid & PNG_INFO_bKGD)
    switch (info_ptr->color_type) {
      case PNG_COLOR_TYPE_GRAY:
      case PNG_COLOR_TYPE_GRAY_ALPHA:
        bgr = bgg = bgb = gamma_correct (info_ptr->background.gray, totalgamma);
        break;
      case PNG_COLOR_TYPE_PALETTE:
        bgr = gamma_correct (info_ptr->palette[info_ptr->background.index].red, totalgamma);
        bgg = gamma_correct (info_ptr->palette[info_ptr->background.index].green, totalgamma);
        bgb = gamma_correct (info_ptr->palette[info_ptr->background.index].blue, totalgamma);
        break;
      case PNG_COLOR_TYPE_RGB:
      case PNG_COLOR_TYPE_RGB_ALPHA:
        bgr = gamma_correct (info_ptr->background.red, totalgamma);
        bgg = gamma_correct (info_ptr->background.green, totalgamma);
        bgb = gamma_correct (info_ptr->background.blue, totalgamma);
        break;
    }
  else
    /* when no background given, we use white [from version 2.37] */
    bgr = bgg = bgb = maxval;

  /* but if background was specified from the command-line, we always use that	*/
  /* I chose to do no gamma-correction in this case; which is a bit arbitrary	*/

  if (background > -1)
  {
/*    Here avoid ppm_parsecolor to no use ppm, pnm, ... libs in gmakefile -ash*/

/*    backcolor = ppm_parsecolor (backstring, maxval);
    switch (info_ptr->color_type) {
      case PNG_COLOR_TYPE_GRAY:
      case PNG_COLOR_TYPE_GRAY_ALPHA:
        bgr = bgg = bgb = PNM_GET1 (backcolor);
        break;
      case PNG_COLOR_TYPE_PALETTE:
      case PNG_COLOR_TYPE_RGB:
      case PNG_COLOR_TYPE_RGB_ALPHA:
        bgr = PPM_GETR (backcolor);
        bgg = PPM_GETG (backcolor);
        bgb = PPM_GETB (backcolor);
        break;
    }
*/

    png_destroy_read_struct (&png_ptr, &info_ptr, (png_infopp)NULL);
    fclose (ifp);
    fprintf(stderr,"\nimages with background not supported .. exiting\n");
    exit(-1);

  }
  png_read_image (png_ptr, png_image);
  png_read_end (png_ptr, info_ptr);
	
  if (alpha == alpha_only) {
    if (info_ptr->color_type == PNG_COLOR_TYPE_GRAY ||
        info_ptr->color_type == PNG_COLOR_TYPE_RGB) {
      pnm_type = PBM_TYPE;
    } else
      if (info_ptr->color_type == PNG_COLOR_TYPE_PALETTE) {
        pnm_type = PBM_TYPE;
        if (info_ptr->valid & PNG_INFO_tRNS) {
          for (i = 0 ; i < info_ptr->num_trans ; i++) {
            if (info_ptr->trans[i] != 0 && info_ptr->trans[i] != maxval) {
              pnm_type = PGM_TYPE;
              break;
            }
          }
        }
      } else {
        if (maxval == 1)
          pnm_type = PBM_TYPE;
        else
          pnm_type = PGM_TYPE;
      }
  } else {
    if (info_ptr->color_type == PNG_COLOR_TYPE_GRAY ||
        info_ptr->color_type == PNG_COLOR_TYPE_GRAY_ALPHA) {
      if (info_ptr->bit_depth == 1) {
        pnm_type = PBM_TYPE;
      } else {
        pnm_type = PGM_TYPE;
      }
    } else
    if (info_ptr->color_type == PNG_COLOR_TYPE_PALETTE) {
      pnm_type = PGM_TYPE;
      for (i = 0 ; i < info_ptr->num_palette ; i++) {
        if (iscolor (info_ptr->palette[i])) {
          pnm_type = PPM_TYPE;
          break;
        }
      }
    } else {
      pnm_type = PPM_TYPE;
    }
  }

  if ((pnm_type == PGM_TYPE) && (maxval > PGM_MAXMAXVAL))
    maxmaxval = PGM_MAXMAXVAL;
  else if ((pnm_type == PPM_TYPE) && (maxval > PPM_MAXMAXVAL))
    maxmaxval = PPM_MAXMAXVAL;
  else maxmaxval = maxval;
 

	cellhd.zone = G_zone();
	cellhd.proj = G_projection();
	nrows = cellhd.rows = cellhd.north = info_ptr->height;
	ncols = cellhd.cols = cellhd.east = info_ptr->width;
	cellhd.south = cellhd.west = 0.0;
	cellhd.ns_res = 1;
	cellhd.ew_res = 1;

	if(G_set_window (&cellhd) < 0)
		exit(3);

	if (nrows != G_window_rows())
	{
		fprintf (stderr, "OOPS: rows changed from %d to %d\n", nrows, G_window_rows());
		exit(1);
	}
	if (ncols != G_window_cols())
	{
		fprintf (stderr, "OOPS: cols changed from %d to %d\n", ncols, G_window_cols());
		exit(1);
	}

	G_set_cell_format(0); /* open with 1 byte per cell */
	cell = G_allocate_cell_buf();

	cf = G_open_cell_new(layer);
	if (!cf)
	{
		char msg[100];
		sprintf (msg, "unable to create layer %s", layer);
		G_fatal_error (msg);
		exit(1);
	}
	irow = icol = 0;
	/* Test compatibility */
	
	
/*************************************/
/*Conversion code -A.Sh.*/
if (info_ptr->color_type == PNG_COLOR_TYPE_RGB){

	for (y = 0 ; y < info_ptr->height ; y++) {
    		png_pixel = png_image[y];

    		for (x = 0 ; x < info_ptr->width ; x++) {
      			c = get_png_val (png_pixel);
      			c2 = get_png_val (png_pixel);
      			c3 = get_png_val (png_pixel);
			fill_color_tab(c,c2,c3);
      
    		}
	}
	num_colors = maxcolor;
	if (Verbose) 
		fprintf(stdout,"\nTotal number of colors is %d\n",maxcolor);
}
 for (y = 0 ; y < info_ptr->height ; y++) {
    png_pixel = png_image[y];

    for (x = 0 ; x < info_ptr->width ; x++) {
      c = get_png_val (png_pixel);
      
      switch (info_ptr->color_type) {
        case PNG_COLOR_TYPE_GRAY:
/*          store_pixel (pnm_pixel, c, c, c,
		((info_ptr->valid & PNG_INFO_tRNS) &&
		 (c == gamma_correct (info_ptr->trans_values.gray, totalgamma))) ?
			0 : maxval);
*/
	for (i = 0 ; i < info_ptr->height ; i++)
            free (png_image[i]);
      free (png_image);
      png_destroy_read_struct (&png_ptr, &info_ptr, (png_infopp)NULL);
      fclose (ifp);
      fprintf(stderr,"\ngrey not supported yet- yummm!\n");
      exit(-1);
          break;

        case PNG_COLOR_TYPE_GRAY_ALPHA:
/*          a = get_png_val (png_pixel);
          store_pixel (pnm_pixel, c, c, c, a);
*/
      for (i = 0 ; i < info_ptr->height ; i++)
            free (png_image[i]);
      free (png_image);
      png_destroy_read_struct (&png_ptr, &info_ptr, (png_infopp)NULL);
      fclose (ifp);
      fprintf(stderr,"\ngrey_alpha not supported yet- yummm!\n");
      exit(-1);
          break;

        case PNG_COLOR_TYPE_PALETTE:
/*         store_pixel (pnm_pixel, info_ptr->palette[c].red,
                       info_ptr->palette[c].green, info_ptr->palette[c].blue,
                       (info_ptr->valid & PNG_INFO_tRNS) &&
                        c<info_ptr->num_trans ?
                        info_ptr->trans[c] : maxval);
*/
		write_PNG_pixel(c);
          break;

        case PNG_COLOR_TYPE_RGB:
	
          c2 = get_png_val (png_pixel);
          c3 = get_png_val (png_pixel);

			   rgb_pixel = lookup_color(c,c2,c3,num_colors);
			   write_PNG_rgbpixel(rgb_pixel);

/*
      for (i = 0 ; i < info_ptr->height ; i++)
            free (png_image[i]);
      free (png_image);
      png_destroy_read_struct (&png_ptr, &info_ptr, (png_infopp)NULL);
      fclose (ifp);
      fprintf(stderr,"\nrgb not supported yet- yummm!\n");
      exit(-1);
*/
          break;

        case PNG_COLOR_TYPE_RGB_ALPHA:
/*          c2 = get_png_val (png_pixel);
          c3 = get_png_val (png_pixel);
          a = get_png_val (png_pixel);
          store_pixel (pnm_pixel, c, c2, c3, a);
*/
      for (i = 0 ; i < info_ptr->height ; i++)
            free (png_image[i]);
      free (png_image);
      png_destroy_read_struct (&png_ptr, &info_ptr, (png_infopp)NULL);
      fclose (ifp);
      fprintf(stderr,"\nrgb alpha not supported yet- yummm!\n");
      exit(-1);
          break;

        default:

          for (i = 0 ; i < info_ptr->height ; i++)
            free (png_image[i]);
          free (png_image);
          png_destroy_read_struct (&png_ptr, &info_ptr, (png_infopp)NULL);
          fclose (ifp);
          fprintf(stderr,"\nunknown PNG color type\n");
	  exit(-1);
      }

    }

  }

/*******************************/  
	


	if (Verbose)
		fprintf (stderr, "\nCREATING SUPPORT FILES FOR %s\n", layer);
	G_close_cell (cf);
	if (*title)
		G_put_cell_title (layer, title);
		
		G_init_colors(&color);
		
	if ( info_ptr->color_type == PNG_COLOR_TYPE_PALETTE) {
	    for (i = 0 ; i < info_ptr->num_palette ; i++) {
	    	      
	      G_set_color((CELL)i, info_ptr->palette[i].red, info_ptr->palette[i].green, info_ptr->palette[i].blue, &color);
	    }
	} else if ( info_ptr->color_type == PNG_COLOR_TYPE_RGB) {
		
		for(i=0; i <num_colors; i++){
		G_set_color((CELL)i, ppm_color[i].red, ppm_color[i].grn,
			ppm_color[i].blu, &color);
		}
 
	} else if ( info_ptr->color_type == PNG_COLOR_TYPE_GRAY) {
/*do something here*/
	} 
	    
	G_write_colors(layer, G_mapset(), &color);
	
	free(ppm_color);


    exit (0);
}



short int write_PNG_pixel(PNG_pixel)
unsigned char	PNG_pixel ;
{
	cell[icol] = (CELL)PNG_pixel;
	if (icol == ncols){
		icol = 0;
		G_put_c_raster_row(cf, cell);
		irow ++;
	}
	icol++;
	return(0);
} /* write_PNG_pixel */

short int write_PNG_rgbpixel(PNG_pixel)
int	PNG_pixel ;
{
	cell[icol] = (CELL)PNG_pixel;
	if (icol == ncols){
		icol = 0;
		G_put_c_raster_row(cf, cell);
		irow ++;
	}
	icol++;
	return(0);
} /* write_PNG_pixel */

int 
lookup_color (int r, int g, int b, int num)
{
	int x;

	for (x=0;x<num;x++){
		if (ppm_color[x].red == r && 
		    ppm_color[x].grn == g &&
		    ppm_color[x].blu == b){
			break;
		}
	}
	return(x);
}


int fill_color_tab( int c, int c2, int c3) {

int i;
int match = 0;

	
		for (i=0;i<maxcolor;i++){
			if (ppm_color[i].red == c && 
			    ppm_color[i].grn == c2 &&
			    ppm_color[i].blu == c3){
				match = 1;
				break;
			}
		}
		if (match == 0){
			ppm_color[maxcolor].red = c;
			ppm_color[maxcolor].grn = c2;
			ppm_color[maxcolor].blu = c3;
			maxcolor++;
			
			if (knum < maxcolor + 1) {
			
			knum *= 2;
			ppm_color = (mycolor*) realloc((void*) ppm_color,knum * sizeof(mycolor));
			
  				if (ppm_color == NULL) {
    					fprintf(stderr,"\nCouldn't allocate space for image.\n Try increase your comp's RAM (256 Mb should do)\n");
    					exit(-1);
  				}
			}
			
			if (maxcolor == longcolor)
				G_fatal_error("Exceeded maximum colors - exiting");
		}
return 0;
}
