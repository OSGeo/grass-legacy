/* 
 * gdith.c --
 *
 *      Procedures dealing with grey-scale and mono dithering, 
 *      as well as X Windows set up procedures.
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

#include <math.h>
#include "video.h"
#include "proto.h"
#include "dither.h"
#include <sys/time.h>

/* Range values for lum, cr, cb. */
int LUM_RANGE;
int CR_RANGE;
int CB_RANGE;

/* Array that remaps color numbers to actual pixel values used by X server. */

unsigned char pixel[256];
unsigned long wpixel[256];

/* Arrays holding quantized value ranged for lum, cr, and cb. */

int *lum_values;
int *cr_values;
int *cb_values;

/* Declaration of global variable containing dither type. */

extern int ditherType;
extern int matched_depth;

/* Structures used by the X server. */

Display *display;

static XImage *ximage = NULL;
static Colormap cmap;
static Window window;
static GC gc;

/* Frame Rate Info */
extern int framerate;

/* Video rates table */
/* Cheat on Vid rates, round to 30, and use 30 if illegal value 
   Except for 9, where Xing means 15, and given their popularity, we'll
   be nice and do it */
static int VidRateNum[16]={30, 24, 24, 25, 30, 30, 50, 60, 
                         60, 15, 30, 30, 30, 30, 30, 30};

/* Luminance and chrominance lookup tables */
static double *L_tab, *Cr_r_tab, *Cr_g_tab, *Cb_g_tab, *Cb_b_tab;


/*
 *--------------------------------------------------------------
 *
 * InitColor --
 *
 *	Initialize lum, cr, and cb quantized range value arrays.
 *      Also initializes the lookup tables for the possible
 *      values of lum, cr, and cb.
 *
 * Results: 
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void
InitColor()
{
  int i, CR, CB;

  L_tab    = (double *)malloc(LUM_RANGE*sizeof(double)); 
  Cr_r_tab = (double *)malloc(CR_RANGE*sizeof(double));
  Cr_g_tab = (double *)malloc(CR_RANGE*sizeof(double));
  Cb_g_tab = (double *)malloc(CB_RANGE*sizeof(double));
  Cb_b_tab = (double *)malloc(CB_RANGE*sizeof(double));

  if (gammaCorrectFlag) {
    for (i=0; i<LUM_RANGE; i++) {
      lum_values[i] = ((i * 256) / (LUM_RANGE)) + (256/(LUM_RANGE*2));
      L_tab[i] = 1.164 * (lum_values[i] - 16.0);
      L_tab[i] = (pow(L_tab[i] / 255.0, (1 / gammaCorrect)) * 255.0) + 0.5;
    }
  } else {
    for (i=0; i<LUM_RANGE; i++) {
      lum_values[i] = ((i * 256) / (LUM_RANGE)) + (256/(LUM_RANGE*2));
      L_tab[i] = 1.164 * (lum_values[i] - 16.0);
    }
  }

  for (i=0; i<CR_RANGE; i++) {
    cr_values[i] = ((i * 256) / (CR_RANGE)) + (256/(CR_RANGE*2));
    Cr_r_tab[i] = 1.366 * (cr_values[i] - 128.0);
    Cr_g_tab[i] = -0.700 * (cr_values[i] - 128.0);
  }

  for (i=0; i<CB_RANGE; i++) {
    cb_values[i] = ((i * 256) / (CB_RANGE)) + (256/(CB_RANGE*2));
    Cb_g_tab[i] = -0.334 * (cb_values[i] - 128.0);
    Cb_b_tab[i] = 1.732 * (cb_values[i] - 128.0);
  }

}


/*
 *--------------------------------------------------------------
 *
 * ConvertColor --
 *
 *	Given a l, cr, cb tuple, converts it to r,g,b.
 *
 * Results:
 *	r,g,b values returned in pointers passed as parameters.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static void
ConvertColor(l, cr, cb, r, g, b)
     unsigned int l, cr, cb;
     unsigned char *r, *g, *b;
{
  double fl, fcr, fcb, fr, fg, fb;

/*
 * Old method w/o lookup table
 *
 * fl = 1.164*(((double) l)-16.0);
 * fcr =  ((double) cr) - 128.0;
 * fcb =  ((double) cb) - 128.0;
 *
 * fr = fl + (1.366 * fcr);
 * fg = fl - (0.700 * fcr) - (0.334 * fcb);
 * fb = fl + (1.732 * fcb);
 */

  fl = L_tab[l];

  fr = fl + Cr_r_tab[cr];
  fg = fl + Cr_g_tab[cr] + Cb_g_tab[cb];
  fb = fl + Cb_b_tab[cb];

  if (fr < 0.0) fr = 0.0;
  else if (fr > 255.0) fr = 255.0;

  if (fg < 0.0) fg = 0.0;
  else if (fg > 255.0) fg = 255.0;

  if (fb < 0.0) fb = 0.0;
  else if (fb > 255.0) fb = 255.0;

  *r = (unsigned char) fr;
  *g = (unsigned char) fg;
  *b = (unsigned char) fb;

}

#ifdef SH_MEM

int gXErrorFlag = 0;

int HandleXError(dpy, event)
     Display *dpy;
     XErrorEvent *event;
{
  gXErrorFlag = 1;

  return 0;
}

int HandleXError();

void InstallXErrorHandler()
{
  XSetErrorHandler(HandleXError);
  XFlush(display);
}

void DeInstallXErrorHandler()
{
  XSetErrorHandler(NULL);
  XFlush(display);
}
#endif


/*
 *--------------------------------------------------------------
 *
 * ResizeDisplay --
 *
 *	Resizes display window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void ResizeDisplay(w, h)
     unsigned int w, h;
{

  if (ditherType == NO_DITHER || ditherType == PPM_DITHER) return;

  XResizeWindow(display, window, w, h);
  XFlush(display);
}


/*
 *--------------------------------------------------------------
 *
 * MakeWindow --
 *
 *	Create X Window
 *
 * Results:
 *	Read the code.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

#ifdef SH_MEM
int CompletionType = -1;
#endif

static int
MakeWindow(name) 
char *name;
{
  
  XSizeHints hint;
  unsigned int fg, bg;
  char *hello = "MPEG Play";
  int screen;
  Window CreateFullColorWindow();
  XVisualInfo vinfo;
  
  if ((ditherType == NO_DITHER) || (ditherType == PPM_DITHER)) return;

  display = XOpenDisplay(name);
  if (display == NULL) {
    fprintf(stderr, "Can not open display\n");
    exit(-2);
  }

#ifdef SH_MEM
  if(shmemFlag)
    CompletionType = XShmGetEventBase(display) + ShmCompletion;
#endif

  screen = DefaultScreen (display);
  
  /* Fill in hint structure */

  hint.x = 200;
  hint.y = 300;
  hint.width = 150;
  hint.height = 150;
  hint.flags = PPosition | PSize;
  
  /* Get some colors */
  
  bg = WhitePixel (display, screen);
  fg = BlackPixel (display, screen);
  
  /* Make the window */
  
  if (ditherType == FULL_COLOR_DITHER || ditherType == FULL_COLOR2_DITHER) {
    window = CreateFullColorWindow (display, hint.x, hint.y, hint.width, hint.height);
    if (window == 0) {
      fprintf (stderr, "-color option only valid on full color display\n");
      exit (-1);
    }
  } else {
    if (((XMatchVisualInfo (display, screen, 24, TrueColor,   &vinfo) != 0) ||
	 (XMatchVisualInfo (display, screen, 24, DirectColor, &vinfo) != 0)) &&
	(!quietFlag)) {
      printf("\nOn 24 bit displays:  use -dither color to get full color\n\t\tordered dither is the default.\n");
    }
    if (ditherType == MONO_DITHER || ditherType == MONO_THRESHOLD) {
      window = XCreateSimpleWindow (display,
				    DefaultRootWindow (display),
				    hint.x, hint.y,
				    hint.width, hint.height,
				    4, fg, bg);
      matched_depth = 1;
    } else {
      Visual *vis;
      XSetWindowAttributes attrib;
      unsigned long attrib_flags=0;
      
      if (!XMatchVisualInfo (display, screen, matched_depth = 8, PseudoColor, 
			     &vinfo)) {
	if (ditherType != GRAY_DITHER && ditherType != GRAY2_DITHER &&
	    ditherType != GRAY256_DITHER && ditherType != GRAY2562_DITHER) {
	  fprintf(stderr, "specified dither requires 8 bit display\n");
	  return 0;
	} else if (!XMatchVisualInfo(display, screen, matched_depth = 32,
			GrayScale, &vinfo) &&
	           !XMatchVisualInfo(display, screen, matched_depth = 24,
			GrayScale, &vinfo) &&
	           !XMatchVisualInfo(display, screen, matched_depth = 16,
			GrayScale, &vinfo) &&
	           !XMatchVisualInfo(display, screen, matched_depth = 8,
			GrayScale, &vinfo) &&
	           !XMatchVisualInfo(display, screen, matched_depth = 32,
			TrueColor, &vinfo) &&
	           !XMatchVisualInfo(display, screen, matched_depth = 24,
			TrueColor, &vinfo) &&
	           !XMatchVisualInfo(display, screen, matched_depth = 16,
			TrueColor, &vinfo)) {
	  fprintf(stderr, "- -dither gray requires at least 8 bit display\n");
	  exit(-1);
	}
      }
      
      vis=vinfo.visual;
      if (XDefaultDepthOfScreen(XDefaultScreenOfDisplay(display)) != 8) {
	attrib_flags |= CWColormap;
	attrib.colormap = XCreateColormap(display, DefaultRootWindow(display),
					  vis, AllocNone);
	owncmFlag = TRUE; 
      }
      
      attrib.background_pixel = bg;
      attrib.border_pixel = fg;
      attrib.backing_store = NotUseful;
      attrib.save_under = False;
      attrib.background_pixel = bg;
      attrib.border_pixel = bg;
      attrib_flags |= CWBackPixel | CWBorderPixel | CWBackingStore | CWSaveUnder;
      window = XCreateWindow (display,
			      DefaultRootWindow (display),
			      hint.x, hint.y,
			      hint.width, hint.height, 4,
			      matched_depth, InputOutput, vis,
			      attrib_flags, &attrib);
    }}
  
  XSelectInput(display, window, StructureNotifyMask);
  
  /* Tell other applications about this window */
  
  XSetStandardProperties (display, window, hello, hello, None, NULL, 0, &hint);
  
  /* Map window. */
  
  XMapWindow(display, window);
  
  /* Wait for map. */
  while(1) {
    XEvent	xev;
    
    XNextEvent(display, &xev);
    if(xev.type == MapNotify && xev.xmap.event == window)
      break;
  }
  
  XSelectInput(display, window, NoEventMask);

  return TRUE;
}
  

/*
 *--------------------------------------------------------------
 *
 * InitDisplay --
 *
 *	Initialized display, sets up colormap, etc.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void InitDisplay(name)
char *name;
{

  int ncolors = LUM_RANGE*CB_RANGE*CR_RANGE;
  XColor xcolor;
  int i, lum_num, cr_num, cb_num;
  unsigned char r, g, b;
  Colormap dcmap;

  if ((ditherType == NO_DITHER) || (ditherType == PPM_DITHER)) return;
  if (noDisplayFlag) return;

  if (!MakeWindow(name)) {
    /* Could not do that dither.  Try again if can */
    switch (ditherType) {
    case HYBRID_DITHER:
    case HYBRID2_DITHER:
    case FS4_DITHER:
    case FS2_DITHER:
    case FS2FAST_DITHER:
    case Twox2_DITHER:
    case ORDERED_DITHER:
    case ORDERED2_DITHER:
    case MBORDERED_DITHER:
      fprintf(stderr, "trying -dither color\n");
      ditherType = FULL_COLOR_DITHER;
      InitColorDisplay(name);
      InitColorDither(matched_depth == 32);
      return;

    case GRAY_DITHER:
    case GRAY2_DITHER:
    case GRAY256_DITHER:
    case GRAY2562_DITHER:
    case FULL_COLOR_DITHER:
    case FULL_COLOR2_DITHER:
    case MONO_DITHER:
    case MONO_THRESHOLD:
    default:
      /* cant do anything */
      exit(-1);
  }
}

  gc = XCreateGC(display, window, 0, 0);

  dcmap = cmap = XDefaultColormap(display, DefaultScreen(display));

  xcolor.flags = DoRed | DoGreen | DoBlue;

  if (owncmFlag) goto create_map;
  retry_alloc_colors:
  for (i=0; i<ncolors; i++) {

    lum_num = (i / (CR_RANGE*CB_RANGE))%LUM_RANGE;
    cr_num = (i / CB_RANGE)%CR_RANGE;
    cb_num = i % CB_RANGE;

    ConvertColor(lum_num, cr_num, cb_num, &r, &g, &b);

    xcolor.red = r * 256;
    xcolor.green = g * 256;
    xcolor.blue = b * 256;

    if (XAllocColor(display, cmap, &xcolor) == 0 && cmap == dcmap) {
      int j;
      unsigned long tmp_pixel;
      XWindowAttributes xwa;

      if (!quietFlag) {
        fprintf(stderr, "Using private colormap.\n");
      }

      /* Free colors. */
      for(j = 0; j < i; j ++) {
        tmp_pixel = wpixel[j];
        XFreeColors(display, cmap, &tmp_pixel, 1, 0);
      }

      create_map:
      XGetWindowAttributes(display, window, &xwa);
      cmap = XCreateColormap(display, window, xwa.visual, AllocNone);
      XSetWindowColormap(display, window, cmap);

      goto retry_alloc_colors;
    }
    pixel[i] = xcolor.pixel;
    wpixel[i] = xcolor.pixel;
  }

  ximage = NULL;
}


/*
 *--------------------------------------------------------------
 *
 * InitGrayDisplay --
 *
 *	Initialized display for gray scale dither.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void InitGrayDisplay(name)
char *name;
{
  int ncolors = 128;
  XColor xcolor;
  int i;
  Colormap dcmap;
  unsigned long tmp_pixels[256];

  MakeWindow(name);

  gc = XCreateGC(display, window, 0, 0);

  dcmap = cmap = XDefaultColormap(display, DefaultScreen(display));

  xcolor.flags = DoRed | DoGreen | DoBlue;

  if (owncmFlag) goto create_map;
  retry_alloc_grays:
  for (i=0; i<ncolors; i++) {

    xcolor.red = (i*2) * 256;
    xcolor.green = (i*2) * 256;
    xcolor.blue = (i*2) * 256;

    if(XAllocColor(display, cmap, &xcolor) == 0 && cmap == dcmap) {
      int j;
      XWindowAttributes xwa;

      if (!quietFlag) {
        fprintf(stderr, "Using private colormap.\n");
      }

      /* Free colors. */
      for(j = 0; j < i; j ++) {
        unsigned long tmp_pixel;
        tmp_pixel = tmp_pixels[j*2];
        XFreeColors(display, cmap, &tmp_pixel, 1, 0);
      }

      create_map:
      XGetWindowAttributes(display, window, &xwa);
      cmap = XCreateColormap(display, window, xwa.visual, AllocNone);
      XSetWindowColormap(display, window, cmap);

      goto retry_alloc_grays;
    }
    tmp_pixels[i*2] = pixel[i*2] = xcolor.pixel;
    tmp_pixels[(i*2)+1] = pixel[(i*2)+1] = xcolor.pixel;
    wpixel[(i*2)] = xcolor.pixel;
    wpixel[(i*2)+1] = xcolor.pixel;
    if(matched_depth == 8) {
      wpixel[i*2] |= wpixel[i*2] << 8;
      wpixel[i*2+1] |= wpixel[i*2+1] << 8;
    }
    if(matched_depth == 8 || matched_depth == 16) {
      wpixel[i*2] |= wpixel[i*2] << 16;
      wpixel[i*2+1] |= wpixel[i*2+1] << 16;
    }
#ifdef SIXTYFOUR_BIT
    if(matched_depth == 8 || matched_depth == 16 || matched_depth == 24 || matched_depth == 32) {
      wpixel[i*2] |= wpixel[i*2] << 32;
      wpixel[i*2+1] |= wpixel[i*2+1] << 32;
    }
#endif

  }

  ximage = NULL;
}


/*
 *--------------------------------------------------------------
 *
 * InitGray256Display --
 *
 *	Initialized display for gray scale dither with 256 levels
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */


void InitGray256Display(name)
char *name;
{
  int ncolors = 256;
  XColor xcolor;
  int i;
  Colormap dcmap;
  int result;
  XWindowAttributes xwa;
  unsigned long tmp_pixels[256];

  MakeWindow(name);

  gc = XCreateGC(display, window, 0, 0);

  dcmap = cmap = XDefaultColormap(display, DefaultScreen(display));

  xcolor.flags = DoRed | DoGreen | DoBlue;

  if (owncmFlag) {
    XGetWindowAttributes(display, window, &xwa);
    cmap = XCreateColormap(display, window, xwa.visual, AllocNone);
    XSetWindowColormap(display, window, cmap);
  }

  retry_alloc_grays:
  for (i=0; i<ncolors; i++) {
    xcolor.red = i * 256;
    xcolor.green = i * 256;
    xcolor.blue = i * 256;
    if((result=XAllocColor(display, cmap, &xcolor)) == 0 && cmap == dcmap) {
      int j;
      unsigned long tmp_pixel;

      if (!quietFlag) {
        fprintf(stderr, "Using private colormap.\n");
      }

      /* Free colors. */
      for(j = 0; j < i; j ++) {
        tmp_pixel = tmp_pixels[j];
        XFreeColors(display, cmap, &tmp_pixel, 1, 0);
      }

      XGetWindowAttributes(display, window, &xwa);
      cmap = XCreateColormap(display, window, xwa.visual, AllocNone);
      XSetWindowColormap(display, window, cmap);

      goto retry_alloc_grays;
    }
    tmp_pixels[i] = pixel[i] = xcolor.pixel;
    wpixel[i] = xcolor.pixel;
    if(matched_depth == 8) wpixel[i] |= wpixel[i] << 8;
    if(matched_depth == 8 || matched_depth == 16) {
      wpixel[i] |= wpixel[i] << 16;
    }
#ifdef SIXTYFOUR_BIT
    if(matched_depth == 8 || matched_depth == 16 || matched_depth == 24 || matched_depth == 32) {
      wpixel[i] |= wpixel[i] << 32;
    }
#endif

  }

  ximage = NULL;
}


/*
 *--------------------------------------------------------------
 *
 * InitMonoDisplay --
 *
 *	Initialized display for monochrome dither.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void InitMonoDisplay(name)
char *name;
{
  XGCValues xgcv;

  MakeWindow(name);

  xgcv.background = BlackPixel(display, DefaultScreen(display));
  xgcv.foreground = WhitePixel(display, DefaultScreen(display));

  gc = XCreateGC(display, window, GCForeground | GCBackground, &xgcv);

  ximage = NULL;
}



/*
 *--------------------------------------------------------------
 *
 * InitColorDisplay --
 *
 *	Initialized display for full color output.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

void InitColorDisplay(name)
char *name;
{
  XWindowAttributes winattr;
  MakeWindow(name);

  gc = XCreateGC(display, window, 0, 0);
  ximage = NULL;

  XGetWindowAttributes(display, window, &winattr);
  /*
   * Misuse of wpixel 
   */
  wpixel[0] = winattr.visual->red_mask;
  wpixel[1] = winattr.visual->green_mask;
  wpixel[2] = winattr.visual->blue_mask;
}


/*
 *--------------------------------------------------------------
 *
 * ExecuteDisplay --
 *
 *	Actually displays display plane in previously created window.
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
ExecuteDisplay(vid_stream)
     VidStream *vid_stream;
{
  char dummy;
  Visual *FindFullColorVisual();
  Visual *fc_visual;
  int depth;
  static int rate_deal = -1;
  static int one_frame_time;
  static struct timeval tftarget, tfnow;
  register int usec, sec;

  totNumFrames++;

  if (partialFlag) {
    if ((endFrame != -1) && (totNumFrames > endFrame)) {
#ifdef ANALYSIS
      PrintAllStats();
#endif
      PrintTimeInfo();
      
      if (loopFlag) {
              clear_data_stream(&vid_stream->buf_start,
			  &vid_stream->max_buf_length,
			  &vid_stream->buf_length, 
			  &vid_stream->buffer);
              longjmp(env, 1);
      }
      
      DestroyVidStream(curVidStream);
      exit(0);
    }
    if (totNumFrames < startFrame) {
      return;
    }
  }

    /* Do frame rate control */
  switch (rate_deal) {
  case 0:
    break;
  default:
    gettimeofday(&tfnow, (struct timezone *)NULL);
    usec = tftarget.tv_usec - tfnow.tv_usec;
    sec  = tftarget.tv_sec - tfnow.tv_sec;
    if (usec < 0) {
      usec += 1000000;
      sec--;
    }
    
    /* If we're not behind, wait a bit */
    if ((sec >= 0)  &&  usec > 0) {
      tfnow.tv_sec = sec;
      tfnow.tv_usec = usec;
      select(0, NULL, NULL, NULL ,&tfnow); 
      gettimeofday(&tfnow, (struct timezone *)NULL);
    }
    /* Setup target for next frame */
    tftarget.tv_usec = tfnow.tv_usec + one_frame_time;
    if (tftarget.tv_usec >= 1000000) {
      tftarget.tv_usec -= 1000000;
      tftarget.tv_sec = tfnow.tv_sec + 1;
    } else tftarget.tv_sec = tfnow.tv_sec;
    break;
  case -1:
    switch (framerate) {
    case -1: /* Go with stream Value */
      rate_deal = VidRateNum[vid_stream->picture_rate];
      gettimeofday(&tftarget, (struct timezone *)NULL);
      one_frame_time = 1000000 / rate_deal;
      break;
    case 0: /* as fast as possible */
      rate_deal = 0;
      break;
    default:
      rate_deal = framerate;
      gettimeofday(&tftarget, (struct timezone *)NULL);
      one_frame_time = 1000000 / rate_deal;
      break;
    }
    break;
  }

  if (!quietFlag) {
    fprintf (stderr, "%d\r", totNumFrames);
  }


  if (requireKeypressFlag) {
    char foo;
    printf("Press return (%d) ",totNumFrames);    
    while ((foo=getchar())!='\n');
  }

  if (ditherType == NO_DITHER) return;
  if (ditherType == PPM_DITHER) {
    ExecutePPM(vid_stream);
    return;
  }
  if (!noDisplayFlag) {
  if (ximage == NULL) {

    int pixels_per_mb = 16;
    if(IS_2x2_DITHER(ditherType)) pixels_per_mb = 32;
    
    if ((ditherType == FULL_COLOR_DITHER) ||
	       (ditherType == FULL_COLOR2_DITHER)) {
      int w, h;
      
      w = vid_stream->mb_width  * pixels_per_mb;
      h = vid_stream->mb_height * pixels_per_mb;
      
      fc_visual = FindFullColorVisual(display, &depth);
      ximage = XCreateImage (display, fc_visual, depth, ZPixmap,
			     0, &dummy, w, h, 32, 0);
      
    } else if (ditherType == MONO_DITHER || ditherType == MONO_THRESHOLD) {
      ximage = XCreateImage (display, None, matched_depth, XYBitmap, 0, &dummy,
			     vid_stream->mb_width * pixels_per_mb,
			     vid_stream->mb_height * pixels_per_mb, 8, 0);
      ximage->byte_order = MSBFirst;
      ximage->bitmap_bit_order = MSBFirst;
    } else {
      ximage = XCreateImage(display, None, matched_depth, ZPixmap, 0, &dummy,
			    vid_stream->mb_width * pixels_per_mb,
			    vid_stream->mb_height * pixels_per_mb, 8, 0);
    }
  }

/* 
 * Always work in native bit and byte order. This tells Xlib to reverse
 * bit and byte order if necessary when crossing a network. Frankly, this
 * part of XImages is somewhat underdocumented, so this may not be exactly
 * correct.
 */ 
#ifdef LITTLE_ENDIAN_ARCHITECTURE
    ximage->byte_order = LSBFirst;
    ximage->bitmap_bit_order = LSBFirst;
#else
    ximage->byte_order = MSBFirst;
    ximage->bitmap_bit_order = MSBFirst;
#endif

  
#ifdef SH_MEM
    if (shmemFlag) {
           XShmPutImage(display, window, gc, vid_stream->current->ximage, 
           0, 0, 0, 0,
           vid_stream->current->ximage->width, 
           vid_stream->current->ximage->height, True);
           XFlush(display);
      
      while(1) {
        XEvent xev;
	
        XNextEvent(display, &xev);
        if (xev.type == CompletionType) {
          break;
        }
      }
    }
    else 
#endif
      
      {
        ximage->data = (char *) vid_stream->current->display; 
	
        XPutImage(display, window, gc, ximage, 0, 0, 0, 0, ximage->width, ximage->height);
      }
  }
}


extern char *inputName;
extern char *strrchr();
#define PPM_BITS 8


/*
 *--------------------------------------------------------------
 *
 * ExecutePPM --
 *
 *	Write out a display plane as a PPM file.
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
ExecutePPM(vid_stream)
     VidStream *vid_stream;
{
  static int munged = 0;
  static char mungedInputName[300];
  char fileName[300];
  FILE *file;
  int n;
  unsigned int *p;
  unsigned int r, g, b;

  if (!munged) {
    char *cp;

    cp = strrchr(inputName, '/');
    if (cp != NULL)
      ++cp;
    else
      cp = inputName;
    strcpy(mungedInputName, cp);
    cp = strrchr(mungedInputName, '.');
    if (cp != NULL)
	*cp = '\0';
    munged = 1;
  }

  sprintf(fileName, "%s_%05d.ppm", mungedInputName, totNumFrames );
  file = fopen(fileName, "w");
  if (file == NULL) {
    perror(fileName);
    exit(1);
  }

  fprintf(file, "P6\n");
  fprintf(file, "%d %d\n", vid_stream->h_size, vid_stream->v_size);
  fprintf(file, "255\n");

  p = (unsigned int *) vid_stream->current->display;
  n = vid_stream->h_size * vid_stream->v_size;
  while (n > 0) {
    r = *p & 0xff;
    g = (*p >> PPM_BITS) & 0xff;
    b = (*p >> (2*PPM_BITS)) & 0xff;
    putc(r, file);
    putc(g, file);
    putc(b, file);
    ++p;
    --n;
  }

  fclose(file);
}


