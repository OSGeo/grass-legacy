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

/* Range values for lum, cr, cb. */
int LUM_RANGE;
int CR_RANGE;
int CB_RANGE;

/* Array that remaps color numbers to actual pixel values used by X server. */

unsigned char pixel[256];

/* Arrays holding quantized value ranged for lum, cr, and cb. */

int *lum_values;
int *cr_values;
int *cb_values;

/* Declaration of global variable containing dither type. */

extern int ditherType;

/* Structures used by the X server. */

Display *display;

static XImage *ximage = NULL;
static Colormap cmap;
static Window window;
static GC gc;



/*
 *--------------------------------------------------------------
 *
 * InitColor --
 *
 *	Initialized lum, cr, and cb quantized range value arrays.
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
  int i;

  for (i=0; i<LUM_RANGE; i++) {
    lum_values[i] = ((i * 256) / (LUM_RANGE)) + (256/(LUM_RANGE*2));
  }

  for (i=0; i<CR_RANGE; i++) {
    cr_values[i] = ((i * 256) / (CR_RANGE)) + (256/(CR_RANGE*2));
  }

  for (i=0; i<CB_RANGE; i++) {
    cb_values[i] = ((i * 256) / (CB_RANGE)) + (256/(CB_RANGE*2));
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
     unsigned char l, cr, cb;
     unsigned char *r, *g, *b;
{
  double fl, fcr, fcb, fr, fg, fb;

  fl = 1.164*(((double) l)-16.0);
  fcr =  ((double) cr) - 128.0;
  fcb =  ((double) cb) - 128.0;


  fr = fl + (1.596 * fcb);
  fg = fl - (0.813 * fcb) - (0.391 * fcr);
  fb = fl + (2.018 * fcr);

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

void InstallXErrorHandler()
{
  int HandleXError();

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
     int w, h;
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

static void 
MakeWindow(name) 
char *name;
{
  
  XSizeHints hint;
  unsigned int fg, bg;
  char *hello = "MPEG Play";
  int screen;
  Window CreateFullColorWindow();

  if (ditherType == NO_DITHER || ditherType == PPM_DITHER) return;

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
  
  if (ditherType == FULL_COLOR_DITHER || ditherType==FULL_COLOR2_DITHER) {
    window = CreateFullColorWindow (display, hint.x, hint.y, hint.width, hint.height);
    if (window == 0) {
      fprintf (stderr, "-color option only valid on full color display\n");
      exit (-1);
    }
  } else if (ditherType == MONO_DITHER || ditherType == MONO_THRESHOLD) {
    window = XCreateSimpleWindow (display,
				  DefaultRootWindow (display),
				  hint.x, hint.y,
				  hint.width, hint.height,
				  4, fg, bg);
  } else {
    XVisualInfo vinfo;
    
    if (!XMatchVisualInfo (display, screen, 8, PseudoColor, 
			   &vinfo)) {

      if (!XMatchVisualInfo(display, screen, 8, GrayScale, 
			    &vinfo)) {

	fprintf(stderr, "-requires 8 bit display\n");
	exit(-1);
      }
    }

    window = XCreateSimpleWindow (display,
				 DefaultRootWindow (display),
				 hint.x, hint.y,
				 hint.width, hint.height,
				 4, fg, bg);
  }
  
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

  if (ditherType == NO_DITHER) return;
  if (noDisplayFlag) return;

  MakeWindow(name);

  gc = XCreateGC(display, window, 0, 0);

  dcmap = cmap = XDefaultColormap(display, DefaultScreen(display));

  xcolor.flags = DoRed | DoGreen | DoBlue;

  if (owncmFlag) goto create_map;
  retry_alloc_colors:
  for (i=0; i<ncolors; i++) {

    lum_num = (i / (CR_RANGE*CB_RANGE))%LUM_RANGE;
    cr_num = (i / CB_RANGE)%CR_RANGE;
    cb_num = i % CB_RANGE;

    ConvertColor(lum_values[lum_num], cr_values[cr_num], cb_values[cb_num], &r, &g, &b);

    xcolor.red = r * 256;
    xcolor.green = g * 256;
    xcolor.blue = b * 256;

    if(XAllocColor(display, cmap, &xcolor) == 0 && cmap == dcmap) {
      int j;
      long tmp_pixel;
      XWindowAttributes xwa;

      if (!quietFlag) {
	fprintf(stderr, "Using private colormap.\n");
      }

      /* Free colors. */
      for(j = 0; j < i; j ++) {
	tmp_pixel = pixel[j];
        XFreeColors(display, cmap, &tmp_pixel, 1, 0);
      }

      create_map:
      XGetWindowAttributes(display, window, &xwa);
      cmap = XCreateColormap(display, window, xwa.visual, AllocNone);
      XSetWindowColormap(display, window, cmap);

      goto retry_alloc_colors;
    }
    pixel[i] = xcolor.pixel;
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
      long tmp_pixel;
      XWindowAttributes xwa;

      if (!quietFlag) {
	fprintf(stderr, "Using private colormap.\n");
      }

      /* Free colors. */
      for(j = 0; j < i; j ++) {
	tmp_pixel = pixel[j*2];
        XFreeColors(display, cmap, &tmp_pixel, 1, 0);
      }

      create_map:
      XGetWindowAttributes(display, window, &xwa);
      cmap = XCreateColormap(display, window, xwa.visual, AllocNone);
      XSetWindowColormap(display, window, cmap);

      goto retry_alloc_grays;
    }
    pixel[(i*2)] = xcolor.pixel;
    pixel[(i*2)+1] = xcolor.pixel;
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
      long tmp_pixel;

      if (!quietFlag) {
	fprintf(stderr, "Using private colormap.\n");
      }

      /* Free colors. */
      for(j = 0; j < i; j ++) {
	tmp_pixel = pixel[j];
        XFreeColors(display, cmap, &tmp_pixel, 1, 0);
      }

      XGetWindowAttributes(display, window, &xwa);
      cmap = XCreateColormap(display, window, xwa.visual, AllocNone);
      XSetWindowColormap(display, window, cmap);

      goto retry_alloc_grays;
    }
    pixel[i] = xcolor.pixel;
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

  MakeWindow(name);

  gc = XCreateGC(display, window, 0, 0);
  ximage = NULL;
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

  totNumFrames++;
  if (!quietFlag) {
    fprintf (stderr, "%d\r", totNumFrames);
  }

  if (partialFlag)
    if (!((totNumFrames>=startFrame) && 
	  ((endFrame==-1) || (totNumFrames<=endFrame))))
      return;
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

  if (ximage == NULL) {
    
    if (ditherType == Twox2_DITHER) {
      ximage = XCreateImage(display, None, 8, ZPixmap, 0, &dummy,
			    vid_stream->mb_width * 32,
			    vid_stream->mb_height * 32, 8, 0);
    } else if (ditherType == FULL_COLOR_DITHER) {
      fc_visual = FindFullColorVisual(display, &depth);
      ximage = XCreateImage (display, fc_visual, depth, ZPixmap,
			     0, &dummy, vid_stream->mb_width * 16,
			     vid_stream->mb_height * 16, 32, 0);
    } else if (ditherType == FULL_COLOR2_DITHER) {
      fc_visual = FindFullColorVisual(display, &depth);
      ximage = XCreateImage (display, fc_visual, depth, ZPixmap,
			     0, &dummy, vid_stream->mb_width * 32,
			     vid_stream->mb_height * 32, 32, 0);
    } else if (ditherType == MONO_DITHER || ditherType == MONO_THRESHOLD) {
      ximage = XCreateImage (display, None, 1, XYBitmap, 0, &dummy,
			     vid_stream->mb_width * 16,
			     vid_stream->mb_height * 16, 8, 0);
      ximage->byte_order = MSBFirst;
      ximage->bitmap_bit_order = MSBFirst;
    } else {
      ximage = XCreateImage(display, None, 8, ZPixmap, 0, &dummy,
			    vid_stream->mb_width * 16,
			    vid_stream->mb_height * 16, 8, 0);
    }
  }
  
  if (!noDisplayFlag) {
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
	if(xev.type == CompletionType)
	  break;
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
#define BITS 8

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
#ifndef BGR
    r = *p & 0xff;
    g = (*p >> BITS) & 0xff;
    b = (*p >> (2*BITS)) & 0xff;
#else
    b = *p & 0xff;
    g = (*p >> BITS) & 0xff;
    r = (*p >> (2*BITS)) & 0xff;
#endif
    putc(r, file);
    putc(g, file);
    putc(b, file);
    ++p;
    --n;
  }

  fclose(file);
}


