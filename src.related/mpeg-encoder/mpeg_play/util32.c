/* 
 * util32.c --
 *
 *      Miscellaneous functions that deal with 32 bit color displays.
 *
 */

extern int matched_depth;

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

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "video.h"
#include "proto.h"


/*
 *--------------------------------------------------------------
 *
 * FindFullColorVisual
 *
 *  Returns a pointer to a full color bit visual on the display
 *
 * Results:
 *      See above.
 *  
 * Side effects:
 *      Unknown.
 *
 *--------------------------------------------------------------
 */
Visual *
FindFullColorVisual (dpy, depth)
     Display *dpy;
     int *depth;
{
  XVisualInfo vinfo;
  XVisualInfo *vinfo_ret;
  int numitems, maxdepth;
  
  vinfo.class = TrueColor;
  
  vinfo_ret = XGetVisualInfo(dpy, VisualClassMask, &vinfo, &numitems);
  
  if (numitems == 0) return NULL;

  maxdepth = 0;
  while(numitems > 0) {
    if (vinfo_ret[numitems-1].depth > maxdepth) {
      maxdepth = vinfo_ret[numitems-1 ].depth;
    }
    numitems--;
  }
  XFree(vinfo_ret);

  if (maxdepth < 16) return NULL;

  if (XMatchVisualInfo(dpy, DefaultScreen(dpy), maxdepth, 
		       TrueColor, &vinfo)) {
    *depth = maxdepth;
    return vinfo.visual;
  }
  
  return NULL;
}


/*
 *--------------------------------------------------------------
 *
 * CreateFullColorWindow
 *
 *  Creates a window capable of handling 32 bit color.
 *
 * Results:
 *      See above.
 *  
 * Side effects:
 *      Unknown.
 *
 *--------------------------------------------------------------
 */
Window
CreateFullColorWindow (dpy, x, y, w, h)
     Display *dpy;
     int x, y;
     unsigned int w, h;
{
  int depth;
  Visual *visual;
  XSetWindowAttributes xswa;
  unsigned int mask;
  unsigned int class;
  int screen;

  screen = XDefaultScreen(dpy);
  class = InputOutput;	/* Could be InputOnly */
  visual = FindFullColorVisual (dpy, &depth);
  matched_depth = depth;
  if(matched_depth == 24) matched_depth = 32;
  if (visual == NULL) {
    return 0;
  }
  mask = CWBackPixel | CWColormap | CWBorderPixel;
  xswa.colormap = XCreateColormap(dpy, XRootWindow(dpy, screen),
    visual, AllocNone);
  xswa.background_pixel = BlackPixel(dpy, DefaultScreen(dpy));
  xswa.border_pixel = WhitePixel(dpy, DefaultScreen(dpy));

  return XCreateWindow(dpy, RootWindow(dpy, screen), x, y, w, h,
    1, depth, class, visual, mask, &xswa);
}
