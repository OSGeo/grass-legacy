/* * Last edited: Sep 26 11:56 1991 (mallet) */
/*
 * $Id: Pixmap.c,v 1.5 1991/09/27 17:32:23 mallet Exp $
 * 
 * Copyright 1991 Lionel Mallet
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appears in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Lionel MALLET not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  Lionel MALLET makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * Lionel MALLET DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS, IN NO EVENT SHALL Lionel MALLET BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION 
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  This software is opened and free. Furthermore, everybody is kindly
 * invited to participate to improve it for the benefit of all.
 * Improvements can be new features, bugs fixes and porting issues
 * resolution.
 *
 * Author:  Lionel Mallet, SIMULOG
 */

/*
 * $XConsortium: Pixmap.c,v 1.12 90/06/09 20:19:28 dmatic Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Davor Matic, MIT X Consortium
 */

static char rcsid[] = "$Id: Pixmap.c,v 1.5 1991/09/27 17:32:23 mallet Exp $";

#include <X11/IntrinsicP.h>
#include <X11/Xmu/Converters.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include <X11/Xos.h>
#include <X11/cursorfont.h>
#include "PixmapP.h"
#include "xgrass.h"
    
#define XtStrlen(s)                   ((s) ? strlen(s) : 0)
#define abs(x)                        (((x) > 0) ? (x) : -(x))
#define min(x, y)                     (((x) < (y)) ? (x) : (y))
#define max(x, y)                     (((x) > (y)) ? (x) : (y))


static Boolean _PWDEBUG = False;
static unsigned int depth;
static int screen;
static Display *dpy;

#define DefaultGridTolerance 5
#define DefaultPixmapWidth   32
#define DefaultPixmapHeight  32
#define DefaultStippled      TRUE
#define DefaultGrid          TRUE
#define DefaultResize        TRUE
#define DefaultProportional  TRUE
#define DefaultAxes          FALSE
#define DefaultDistance      10
#define DefaultSquareSize    20

static XtResource resources[] = {
#define offset(field) XtOffset(PixmapWidget, pixmap.field)
{XtNcursor, XtCCursor, XtRCursor, sizeof(Cursor),
   offset(cursor), XtRString, "tcross"},
{XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     offset(foreground_pixel), XtRString, XtDefaultForeground},
{XtNhighlight, XtCHighlight, XtRPixel, sizeof(Pixel),
     offset(highlight_pixel), XtRString, XtDefaultForeground},
{XtNframing, XtCFraming, XtRPixel, sizeof(Pixel),
     offset(framing_pixel), XtRString, XtDefaultForeground},
{XtNtransparent, XtCTransparent, XtRPixel, sizeof(Pixel),
     offset(transparent_pixel), XtRString, XtDefaultTransparent},
{XtNproportional, XtCProportional, XtRBoolean, sizeof(Boolean),
     offset(proportional), XtRImmediate, (XtPointer) DefaultProportional},
{XtNgrid, XtCGrid, XtRBoolean, sizeof(Boolean),
     offset(grid), XtRImmediate, (XtPointer) DefaultGrid},
{XtNgridTolerance, XtCGridTolerance, XtRDimension, sizeof(Dimension),
     offset(grid_tolerance), XtRImmediate, (XtPointer) DefaultGridTolerance},
{XtNstippled, XtCStippled, XtRBoolean, sizeof(Boolean),
     offset(stippled), XtRImmediate, (XtPointer) DefaultStippled},
{XtNaxes, XtCAxes, XtRBoolean, sizeof(Boolean),
     offset(axes), XtRImmediate, (XtPointer) DefaultAxes},
{XtNresize, XtCResize, XtRBoolean, sizeof(Boolean),
     offset(resize), XtRImmediate, (XtPointer) DefaultResize},
{XtNdistance, XtCDistance, XtRDimension, sizeof(Dimension),
     offset(distance), XtRImmediate, (XtPointer) DefaultDistance},
{XtNsquareSize, XtCSquareSize, XtRDimension, sizeof(Dimension),
     offset(squareH), XtRImmediate, (XtPointer) DefaultSquareSize},
{XtNsquareSize, XtCSquareSize, XtRDimension, sizeof(Dimension),
     offset(squareW), XtRImmediate, (XtPointer) DefaultSquareSize},
{XtNpixmapWidth, XtCPixmapWidth, XtRDimension, sizeof(Dimension),
     offset(width), XtRImmediate, (XtPointer) DefaultPixmapWidth},
{XtNpixmapHeight, XtCPixmapHeight, XtRDimension, sizeof(Dimension),
     offset(height), XtRImmediate, (XtPointer) DefaultPixmapHeight},
{XtNbutton1Action, XtCButton1Action, XtRInt, sizeof(int),
     offset(button_action[0]), XtRImmediate, (XtPointer) Set},
{XtNbutton2Action, XtCButton2Action, XtRInt, sizeof(int),
     offset(button_action[1]), XtRImmediate, (XtPointer) Invert},
{XtNbutton3Action, XtCButton3Action, XtRInt, sizeof(int),
     offset(button_action[2]), XtRImmediate, (XtPointer) Clear},
{XtNbutton4Action, XtCButton4Action, XtRInt, sizeof(int),
     offset(button_action[3]), XtRImmediate, (XtPointer) Clear},
{XtNbutton5Action, XtCButton5Action, XtRInt, sizeof(int),
     offset(button_action[4]), XtRImmediate, (XtPointer) Clear},
{XtNfilename, XtCFilename, XtRString, sizeof(String),
     offset(filename), XtRImmediate, (XtPointer) XtNscratch},
{XtNaddColorNtfyProc, XtCAddColorNtfyProc, XtRFunction, sizeof(AddColorNotifyProc), offset(AddColorNotify), XtRImmediate, (XtPointer) NULL},
{XtNstipple, XtCStipple, XtRBitmap, sizeof(Pixmap),
     offset(stipple), XtRImmediate, (XtPointer) XtUnspecifiedPixmap},
#undef offset
};

void PWTMark();
void PWTUnmark();
void PWTPaste();
void PWTSetColor();

static XtActionsRec actions[] =
{
{"mark",               PWTMark},
{"unmark",             PWTUnmark},
{"paste",              PWTPaste},
{"set-color",          PWTSetColor},
{"PW-debug",           PWDebug},
{"terminate",          PWTerminate},
{"store-to-buffer",    PWStoreToBuffer},
{"change-notify",      PWChangeNotify},
{"set-changed",        PWSetChanged},
{"up",                 PWUp},
{"down",               PWDown},
{"left",               PWLeft},
{"right",              PWRight},
{"fold",               PWFold},
{"flip-horiz",         PWFlipHoriz},
{"flip-vert",          PWFlipVert},
{"rotate-right",       PWRotateRight},
{"rotate-left",        PWRotateLeft},
{"set",                PWSet},
{"clear",              PWClear},
{"undo",               PWUndo},
{"redraw",             PWRedraw},
};

static char translations[] =
"\
 Ctrl<Btn1Down>:   mark()\n\
 Ctrl<Btn2Down>:   paste()\n\
 Ctrl<Btn3Down>:   unmark()\n\
 Shift<BtnUp>:    set-color()\n\
 Ctrl<Key>l:       redraw()\n\
 <Key>d:           PW-debug()\n\
 <Key>t:           terminate()\n\
 <Key>Up:          store-to-buffer()\
                   up()\
                   change-notify()\
                   set-changed()\n\
 <Key>Down:        store-to-buffer()\
                   down()\
                   change-notify()\
                   set-changed()\n\
 <Key>Left:        store-to-buffer()\
                   left()\
                   change-notify()\
                   set-changed()\n\
 <Key>Right:       store-to-buffer()\
                   right()\
                   change-notify()\
                   set-changed()\n\
 <Key>f:           store-to-buffer()\
                   fold()\
                   change-notify()\
                   set-changed()\n\
 <Key>h:           store-to-buffer()\
                   flip-horiz()\
                   change-notify()\
                   set-changed()\n\
 <Key>v:           store-to-buffer()\
                   flip-vert()\
                   change-notify()\
                   set-changed()\n\
 <Key>r:           store-to-buffer()\
                   rotate-right()\
                   change-notify()\
                   set-changed()\n\
 <Key>l:           store-to-buffer()\
                   rotate-left()\
                   change-notify()\
                   set-changed()\n\
 <Key>s:           store-to-buffer()\
                   set()\
                   change-notify()\
                   set-changed()\n\
 <Key>c:           store-to-buffer()\
                   clear()\
                   change-notify()\
                   set-changed()\n\
 <Key>u:           undo()\
                   change-notify()\
                   set-changed()\n\
";

Atom targets[] = {
    XA_BITMAP,
    XA_PIXMAP,
    XA_STRING,
};

#include "Requests.h"

static void ClassInitialize();
static void Initialize();
static void Redisplay();
static void Resize();
static void Destroy();
static Boolean SetValues();
 
PixmapClassRec pixmapClassRec = {
{   /* core fields */
    /* superclass		*/	(WidgetClass) &coreClassRec,
    /* class_name		*/	"Pixmap",
    /* widget_size		*/	sizeof(PixmapRec),
    /* class_initialize		*/	ClassInitialize,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	XtInheritRealize,
    /* actions			*/	actions,
    /* num_actions		*/	XtNumber(actions),
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	FALSE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	TRUE,
    /* destroy			*/	Destroy,
    /* resize			*/	Resize,
    /* expose			*/	Redisplay,
    /* set_values		*/	SetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	translations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL,
  },
  {
    /* targets                  */      targets,
    /* num_trets                */      XtNumber(targets),
    /* requests                 */      requests,
    /* num_requests             */      XtNumber(requests),
  }
};
 
WidgetClass pixmapWidgetClass = (WidgetClass) &pixmapClassRec;

Boolean PWQueryGrid(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;

    return PW->pixmap.grid;
}

Boolean PWQueryAxes(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;

    return PW->pixmap.axes;
}

Boolean PWQueryStored(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;
    
    return (PW->pixmap.storage != NULL);
}

#include "Graphics.c"
#include "Handlers.c"
#include "ReqMach.c"
#include "CutAndPaste.c"
    
void PWDebug(w)
    Widget w;
{
     _PWDEBUG ^= True;
     XSynchronize(dpy, _PWDEBUG);
}

void PWTSetColor(w, event)
     Widget w;
     XEvent *event;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  PWSetForeground(w, 
		  PWGetPxl(w, InPixmapX(PW, event->xbutton.x), 
			   InPixmapY(PW, event->xbutton.y)));
  if (PW->pixmap.colorNotify)
    (*PW->pixmap.colorNotify)(w, PW->pixmap.foreground_pixel);
}

void ColorNotify(w, proc)
     Widget w;
     UseColorNotifyProc proc;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  PW->pixmap.colorNotify = proc;
}

Pixmap PWGetPixmap(w) 
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;
 
    return GetPixmap(PW, PW->pixmap.zoom.image);
}

XImage *GetImage(PW, pixmap)
    PixmapWidget PW;
    Pixmap pixmap;
{
    Window root;
    int x, y;
    unsigned int width, height, border_width, depth;
    XImage *image;

    XGetGeometry(dpy, pixmap, &root, &x, &y,
		 &width, &height, &border_width, &depth);

    image = XGetImage(dpy, pixmap, x, y, width, height,
		      AllPlanes, ZPixmap);

    return image;
}

XImage *CreatePixmapImage(PW, width, height)
    PixmapWidget PW;
    Dimension width, height;
{
  Position x, y;
  int bitmap_pad;
  XImage *image;
 
  if (depth <= 8) bitmap_pad = 8;
  else if (depth <= 16) bitmap_pad = 16;
  else bitmap_pad = 32;
  
  image = XCreateImage(dpy,
		       DefaultVisual(dpy, screen),
		       depth,
		       ZPixmap, 0,
		       NULL, width, height,
		       bitmap_pad, 0);

  if (image) 
    {
      image->data = XtMalloc(image->bytes_per_line * height);
      if (!image->data) 
	{
	  XtWarning("Pixmap error in allocationg image data, no more space\n");
	  exit(1);
	}
    }
  else
    {
      XtWarning("Pixmap error in creating XImage\n");
      exit(1);
    }
  
  if (WhitePixel(dpy, screen) != 0) /* to clear the image, hope white is or 0 
				       or greater then black */
    XAddPixel(image, WhitePixel(dpy, screen)); /* because image pixels are 
						  initialized to 0 */
  
  if (_PWDEBUG) {
    printf("bytes_per_line %d\n", image->bytes_per_line);
    printf("size data %d\n", image->bytes_per_line * height);
    printf("bitmap pad %d\n", image->bitmap_pad);
  }
     
  return image;
}

void DestroyPixmapImage(image)
    XImage **image;
{
    if (*image) 
      {
	free((*image)->data);
	XDestroyImage(*image);
	*image = NULL;
      }
}

XImage *PWGetImage(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;

    return PW->pixmap.image;
}

void PWChangeNotify(w, client_data, call_data)
     Widget       w;
     XPointer      client_data;
     XPointer      call_data;
{
    PixmapWidget PW = (PixmapWidget) w;

    if (PW->pixmap.notify)
	(*PW->pixmap.notify)(w, client_data, call_data);
}

void Notify(w, proc)
     Widget   w;
     void   (*proc)();
{
    PixmapWidget PW = (PixmapWidget) w;

    PW->pixmap.notify = proc;
}

void PWSetChanged(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;
	
    PW->pixmap.changed = True;
}

Boolean PWQueryChanged(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;
	
    return PW->pixmap.changed;
}

void PWClearChanged(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;
    
    PW->pixmap.changed = False;
}

void PWSelect(w, from_x, from_y, to_x, to_y, time)
    Widget w;
    Position from_x, from_y,
	     to_x, to_y;
    Time time;
{
    PWMark(w, from_x, from_y, to_x, to_y);
    PWGrabSelection(w, time);
}

void PWSwitchAxes(w)
     Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;

    PW->pixmap.axes ^= True;
    PWHighlightAxes(w);
}

void PWAxes(w, _switch)
    Widget w;
    Boolean _switch;
{
    PixmapWidget PW = (PixmapWidget) w;
    
    if (PW->pixmap.axes != _switch)
	PWSwitchAxes(w);
}

void PWRedrawAxes(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;
    
    if (PW->pixmap.axes)
	PWHighlightAxes(w);
}

void PWPutImage(w, display, drawable, gc, x, y)
     PixmapWidget w;
     Display     *display;
     Drawable     drawable;
     GC           gc;
     Position     x, y;
{
    PixmapWidget PW = (PixmapWidget) w;

  XPutImage(display, drawable, gc, PW->pixmap.image,
	    0, 0, x, y, PW->pixmap.image->width, PW->pixmap.image->height);
}


String StripFilename(filename)
    String filename;
{
    char *begin = G_rindex (filename, '/');
    char *end, *result;
    int length;
    
    if (filename) {
	begin = (begin ? begin + 1 : filename);
	end = G_index (begin, '.'); /* change to rindex to allow longer names */
	length = (end ? (end - begin) : strlen (begin));
	result = (char *) XtMalloc (length + 1);
	strncpy (result, begin, length);
	result [length] = '\0';
	return (result);
    }
    else
	return (XtNdummy);
}

void DecodeTransparentPixels(pw, image, pix_mask)
     PixmapWidget pw;
     XImage *image;
     Pixmap pix_mask;
{
  if (pix_mask)
    {
      XImage *mask_image;
      Position x, y;

      mask_image = XGetImage(dpy, pix_mask, 0, 0,
			     image->width, image->height, AllPlanes, 
			     ZPixmap);

      for (x = 0; x < image->width; x++)
        for (y = 0; y < image->height; y++)
	  if (GetPxl(mask_image, x, y) == 0)
	    SetPxl(image, x, y, pw->pixmap.transparent_pixel);
    }
}


Pixmap ReadTransparentPixels(pw, image)
     PixmapWidget pw;
     XImage *image;
{
  XImage *mask_image;
  Pixmap pix;
  XGCValues gcv;
  GC gc;
  Position x, y;
  int needed = 0;

  mask_image = XCreateImage(dpy, DefaultVisual(dpy, screen), 1, ZPixmap, 0, 
			    NULL, image->width, image->height, 8, 0);
  mask_image->data = XtMalloc(mask_image->bytes_per_line*image->height);
  
  for (x = 0; x < image->width; x++)
    for (y = 0; y < image->height; y++)
      if (GetPxl(image, x, y) == pw->pixmap.transparent_pixel)
	needed++, SetPxl(mask_image, x, y, 0);
      else SetPxl(mask_image, x, y, ~0);
  
  if (!needed) return((Pixmap) 0);
  
  pix = XCreatePixmap(dpy, XtWindow(pw), image->width, image->height, 1);
  gcv.function = GXcopy;
  gc = XCreateGC(dpy, pix, GCFunction, &gcv);
  XPutImage(dpy, pix, gc, mask_image, 0, 0, 0, 0, image->width, image->height);

  DestroyPixmapImage(&mask_image);

  return(pix);
}

void ReadColorsInUse(pw, image, attribs, inuse)
     PixmapWidget pw;
     XImage *image;
     XpmAttributes *attribs;
     int **inuse;
{
  Position x, y;

  for (x = 0; x < image->width; x++)
    for (y = 0; y < image->height; y++)
      if (!(*inuse)[GetPxl(image,x, y)])
	{
	  (*inuse)[GetPxl(image, x, y)]++;
	  attribs->ncolors++;
	  if (GetPxl(image, x, y) == pw->pixmap.transparent_pixel)
	    attribs->mask_pixel = 1;
	}
}
    
  
void PWGetUnzoomedPixmap(w, pixmap, pixmap_mask)
     Widget w;
     Pixmap *pixmap, *pixmap_mask;
{
    PixmapWidget PW = (PixmapWidget) w;
    XGCValues gcv;
    GC gc ;
    
    if (PW->pixmap.zooming) 
      {    
	Pixmap zoom_pixmask;
	XImage *image_mask;
	
	*pixmap = XCreatePixmap(dpy, XtWindow(w), 
				PW->pixmap.zoom.image->width, 
				PW->pixmap.zoom.image->height, 
				depth);
        gcv.function = GXcopy;
        gc = XCreateGC(dpy, *pixmap, GCFunction, &gcv);
	
	XPutImage(dpy, *pixmap, gc, 
		  PW->pixmap.zoom.image, 
		  0, 0, 0, 0, 
		  PW->pixmap.zoom.image->width, 
		  PW->pixmap.zoom.image->height);
	XPutImage(dpy, *pixmap, gc, 
		  PW->pixmap.image, 
		  0, 0, 
		  PW->pixmap.zoom.at_x,
		  PW->pixmap.zoom.at_y,
		  PW->pixmap.image->width, 
		  PW->pixmap.image->height);

	image_mask = XGetImage(dpy, *pixmap, 0, 0, 
			       PW->pixmap.zoom.image->width,
			       PW->pixmap.zoom.image->height, AllPlanes,
			       ZPixmap);

	*pixmap_mask = ReadTransparentPixels(PW, image_mask);
	
        XFreeGC(dpy, gc);
        DestroyPixmapImage(&image_mask);
    }
    else {
	*pixmap = XCreatePixmap(dpy, XtWindow(w), 
			    PW->pixmap.image->width, 
			    PW->pixmap.image->height, 
			    depth);
        gcv.function = GXcopy;
        gc = XCreateGC(dpy, *pixmap, GCFunction, &gcv);

	XPutImage(dpy, *pixmap, gc, 
		  PW->pixmap.image, 
		  0, 0, 0, 0,
		  PW->pixmap.image->width, 
		  PW->pixmap.image->height);
        XFreeGC(dpy, gc);

	*pixmap_mask = ReadTransparentPixels(PW, PW->pixmap.image);
    }
}


void Refresh();

static void ClassInitialize()
{
  static XtConvertArgRec screenConvertArg[] = {
  {XtWidgetBaseOffset, (XPointer) XtOffset(Widget, core.screen),
       sizeof(Screen *)}
  };

  XtAddConverter(XtRString, XtRCursor, XmuCvtStringToCursor,
		 screenConvertArg, XtNumber(screenConvertArg));
		 
  XtAddConverter(XtRString, XtRBitmap, XmuCvtStringToBitmap,
		 screenConvertArg, XtNumber(screenConvertArg));
  
/*   XtAddConverter(XtRString, XtRPixmap, XmuCvtStringToPixmap,
		 screenConvertArg, XtNumber(screenConvertArg)); */
  
}

static void Initialize(request, new, argv, argc)
    PixmapWidget request, new;
    ArgList argv;
    Cardinal argc;
{
    XGCValues  values;
    XtGCMask   mask;

    dpy = XtDisplay(new);
    screen = DefaultScreen(dpy);
    depth = DefaultDepth(dpy,screen);

    new->pixmap.colorTable = (PWColorInfo **) calloc(1<<depth, 
						    sizeof(PWColorInfo*));
    
    new->pixmap.notify = NULL;
    new->pixmap.cardinal = 0;
    new->pixmap.current = 0;
    new->pixmap.fold = False;
    new->pixmap.changed = False;
    new->pixmap.zooming = False;
    new->pixmap.selection.own = False;
    new->pixmap.selection.limbo = False;
    new->pixmap.clear_pixel = WhitePixel(dpy, screen);

    new->pixmap.request_stack = (PWRequestStack *)
	XtMalloc(sizeof(PWRequestStack));
    new->pixmap.request_stack[0].request = NULL;
    new->pixmap.request_stack[0].call_data = NULL;
    new->pixmap.request_stack[0].trap = False;

    new->core.width = new->pixmap.width * new->pixmap.squareW + 
	2 * new->pixmap.distance;
    new->core.height = new->pixmap.height * new->pixmap.squareH + 
	2 * new->pixmap.distance;
  
    new->pixmap.hot.x = new->pixmap.hot.y = NotSet;
    new->pixmap.buffer_hot.x = new->pixmap.buffer_hot.y = NotSet;
    
    new->pixmap.mark.from_x = new->pixmap.mark.from_y = NotSet;
    new->pixmap.mark.to_x = new->pixmap.mark.to_y = NotSet;
    new->pixmap.buffer_mark.from_x = new->pixmap.buffer_mark.from_y = NotSet;
    new->pixmap.buffer_mark.to_x = new->pixmap.buffer_mark.to_y = NotSet;

    values.foreground = new->pixmap.foreground_pixel;
    values.background = new->core.background_pixel;
    values.function = GXcopy;
    values.plane_mask = AllPlanes;
    
    mask = GCForeground | GCBackground | GCFunction | GCPlaneMask;
    new->pixmap.drawing_gc = XCreateGC(dpy, RootWindow(dpy,screen), 
				       mask, &values);

    values.foreground = new->pixmap.clear_pixel;
    values.background = new->core.background_pixel;
    values.function = GXcopy;
    values.plane_mask = AllPlanes;
    
    mask = GCForeground | GCBackground | GCFunction | GCPlaneMask;
    new->pixmap.clear_gc = XCreateGC(dpy, RootWindow(dpy,screen), 
				       mask, &values);

    values.foreground = new->pixmap.highlight_pixel;
    values.background = new->core.background_pixel;
    values.foreground ^= values.background;
    values.function = GXxor;
    values.plane_mask = AllPlanes;
    mask = GCForeground | GCBackground | GCFunction | GCPlaneMask;
    new->pixmap.highlighting_gc = XCreateGC(dpy, RootWindow(dpy, screen),
					    mask, &values);

    values.foreground = new->pixmap.framing_pixel;
    values.background = new->core.background_pixel;
    values.foreground ^= values.background;
    values.function = GXxor;
    values.plane_mask = AllPlanes;
    mask = GCForeground | GCBackground | GCFunction | GCPlaneMask;

    new->pixmap.framing_gc = XCreateGC(dpy, RootWindow(dpy, screen),
				       mask, &values);

    values.foreground = new->pixmap.framing_pixel;
    values.background = new->core.background_pixel;
    values.foreground ^= values.background;
    values.function = GXxor;
    values.plane_mask = AllPlanes;
    mask = GCForeground | GCBackground | GCFunction | GCPlaneMask;
    new->pixmap.axes_gc = XCreateGC(dpy, RootWindow(dpy, screen),
				    mask, &values);

    values.foreground = new->pixmap.transparent_pixel;
    values.background = new->pixmap.clear_pixel;
    values.function = GXcopy;
    mask = GCForeground | GCBackground | GCFunction;
    if (new->pixmap.stipple != XtUnspecifiedPixmap)
      {
	values.stipple = new->pixmap.stipple;
	mask |= GCStipple | GCFillStyle;
      }
    values.fill_style = (new->pixmap.stippled ? 
			 FillOpaqueStippled : FillSolid);
    new->pixmap.transparent_gc = XCreateGC(dpy, RootWindow(dpy, screen),
					   mask, &values);
    
    new->pixmap.storage = NULL;
    
    new->pixmap.image = CreatePixmapImage(new, 
					  new->pixmap.width,
					  new->pixmap.height);
    
    new->pixmap.buffer = CreatePixmapImage(new, 
					   new->pixmap.width,
					   new->pixmap.height);
    
    PWUseColorInTable(new, new->pixmap.transparent_pixel, NULL,
		      NoColorName, NULL, NULL, NULL, NoColorName);
    if (new->pixmap.AddColorNotify != (AddColorNotifyProc) 0)
      new->pixmap.AddColorNotify(new, new->pixmap.transparent_pixel, 
				 NoColorName);

    /* Read file */
    {
	int status;
	Pixmap pix, pix_mask;
	XImage *image, *buffer;
	char  *buffer_data;
	XpmAttributes attribs;

	attribs.visual = DefaultVisual(dpy, screen);
	attribs.colormap = DefaultColormap(dpy, screen);
	attribs.depth = depth;
	attribs.colorsymbols = (XpmColorSymbol *)NULL;
	attribs.numsymbols = 0;
	attribs.valuemask = XpmVisual | XpmColormap | XpmDepth | 
	  XpmReturnPixels | XpmReturnInfos;
	
	status = XpmReadPixmapFile(dpy, RootWindow(dpy, screen), 
				 new->pixmap.filename, &pix, &pix_mask,
				 &attribs);
    
	if (status == XpmPixmapSuccess) 
	  {
	    
	    { /* Notify colors to be loaded with this pixmap */
	      int i, shift = 0, offset;
	      
	      switch (depth)
		{
		case 1:
		  offset = 2;
		  break;
		case 4:
		  offset = 3;
		  break;
		case 6:
		  offset = 4;
		  break;
		case 8:
		default:
		  offset = 5;
		  break;
		}
	      
	      for (i = 0; i < attribs.ncolors; i++)
		{
		  if (_PWDEBUG) {
		    printf("Pixel %d\n", *(attribs.pixels+i-shift));
		    printf("Color name %s\n", 
			   *(*(attribs.colorTable+i)+offset));
		    printf("colorTable[%d][0] %s\n", i,
			   **(attribs.colorTable+i));
		    printf("colorTable[%d][1] %s\n", i, 
			   *(*(attribs.colorTable+i)+1));
		    printf("colorTable[%d][2] %s\n", i, 
			   *(*(attribs.colorTable+i)+2));
		    printf("colorTable[%d][3] %s\n", i, 
			   *(*(attribs.colorTable+i)+3));
		    printf("colorTable[%d][4] %s\n", i, 
			   *(*(attribs.colorTable+i)+4));
		    printf("colorTable[%d][5] %s\n", i, 
			   *(*(attribs.colorTable+i)+5));
		  }

		  if ((attribs.mask_pixel != UNDEF_PIXEL) && 
		      (i == attribs.mask_pixel)) 
		    {
		      PWUpdateColorInTable(new, new->pixmap.transparent_pixel, 
					   *(*(attribs.colorTable+i)), 
					   *(*(attribs.colorTable+i)+1),
					   *(*(attribs.colorTable+i)+2),
					   *(*(attribs.colorTable+i)+3),
					   *(*(attribs.colorTable+i)+4),
					   *(*(attribs.colorTable+i)+5));
		      shift = 1;
		    }
		  else 
		    {
		      PWUseColorInTable(new, *(attribs.pixels+i-shift), 
					*(*(attribs.colorTable+i)), 
					*(*(attribs.colorTable+i)+1),
					*(*(attribs.colorTable+i)+2),
					*(*(attribs.colorTable+i)+3),
					*(*(attribs.colorTable+i)+4),
					*(*(attribs.colorTable+i)+5));
		      if (new->pixmap.AddColorNotify != (AddColorNotifyProc) 0)
			new->pixmap.AddColorNotify(new, 
						   *(attribs.pixels+i-shift), 
						   *(*(attribs.colorTable+i)
						     +offset));
		    }
		}
	      
	      if (new->pixmap.hints_cmt) XtFree(new->pixmap.hints_cmt);
	      if (attribs.hints_cmt) 
		new->pixmap.hints_cmt = XtNewString(attribs.hints_cmt);
	      if (new->pixmap.colors_cmt) XtFree(new->pixmap.colors_cmt);
	      if (attribs.colors_cmt) 
		new->pixmap.colors_cmt = XtNewString(attribs.colors_cmt);
	      if (new->pixmap.pixels_cmt) XtFree(new->pixmap.pixels_cmt);
	      if (attribs.pixels_cmt) 
		new->pixmap.pixels_cmt = XtNewString(attribs.pixels_cmt);
	      
	    }
	  
	    image = XGetImage(dpy, pix, 0, 0, 
		 	      attribs.width, attribs.height, AllPlanes, 
			      ZPixmap);

	    buffer = CreatePixmapImage(new, new->pixmap.image->width, 
				       new->pixmap.image->height);
	
	    TransferImageData(new->pixmap.image, buffer);
	
	    XFreePixmap(dpy, pix);
	    if (pix_mask) 
	      {
		DecodeTransparentPixels(new, image, pix_mask);
		XFreePixmap(dpy, pix_mask);
	      }
	
	    DestroyPixmapImage(&new->pixmap.image);
	    DestroyPixmapImage(&new->pixmap.buffer);
	
	    new->pixmap.image = image;
	    new->pixmap.buffer = buffer;
	    new->pixmap.width = attribs.width;
	    new->pixmap.height = attribs.height;
	    if (attribs.valuemask & XpmHotspot)
	      {
		new->pixmap.hot.x = attribs.x_hotspot;
		new->pixmap.hot.y = attribs.y_hotspot;	
	      }
	    
	    new->pixmap.changed = False;
	    new->pixmap.zooming = False;
	    XpmFreeAttributes(&attribs);
          }
    }

    Resize(new);
}

Boolean PWQueryMarked(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;

    return QuerySet(PW->pixmap.mark.from_x, PW->pixmap.mark.from_y);
}

Boolean PWQueryStippled(w)
     Widget w;
{
  PixmapWidget PW = (PixmapWidget) w;
  
  return(PW->pixmap.stippled);
}

void FixMark(PW)
    PixmapWidget PW;
{
    if (QuerySet(PW->pixmap.mark.from_x, PW->pixmap.mark.from_y)) {
	PW->pixmap.mark.from_x = min(PW->pixmap.mark.from_x, 
				     PW->pixmap.image->width);
	PW->pixmap.mark.from_y = min(PW->pixmap.mark.from_y, 
				     PW->pixmap.image->height);
	PW->pixmap.mark.to_x = min(PW->pixmap.mark.to_x, 
				   PW->pixmap.image->width);
	PW->pixmap.mark.to_y = min(PW->pixmap.mark.to_y, 
				   PW->pixmap.image->height);
	
	if((PW->pixmap.mark.from_x == PW->pixmap.mark.from_y) &&
	   (PW->pixmap.mark.to_x   == PW->pixmap.mark.to_y))
	    PW->pixmap.mark.from_x = 
		PW->pixmap.mark.from_y =
		    PW->pixmap.mark.to_x = 
			PW->pixmap.mark.to_y = NotSet;
    }
}

int PWStoreFile(w, filename)
    Widget w;
    String filename;
{
    PixmapWidget PW = (PixmapWidget) w;
    int status;
    Pixmap pix, pix_mask;
    XpmAttributes attribs;
    unsigned int width, height;
    
    attribs.visual = DefaultVisual(dpy, screen);
    attribs.colormap = DefaultColormap(dpy, screen);
    attribs.depth = depth;
    attribs.colorsymbols = (XpmColorSymbol *)NULL;
    attribs.numsymbols = 0;
    attribs.valuemask = XpmVisual | XpmColormap | XpmDepth | 
      XpmReturnPixels | XpmReturnInfos;
	
    status = XpmReadPixmapFile(dpy, XtWindow(PW), filename, &pix, &pix_mask,
			     &attribs);

    if (status == XpmPixmapSuccess) {

      { /* Notify colors to be loaded with this pixmap */
	int i, offset, shift = 0;
	
	switch (depth)
	  {
	  case 1:
	    offset = 2; /* BW display */
	    break;
	  case 4:
	    offset = 3; /* G4 display */
	    break;
	  case 6:
	    offset = 4; /* G6 display */
	    break;
	  case 8:
	  default:
	    offset = 5; /* Color display */
	    break;
	  }
	
	for (i = 0; i < attribs.ncolors; i++)
	  {
	    if (_PWDEBUG) {
	      printf("Pixel %d\n", *(attribs.pixels+i-shift));
	      printf("Color name %s\n", *(*(attribs.colorTable+i)+offset));
	      printf("colorTable[%d][0] %s\n", i, **(attribs.colorTable+i));
	      printf("colorTable[%d][1] %s\n", i,*(*(attribs.colorTable+i)+1));
	      printf("colorTable[%d][2] %s\n", i,*(*(attribs.colorTable+i)+2));
	      printf("colorTable[%d][3] %s\n", i,*(*(attribs.colorTable+i)+3));
	      printf("colorTable[%d][4] %s\n", i,*(*(attribs.colorTable+i)+4));
	      printf("colorTable[%d][5] %s\n", i,*(*(attribs.colorTable+i)+5));
	    }
	    
	    if ((attribs.mask_pixel != UNDEF_PIXEL) && 
		 (i == attribs.mask_pixel)) 
	      {
		PWUpdateColorInTable(PW, PW->pixmap.transparent_pixel, 
				     *(*(attribs.colorTable+i)), 
				     *(*(attribs.colorTable+i)+1),
				     *(*(attribs.colorTable+i)+2),
				     *(*(attribs.colorTable+i)+3),
				     *(*(attribs.colorTable+i)+4),
				     *(*(attribs.colorTable+i)+5));
		shift = 1;
	      }
	    else 
	      {
		PWUseColorInTable((Widget) PW, *(attribs.pixels+i-shift), 
				  *(*(attribs.colorTable+i)), 
				  *(*(attribs.colorTable+i)+1),
				  *(*(attribs.colorTable+i)+2),
				  *(*(attribs.colorTable+i)+3),
				  *(*(attribs.colorTable+i)+4),
				  *(*(attribs.colorTable+i)+5));
		if (PW->pixmap.AddColorNotify != (AddColorNotifyProc) 0)
		  PW->pixmap.AddColorNotify(PW, *(attribs.pixels+i-shift), 
					    *(*(attribs.colorTable+i)+offset));
	      }
	  }
      }
    
	DestroyPixmapImage(&PW->pixmap.storage);
	PW->pixmap.storage = XGetImage(dpy, pix, 0, 0, 
				       attribs.width, attribs.height, 
				       AllPlanes, ZPixmap);
	XFreePixmap(dpy, pix);
	if (pix_mask) 
	  {
	    DecodeTransparentPixels(PW, PW->pixmap.storage, pix_mask);
	    XFreePixmap(dpy, pix_mask);
	  }
    
	XpmFreeAttributes(&attribs);
	return XpmPixmapSuccess;
    }
    else
	XtWarning(" read file failed.  PixmapWidget");
    
    return status;
}

String PWUnparseStatus(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;
    
    sprintf(PW->pixmap.status, 
	    "Filename: %s Size:%dx%d",
	    PW->pixmap.filename, PW->pixmap.width, PW->pixmap.height);

    return PW->pixmap.status;
}

    
void PWChangeFilename(w, str)
    Widget w;
    String str;
{
    PixmapWidget PW = (PixmapWidget) w;
    
    if (str)
	if (strcmp(str, "")) {
	    XtFree(PW->pixmap.filename);
	    PW->pixmap.filename = XtNewString( str);
	}
}


/* Warning this function sets or gets the pixmap comments 
 * if passing null, a comment is returned, otherwise, it is stored 
 * memory is allocated for returned comments, should be free by application */
void PWComments(w, hints_cmt, colors_cmt, pixels_cmt)
     Widget w;
     char **hints_cmt, **colors_cmt, **pixels_cmt;
     
{
  PixmapWidget PW = (PixmapWidget)w;
  
  if ((*hints_cmt) && (PW->pixmap.hints_cmt))
    {
      XtFree(PW->pixmap.hints_cmt);
      PW->pixmap.hints_cmt = XtNewString(*hints_cmt);
    }
  else if (*hints_cmt) PW->pixmap.hints_cmt = XtNewString(*hints_cmt);
  else *hints_cmt = XtNewString(PW->pixmap.hints_cmt);

  if ((*colors_cmt) && (PW->pixmap.colors_cmt))
    {
      XtFree(PW->pixmap.colors_cmt);
      PW->pixmap.colors_cmt = XtNewString(*colors_cmt);
    }
  else if (*colors_cmt) PW->pixmap.colors_cmt = XtNewString(*colors_cmt);
  else *colors_cmt = XtNewString(PW->pixmap.colors_cmt);

  if ((*pixels_cmt) && (PW->pixmap.pixels_cmt))
    {
      XtFree(PW->pixmap.pixels_cmt);
      PW->pixmap.pixels_cmt = XtNewString(*pixels_cmt);
    }
  else if (*pixels_cmt) PW->pixmap.pixels_cmt = XtNewString(*pixels_cmt);
  else *pixels_cmt = XtNewString(PW->pixmap.pixels_cmt);
}

  
void PWAddColorNotifyProc(w, proc)
     Widget w;
     AddColorNotifyProc proc;
     
{
  PixmapWidget PW = (PixmapWidget) w;
  
  PW->pixmap.AddColorNotify = proc;
}


PWColorInfo **PWGetColorTable(w)
     Widget w;
     
{
  PixmapWidget PW = (PixmapWidget) w;
  
  return(PW->pixmap.colorTable);
}

void PWUseColorInTable(w, pixel, symbol, sname, mname, g4name, gname, cname)
     Widget w;
     Pixel pixel;
     char *symbol, *sname, *mname, *g4name, *gname, *cname;
     /* name are not used as is, instead memory is malloc'ed to fit in */
     
{
  PixmapWidget PW = (PixmapWidget) w;
  
  if (!PW->pixmap.colorTable[pixel])  /* not yet used color 
					 probably not in colorTable */
    {
      PW->pixmap.colorTable[pixel]=(PWColorInfo*)XtMalloc(sizeof(PWColorInfo));
      PW->pixmap.colorTable[pixel]->symbol = 0;
      PW->pixmap.colorTable[pixel]->s_name = 0;
      PW->pixmap.colorTable[pixel]->m_name = 0;
      PW->pixmap.colorTable[pixel]->g4_name = 0;
      PW->pixmap.colorTable[pixel]->g_name = 0;
      PW->pixmap.colorTable[pixel]->c_name = 0;
      PW->pixmap.colorTable[pixel]->pixel = pixel;
    }
  
  PWUpdateColorInTable(w, pixel, symbol, sname, mname, g4name, 
		       gname, cname);

}


void PWUpdateColorInTable(w, pixel, symbol, sname, mname, g4name, gname, cname)
     Widget w;
     Pixel pixel;
     char *symbol, *sname, *mname, *g4name, *gname, *cname;
     /* name are not used as is, instead memory is malloc'ed to fit in */
     
{
  PixmapWidget PW = (PixmapWidget) w;
  
  if (!PW->pixmap.colorTable[pixel]) return; /* inexistent color in Table */
  /* Update the color info in ColorTable */
  if ((symbol) && ((!PW->pixmap.colorTable[pixel]->symbol) || 
		   (strcmp(symbol, PW->pixmap.colorTable[pixel]->symbol))))
    {
      if (PW->pixmap.colorTable[pixel]->symbol) 
	XtFree(PW->pixmap.colorTable[pixel]->symbol);
      PW->pixmap.colorTable[pixel]->symbol = XtNewString(symbol);
    }
  if ((sname) && ((!PW->pixmap.colorTable[pixel]->s_name) ||
		  (strcmp(sname, PW->pixmap.colorTable[pixel]->s_name))))
    {
      if (PW->pixmap.colorTable[pixel]->s_name) 
	XtFree(PW->pixmap.colorTable[pixel]->s_name);
      PW->pixmap.colorTable[pixel]->s_name = XtNewString(sname);
    }
  if ((mname) && ((!PW->pixmap.colorTable[pixel]->m_name) ||
		  (strcmp(mname, PW->pixmap.colorTable[pixel]->m_name))))
    {
      if (PW->pixmap.colorTable[pixel]->m_name) 
	XtFree(PW->pixmap.colorTable[pixel]->m_name);
      PW->pixmap.colorTable[pixel]->m_name = XtNewString(mname);
    }
  if ((g4name) && ((!PW->pixmap.colorTable[pixel]->g4_name) ||
		   (strcmp(g4name, PW->pixmap.colorTable[pixel]->g4_name))))
    {
      if (PW->pixmap.colorTable[pixel]->g4_name) 
	XtFree(PW->pixmap.colorTable[pixel]->g4_name);
      PW->pixmap.colorTable[pixel]->g4_name = XtNewString(g4name);
    }
  if ((gname) && ((!PW->pixmap.colorTable[pixel]->g_name) ||
		  (strcmp(gname, PW->pixmap.colorTable[pixel]->g_name))))
    {
      if (PW->pixmap.colorTable[pixel]->g_name) 
	XtFree(PW->pixmap.colorTable[pixel]->g_name);
      PW->pixmap.colorTable[pixel]->g_name = XtNewString(gname);
    }
  if ((cname) && ((!PW->pixmap.colorTable[pixel]->c_name) ||
		   ((strcmp(cname, PW->pixmap.colorTable[pixel]->c_name)) &&
		    ((cname[0] != '#') || 
		     (PW->pixmap.colorTable[pixel]->c_name[0] == '#')))))
    {
      if (strcasecmp(cname, NoColorName) == 0)
	{ /* change transparent_pixel if NotSet */
	  /* this test is already performed by the interface */
	  if ((pixel != PW->pixmap.transparent_pixel) && 
	      (PW->pixmap.transparent_pixel != NotSet)) return;
	  
	  PW->pixmap.transparent_pixel = pixel;
	  XSetForeground(dpy, PW->pixmap.transparent_gc, pixel);
	  if (XtIsRealized(PW)) PWRedraw(PW);
	}
      else if (pixel == PW->pixmap.transparent_pixel) 
	{ /* undef transparent_pixel */
	  PW->pixmap.transparent_pixel = NotSet;
	  if (XtIsRealized(PW)) PWRedraw(PW);
	}
      
      if (PW->pixmap.colorTable[pixel]->c_name) 
	XtFree(PW->pixmap.colorTable[pixel]->c_name);
      PW->pixmap.colorTable[pixel]->c_name = XtNewString(cname);
    }
  else if ((!cname) && (!PW->pixmap.colorTable[pixel]->c_name))
    {
      XColor color;

      color.pixel = pixel;
      XQueryColor(dpy, DefaultColormap(dpy, screen), &color);
      
      PW->pixmap.colorTable[pixel]->c_name = (char *)XtMalloc(15*sizeof(char));
      sprintf(PW->pixmap.colorTable[pixel]->c_name, "#%04X%04X%04X", 
	      color.red, color.green, color.blue);
    }
  
}
    
int PWReadFile(w, filename)
    Widget w;
    String filename;
{
    PixmapWidget PW = (PixmapWidget) w;
    int status;
    Pixmap pix, pix_mask;
    XImage *image, *buffer;
    char  *buffer_data;
    XpmAttributes attribs;
    
    attribs.visual = DefaultVisual(dpy, screen);
    attribs.colormap = DefaultColormap(dpy, screen);
    attribs.depth = depth;
    attribs.colorsymbols = (XpmColorSymbol *)NULL;
    attribs.numsymbols = 0;
    attribs.valuemask = XpmVisual | XpmColormap | XpmDepth | 
      XpmReturnPixels | XpmReturnInfos;
	
    status = XpmReadPixmapFile(dpy, XtWindow(PW), filename, &pix, &pix_mask, 
			     &attribs);
      
    if (status == XpmPixmapSuccess) {
	
      { /* Notify colors to be loaded with this pixmap */
	int i, offset, shift = 0;
	
	switch (depth)
	  {
	  case 1:
	    offset = 2; /* BW display */
	    break;
	  case 4:
	    offset = 3; /* G4 display */
	    break;
	  case 6:
	    offset = 4; /* G6 display */
	    break;
	  case 8:
	  default:
	    offset = 5; /* Color display */
	    break;
	  }
	
	for (i = 0; i < attribs.ncolors; i++)
	  {
	    if (_PWDEBUG) {
	      printf("Pixel %d\n", *(attribs.pixels+i-shift));
	      printf("Color name %s\n", *(*(attribs.colorTable+i)+offset));
	      printf("colorTable[%d][0] %s\n", i,**(attribs.colorTable+i));
	      printf("colorTable[%d][1] %s\n", i,*(*(attribs.colorTable+i)+1));
	      printf("colorTable[%d][2] %s\n", i,*(*(attribs.colorTable+i)+2));
	      printf("colorTable[%d][3] %s\n", i,*(*(attribs.colorTable+i)+3));
	      printf("colorTable[%d][4] %s\n", i,*(*(attribs.colorTable+i)+4));
	      printf("colorTable[%d][5] %s\n", i,*(*(attribs.colorTable+i)+5));
	    }

	    if ((attribs.mask_pixel != UNDEF_PIXEL) && 
		(i == attribs.mask_pixel)) 
	      {
		PWUpdateColorInTable(PW, PW->pixmap.transparent_pixel, 
				     *(*(attribs.colorTable+i)), 
				     *(*(attribs.colorTable+i)+1),
				     *(*(attribs.colorTable+i)+2),
				     *(*(attribs.colorTable+i)+3),
				     *(*(attribs.colorTable+i)+4),
				     *(*(attribs.colorTable+i)+5));
		shift = 1;
	      }
	    else 
	      {
		PWUseColorInTable((Widget) PW, *(attribs.pixels+i-shift), 
				  *(*(attribs.colorTable+i)), 
				  *(*(attribs.colorTable+i)+1),
				  *(*(attribs.colorTable+i)+2),
				  *(*(attribs.colorTable+i)+3),
				  *(*(attribs.colorTable+i)+4),
				  *(*(attribs.colorTable+i)+5));
		if (PW->pixmap.AddColorNotify != (AddColorNotifyProc) 0)
		  PW->pixmap.AddColorNotify(PW, *(attribs.pixels+i-shift), 
					    *(*(attribs.colorTable+i)+offset));
	      }
	  }

	if (PW->pixmap.hints_cmt) XtFree(PW->pixmap.hints_cmt);
	if (attribs.hints_cmt) 
	  PW->pixmap.hints_cmt = XtNewString(attribs.hints_cmt);
	if (PW->pixmap.colors_cmt) XtFree(PW->pixmap.colors_cmt);
	if (attribs.colors_cmt) 
	  PW->pixmap.colors_cmt = XtNewString(attribs.colors_cmt);
	if (PW->pixmap.pixels_cmt) XtFree(PW->pixmap.pixels_cmt);
	if (attribs.pixels_cmt) 
	  PW->pixmap.pixels_cmt = XtNewString(attribs.pixels_cmt);

      }
    
	image = XGetImage(dpy, pix, 0, 0, attribs.width, attribs.height, 
			  AllPlanes, ZPixmap);
	buffer = CreatePixmapImage(PW, PW->pixmap.image->width, 
				   PW->pixmap.image->height);
	
	TransferImageData(PW->pixmap.image, buffer);
	
	XFreePixmap(dpy, pix);
        if (pix_mask) 
	  {
	    DecodeTransparentPixels(PW, image, pix_mask);
	    XFreePixmap(dpy, pix_mask);
	  }
      
	DestroyPixmapImage(&PW->pixmap.image);
	DestroyPixmapImage(&PW->pixmap.buffer);
	
	PW->pixmap.image = image;
	PW->pixmap.buffer = buffer;
	PW->pixmap.width = attribs.width;
	PW->pixmap.height = attribs.height;
        if (attribs.valuemask & XpmHotspot)
	  {
	    PW->pixmap.hot.x = attribs.x_hotspot;
	    PW->pixmap.hot.y = attribs.y_hotspot;	
	  }
        else
	  {
	    PW->pixmap.hot.x = PW->pixmap.hot.y = NotSet;
	  }

	PW->pixmap.changed = False;
	PW->pixmap.zooming = False;
	
	XtFree(PW->pixmap.filename);
	PW->pixmap.filename = XtNewString(filename);

	PWUnmark(w);
	
	Resize(PW);

	if (PW->core.visible) {
	    XClearArea(dpy, XtWindow(PW),
		       0, 0, 
		       PW->core.width, PW->core.height,
		       True);
	}
	
	XpmFreeAttributes(&attribs);
	return XpmPixmapSuccess;
    }
    else
	XtWarning(" read file failed.  PixmapWidget");
    
    return status;
}

int PWSetImage(w, image)
    Widget w;
    XImage *image;
{
    PixmapWidget PW = (PixmapWidget) w;
    XImage *buffer;
    
    buffer = CreatePixmapImage(PW, 
			       (Dimension) image->width, 
			       (Dimension) image->height);
    
    TransferImageData(PW->pixmap.image, buffer);
    
    DestroyPixmapImage(&PW->pixmap.image);
    DestroyPixmapImage(&PW->pixmap.buffer);
    
    PW->pixmap.image = image;
    PW->pixmap.buffer = buffer;
    PW->pixmap.width = image->width;
    PW->pixmap.height = image->height;
    
    Resize(PW);
    
    if (PW->core.visible) {
	XClearArea(dpy, XtWindow(PW),
		   0, 0, 
		   PW->core.width, PW->core.height,
		   True);    
    }
}


XpmAttributes *fillXpmAttributesStruct(PW, image)
     PixmapWidget PW;
     XImage *image;
     
{
  XpmAttributes *attribs = (XpmAttributes *)XtMalloc(XpmAttributesSize());
  int i,j = 0, k = 0;
  int *InUse = (int *)calloc(1<<depth, sizeof(int));

  attribs->valuemask = XpmRgbFilename | XpmInfos;
  attribs->rgb_fname = XtNewString(rgb_fname);
  attribs->npixels = 0;
  attribs->cpp = 0;
  attribs->numsymbols = 0;
  attribs->colorsymbols = NULL;
  attribs->ncolors = 0;
  attribs->mask_pixel = 0;
  attribs->colormap = DefaultColormap(dpy, screen);
  attribs->valuemask |= XpmColormap;
  attribs->depth = depth;
  attribs->valuemask |= XpmDepth;
  attribs->width = image->width;
  attribs->height = image->height;
  attribs->valuemask |= XpmSize;
  
  
  /* compute number of colors in pixmap */
  ReadColorsInUse(PW, image, attribs, &InUse);
  /* now allocate memory space for colors */
  attribs->colorTable = (char ***)calloc(attribs->ncolors, sizeof(char **));
  if (attribs->mask_pixel)
    { /* store transparent color as first in colorTable */
      attribs->npixels = attribs->ncolors - 1;
      i = PW->pixmap.transparent_pixel;
      attribs->colorTable[0] = (char **)calloc(6, sizeof(char *));
      attribs->colorTable[0][0] = 
	XtNewString(PW->pixmap.colorTable[i]->symbol);
      attribs->colorTable[0][1] = 
	XtNewString(PW->pixmap.colorTable[i]->s_name);
      attribs->colorTable[0][2] = 
	XtNewString(PW->pixmap.colorTable[i]->m_name);
      attribs->colorTable[0][3] = 
	XtNewString(PW->pixmap.colorTable[i]->g4_name);
      attribs->colorTable[0][4] = 
	XtNewString(PW->pixmap.colorTable[i]->g_name);
      attribs->colorTable[0][5] = 
	XtNewString(PW->pixmap.colorTable[i]->c_name);
      j = 1;
    }
  else attribs->npixels = attribs->ncolors;

  /* allocate space for pixels */
  attribs->pixels = (Pixel *)calloc(attribs->npixels, sizeof(Pixel));
  
  /* fill in pixels and colorTable */
  for (i = j; i < attribs->ncolors; i++)
    {
      for (k; k < 1<<depth; k++) /* find next used color */
	if ((InUse[k]) && (k != PW->pixmap.transparent_pixel)) break;
      
      attribs->pixels[i-j] = k; /* k should be equal to 
				   PW->pixmap.colorTable->pixel */
      attribs->colorTable[i] = (char **)calloc(6, sizeof(char *));
      attribs->colorTable[i][0] = 
	XtNewString(PW->pixmap.colorTable[k]->symbol);
      attribs->colorTable[i][1] = 
	XtNewString(PW->pixmap.colorTable[k]->s_name);
      attribs->colorTable[i][2] = 
	XtNewString(PW->pixmap.colorTable[k]->m_name);
      attribs->colorTable[i][3] = 
	XtNewString(PW->pixmap.colorTable[k]->g4_name);
      attribs->colorTable[i][4] = 
	XtNewString(PW->pixmap.colorTable[k]->g_name);
      attribs->colorTable[i][5] = 
	XtNewString(PW->pixmap.colorTable[k]->c_name);
      k++;
    }
  
  /* fill in comments */
  attribs->hints_cmt = XtNewString(PW->pixmap.hints_cmt);
  attribs->colors_cmt = XtNewString(PW->pixmap.colors_cmt);
  attribs->pixels_cmt = XtNewString(PW->pixmap.pixels_cmt);
  
  /* return struct */
  return(attribs);
}

	  
  
int PWWriteFile(w, filename)
    Widget w;
    String filename;
{
    PixmapWidget PW = (PixmapWidget) w;
    XImage *image;
    GC gc = NULL;
    XGCValues gcv;
    XPoint hot;
    Pixmap pix, pix_mask;
    int status;
    XpmAttributes *attribs;
    
    if (PW->pixmap.zooming) {
	image = CreatePixmapImage(PW,
				  (Dimension) PW->pixmap.zoom.image->width,
				  (Dimension) PW->pixmap.zoom.image->height);
	CopyImageData(PW->pixmap.zoom.image, image, 0, 0, 
		      (Dimension) PW->pixmap.zoom.image->width,
		      (Dimension) PW->pixmap.zoom.image->height, 0, 0);
	CopyImageData(PW->pixmap.image, image, 
		      0, 0, 
		      PW->pixmap.image->width - 1,
		      PW->pixmap.image->height - 1,
		      PW->pixmap.zoom.at_x, PW->pixmap.zoom.at_y);

	if (QuerySet(PW->pixmap.hot.x, PW->pixmap.hot.y))
	  {
	    hot.x = PW->pixmap.hot.x + PW->pixmap.zoom.at_x;
	    hot.y = PW->pixmap.hot.y + PW->pixmap.zoom.at_y;
	  }
	else hot = PW->pixmap.zoom.hot;
    }
    else {
	image = PW->pixmap.image;
	hot = PW->pixmap.hot;
    }
    
    if (filename) 
      {
	XtFree(PW->pixmap.filename);
	PW->pixmap.filename = XtNewString(filename);
      }

    if (_PWDEBUG)
	fprintf(stderr, "Saving filename: %s\n", filename);

    attribs = fillXpmAttributesStruct(PW, image);
    
    if (QuerySet(hot.x, hot.y))
      {
	attribs->valuemask |= XpmHotspot;
	attribs->x_hotspot = hot.x;
	attribs->y_hotspot = hot.y;
      }

    pix = XCreatePixmap(dpy, XtWindow(PW), image->width, image->height, depth);
    gcv.function = GXcopy;
    gc = XCreateGC(dpy, pix, GCFunction, &gcv);
    XPutImage(dpy, pix, gc, image, 0, 0, 0, 0, 
	      image->width, image->height);

    if (attribs->mask_pixel)
      {
	attribs->mask_pixel = 0; /* transparent color is always first in 
				    colorTable */
	pix_mask = ReadTransparentPixels(PW, image);
      }
    else 
      {
	pix_mask = (Pixmap)0;
	attribs->mask_pixel = UNDEF_PIXEL;
      }

    status = XpmWritePixmapFile(dpy,PW->pixmap.filename, 
				pix, pix_mask, attribs);

    XpmFreeAttributes(attribs);
    XFreePixmap(dpy, pix);
    if (pix_mask) XFreePixmap(dpy, pix_mask);
    XFreeGC(dpy, gc);
    
    if (PW->pixmap.zooming)
	DestroyPixmapImage(&image);
    
    if (status == XpmPixmapSuccess)
      PW->pixmap.changed = False;
    
    return status;
}

String PWGetFilename(w, str)
    Widget w;
    String *str;
{
    PixmapWidget PW = (PixmapWidget) w;
    
    *str = XtNewString(PW->pixmap.filename);

    return *str;
}

String PWGetFilepath(w, str)
    Widget w;
    String *str;
{
    PixmapWidget PW = (PixmapWidget) w;
    String end;

    *str = XtNewString(PW->pixmap.filename);
    end = G_rindex(*str, '/');

    if (end)
	*(end + 1) = '\0';
    else 
	**str = '\0';

    return *str;
}


void FixHotSpot(PW)
    PixmapWidget PW;
{
    if (!QueryInPixmap(PW, PW->pixmap.hot.x, PW->pixmap.hot.y))
	PW->pixmap.hot.x = PW->pixmap.hot.y = NotSet;
}


void ZoomOut(PW)
    PixmapWidget PW;
{
    CopyImageData(PW->pixmap.image, PW->pixmap.zoom.image, 
		  0, 0, 
		  PW->pixmap.image->width - 1,
		  PW->pixmap.image->height - 1,
		  PW->pixmap.zoom.at_x, PW->pixmap.zoom.at_y);
    
    DestroyPixmapImage(&PW->pixmap.image);
    DestroyPixmapImage(&PW->pixmap.buffer);
    
    PW->pixmap.image = PW->pixmap.zoom.image;
    PW->pixmap.buffer = PW->pixmap.zoom.buffer;
    PW->pixmap.width = PW->pixmap.image->width;
    PW->pixmap.height = PW->pixmap.image->height;
    PW->pixmap.fold = PW->pixmap.zoom.fold;
    PW->pixmap.changed |= PW->pixmap.zoom.changed;
    PW->pixmap.grid = PW->pixmap.zoom.grid;

    if (QuerySet(PW->pixmap.hot.x, PW->pixmap.hot.y)) {
	PW->pixmap.hot.x += PW->pixmap.zoom.at_x;
	PW->pixmap.hot.y += PW->pixmap.zoom.at_y;
    }
    else
	PW->pixmap.hot = PW->pixmap.zoom.hot;

    PW->pixmap.mark.from_x = NotSet;
    PW->pixmap.mark.from_y = NotSet;
    PW->pixmap.mark.to_x = NotSet;
    PW->pixmap.mark.to_y = NotSet;
    PW->pixmap.zooming = False;
}    

void PWZoomOut(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;
    
    if (PW->pixmap.zooming) {
	ZoomOut(PW);
	
	Resize(PW);
	if (PW->core.visible)
	    XClearArea(dpy, XtWindow(PW),
		       0, 0, 
		       PW->core.width, PW->core.height,
		       True);
    }
}

void PWZoomIn();

void PWZoomMarked(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;

    PWZoomIn(w, 
	     PW->pixmap.mark.from_x, PW->pixmap.mark.from_y,
	     PW->pixmap.mark.to_x,   PW->pixmap.mark.to_y);
}

void PWZoomIn(w, from_x, from_y, to_x, to_y)
    Widget w;
    Position from_x, from_y,
	     to_x, to_y;
{
    PixmapWidget PW = (PixmapWidget) w;
    XImage *image, *buffer;    
    Dimension width, height;
  
    if (PW->pixmap.zooming)
	ZoomOut(PW);
    
    QuerySwap(from_x, to_x);
    QuerySwap(from_y, to_y);
    from_x = max(0, from_x);
    from_y = max(0, from_y);
    to_x = min(PW->pixmap.width - 1, to_x);
    to_y = min(PW->pixmap.height - 1, to_y);
    
    width = to_x - from_x + 1;
    height = to_y - from_y + 1;

    image = CreatePixmapImage(PW, width, height);
    buffer = CreatePixmapImage(PW, width, height);

    CopyImageData(PW->pixmap.image, image, from_x, from_y, to_x, to_y, 0, 0);
    CopyImageData(PW->pixmap.buffer, buffer, from_x, from_y, to_x, to_y, 0, 0);
    
    PW->pixmap.zoom.image = PW->pixmap.image;
    PW->pixmap.zoom.buffer = PW->pixmap.buffer;
    PW->pixmap.zoom.at_x = from_x;
    PW->pixmap.zoom.at_y = from_y;
    PW->pixmap.zoom.fold = PW->pixmap.fold;
    PW->pixmap.zoom.changed = PW->pixmap.changed;
    PW->pixmap.zoom.hot = PW->pixmap.hot;
    PW->pixmap.zoom.grid = PW->pixmap.grid;

    PW->pixmap.image = image;
    PW->pixmap.buffer = buffer;
    PW->pixmap.width = width;
    PW->pixmap.height = height;
    PW->pixmap.changed = False;
    PW->pixmap.hot.x -= from_x;
    PW->pixmap.hot.y -= from_y;
    PW->pixmap.mark.from_x = NotSet;
    PW->pixmap.mark.from_y = NotSet;
    PW->pixmap.mark.to_x = NotSet;
    PW->pixmap.mark.to_y = NotSet;
    PW->pixmap.zooming = True;
    PW->pixmap.grid = True; /* potencially true, could use a resource here */

    FixHotSpot(PW);
    
    Resize(PW);
    if (PW->core.visible)
	XClearArea(dpy, XtWindow(PW),
		   0, 0, 
		   PW->core.width, PW->core.height,
		   True);
}

XImage *ScalePixmapImage();

void PWRescale(w, width, height)
    Widget w;
    Dimension width, height;
{
    PixmapWidget PW = (PixmapWidget) w;
    XImage *image, *buffer;

    if (PW->pixmap.zooming)
	ZoomOut(PW);
        
    image = ScalePixmapImage(PW, PW->pixmap.image, 
		       (double) width / (double) PW->pixmap.image->width,
		       (double) height / (double) PW->pixmap.image->height);

    buffer = CreatePixmapImage(PW, (Dimension) image->width, 
			       (Dimension) image->height);
    
    TransferImageData(PW->pixmap.buffer, buffer);

    DestroyPixmapImage(&PW->pixmap.image);
    DestroyPixmapImage(&PW->pixmap.buffer);
    
    PW->pixmap.image = image;
    PW->pixmap.buffer = buffer;
    PW->pixmap.width = image->width;
    PW->pixmap.height = image->height;
    
    FixHotSpot(PW);
    FixMark(PW);

    Resize(PW);
    if (PW->core.visible)
	XClearArea(dpy, XtWindow(PW),
		   0, 0, 
		   PW->core.width, PW->core.height,
		   True);
}

Boolean PWQueryZooming(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;

    return PW->pixmap.zooming;
}

void PWResize(w, width, height)
    Widget w;
    Dimension width, height;
{
    PixmapWidget PW = (PixmapWidget) w;
    XImage *image, *buffer;

    if (PW->pixmap.zooming)
	ZoomOut(PW);

    image = CreatePixmapImage(PW, width, height);
    buffer = CreatePixmapImage(PW, width, height);

    TransferImageData(PW->pixmap.image, image);
    TransferImageData(PW->pixmap.buffer, buffer);
    
    DestroyPixmapImage(&PW->pixmap.image);
    DestroyPixmapImage(&PW->pixmap.buffer);

    PW->pixmap.image = image;
    PW->pixmap.buffer = buffer;
    PW->pixmap.width = width;
    PW->pixmap.height = height;

    FixHotSpot(PW);
    FixMark(PW);

    Resize(PW);
    if (PW->core.visible)
	XClearArea(dpy, XtWindow(PW),
		   0, 0, 
		   PW->core.width, PW->core.height,
		   True);
}

static void Destroy(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;
    int i;
    
    /* free colorTable */
    for (i = 0; i < 1<<depth; i++)
      if (PW->pixmap.colorTable[i])
	{
	  if (PW->pixmap.colorTable[i]->symbol) 
	    free(PW->pixmap.colorTable[i]->symbol);
	  if (PW->pixmap.colorTable[i]->s_name) 
	    free(PW->pixmap.colorTable[i]->s_name);
	  if (PW->pixmap.colorTable[i]->m_name) 
	    free(PW->pixmap.colorTable[i]->m_name);
	  if (PW->pixmap.colorTable[i]->g4_name) 
	    free(PW->pixmap.colorTable[i]->g4_name);
	  if (PW->pixmap.colorTable[i]->g_name) 
	    free(PW->pixmap.colorTable[i]->g_name);
	  if (PW->pixmap.colorTable[i]->c_name) 
	    free(PW->pixmap.colorTable[i]->c_name);
	  free(PW->pixmap.colorTable[i]);
	}
    cfree(PW->pixmap.colorTable);

    XtDestroyGC(PW->pixmap.drawing_gc);
    XtDestroyGC(PW->pixmap.highlighting_gc);
    XtDestroyGC(PW->pixmap.framing_gc);
    XtDestroyGC(PW->pixmap.clear_gc);
    XtDestroyGC(PW->pixmap.axes_gc);
    XtDestroyGC(PW->pixmap.transparent_gc);

    DestroyPixmapImage(&PW->pixmap.image);
    DestroyPixmapImage(&PW->pixmap.buffer);
    
    PWRemoveAllRequests(w);
}


static void Resize(PW)
     PixmapWidget PW;
{
    Dimension squareW, squareH;

    if (PW->pixmap.resize == True) 
      {
	squareW = max(1, 
		      ((int)PW->core.width - 2 * (int)PW->pixmap.distance) / 
		      (int)PW->pixmap.width);
	squareH = max(1, 
		      ((int)PW->core.height - 2 * (int)PW->pixmap.distance) / 
		      (int)PW->pixmap.height);

	if (PW->pixmap.proportional)
	  PW->pixmap.squareW = PW->pixmap.squareH = min(squareW, squareH);
	else {
	  PW->pixmap.squareW = squareW;
	  PW->pixmap.squareH = squareH;
	}
      }
    
    PW->pixmap.horizOffset = max((Position)PW->pixmap.distance, 
				 (Position)(PW->core.width - 
					    PW->pixmap.width * 
					    PW->pixmap.squareW) / 2);
    PW->pixmap.vertOffset = max((Position)PW->pixmap.distance, 
				(Position)(PW->core.height - 
					   PW->pixmap.height * 
					   PW->pixmap.squareH) / 2);

    PW->pixmap.grid &= ((PW->pixmap.squareW > PW->pixmap.grid_tolerance) && 
			(PW->pixmap.squareH > PW->pixmap.grid_tolerance));
}

static void Redisplay(PW, event, region)
     PixmapWidget PW;
     XEvent      *event;
     Region       region;
{
    if(event->type == Expose) {
	if (PW->core.visible) {  
	    Refresh(PW, 
		    event->xexpose.x, event->xexpose.y,
		    event->xexpose.width, event->xexpose.height);
	}
    }
}

void PWClip(w, from_x, from_y, to_x, to_y)
    Widget  w;
    Position      from_x, from_y,
                  to_x, to_y;
{
    PixmapWidget PW = (PixmapWidget) w;
    XRectangle rectangle;
  
    QuerySwap(from_x, to_x);
    QuerySwap(from_y, to_y);
    from_x = max(0, from_x);
    from_y = max(0, from_y);
    to_x = min(PW->pixmap.width - 1, to_x);
    to_y = min(PW->pixmap.height - 1, to_y);

    rectangle.x = InWindowX(PW, from_x);
    rectangle.y = InWindowY(PW, from_y);
    rectangle.width = InWindowX(PW, to_x  + 1) - InWindowX(PW, from_x);
    rectangle.height = InWindowY(PW, to_y + 1) - InWindowY(PW, from_y);
    XSetClipRectangles(dpy,
		       PW->pixmap.highlighting_gc,
		       0, 0,
		       &rectangle, 1,
		       Unsorted);
    XSetClipRectangles(dpy,
		       PW->pixmap.drawing_gc,
		       0, 0,
		       &rectangle, 1,
		       Unsorted);
    XSetClipRectangles(dpy,
		       PW->pixmap.framing_gc,
		       0, 0,
		       &rectangle, 1,
		       Unsorted);
    XSetClipRectangles(dpy,
		       PW->pixmap.axes_gc,
		       0, 0,
		       &rectangle, 1,
		       Unsorted);
}

void PWUnclip(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;
    XRectangle rectangle;
  
    rectangle.x = InWindowX(PW, 0);
    rectangle.y = InWindowY(PW, 0);
    rectangle.width = InWindowX(PW, PW->pixmap.width) - InWindowX(PW, 0);
    rectangle.height = InWindowY(PW, PW->pixmap.height) - InWindowY(PW, 0);
    XSetClipRectangles(dpy,
		       PW->pixmap.highlighting_gc,
		       0, 0,
		       &rectangle, 1,
		       Unsorted);
    XSetClipRectangles(dpy,
		       PW->pixmap.drawing_gc,
		       0, 0,
		       &rectangle, 1,
		       Unsorted);
    XSetClipRectangles(dpy,
		       PW->pixmap.framing_gc,
		       0, 0,
		       &rectangle, 1,
		       Unsorted);
    XSetClipRectangles(dpy,
		       PW->pixmap.axes_gc,
		       0, 0,
		       &rectangle, 1,
		       Unsorted);
}

void Refresh(PW, x, y, width, height)
    PixmapWidget PW;
    Position     x, y;
    Dimension    width, height;
{
    XRectangle rectangle;
    Position i, j;

    XDefineCursor(dpy, XtWindow(PW), PW->pixmap.cursor);

    rectangle.x = min(x, InWindowX(PW, InPixmapX(PW, x)));
    rectangle.y = min(y, InWindowY(PW, InPixmapY(PW, y)));
    rectangle.width = max(x + width,
		     InWindowX(PW, InPixmapX(PW, x + width)+1)) - rectangle.x;
    rectangle.height = max(y + height,
		     InWindowY(PW, InPixmapY(PW, y + height)+1)) - rectangle.y;
    
    XClearArea(dpy, XtWindow(PW),
	       rectangle.x, rectangle.y,
	       rectangle.width, rectangle.height,
	       False);

    XSetClipRectangles(dpy,
		       PW->pixmap.framing_gc,
		       0, 0,
		       &rectangle, 1,
		       Unsorted);

    XDrawRectangle(dpy, XtWindow(PW),
		   PW->pixmap.framing_gc,
		   InWindowX(PW, 0) - 1, InWindowY(PW, 0) - 1,
		   InWindowX(PW, PW->pixmap.width) - InWindowX(PW, 0) + 1, 
		   InWindowY(PW, PW->pixmap.height) - InWindowY(PW, 0) + 1);

    PWClip((Widget) PW,
	   InPixmapX(PW, x),InPixmapY(PW, y),
	   InPixmapX(PW, x + width), InPixmapY(PW, y + height));

    PWRedrawSquares((Widget) PW, InPixmapX(PW, x), InPixmapY(PW, y),
		    InPixmapX(PW, x + width), InPixmapY(PW, y + height));
    
    PWRedrawGrid((Widget) PW,
		 InPixmapX(PW, x), InPixmapY(PW, y),
		 InPixmapX(PW, x + width), InPixmapY(PW, y + height));
    PWRedrawMark((Widget) PW);
    PWRedrawHotSpot((Widget) PW);
    PWRedrawAxes((Widget) PW);
    PWUnclip((Widget) PW);
}

void PWSwitchGrid(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;
    PW->pixmap.grid ^= TRUE;
    PWDrawGrid(w,
	       0, 0,
	       PW->pixmap.image->width - 1, PW->pixmap.image->height - 1);
}

void PWGrid(w, _switch)
    Widget w;
    Boolean _switch;
{
    PixmapWidget PW = (PixmapWidget) w;
    
    if (PW->pixmap.grid != _switch)
	PWSwitchGrid(w);
}

static Boolean SetValues(current, request, new)
     Widget current, request, new;
{
  PixmapWidget p_old = (PixmapWidget) current;
  PixmapWidget p_new = (PixmapWidget) new;
  
  if ((p_old->pixmap.cursor != p_new->pixmap.cursor) && XtIsRealized(new))
    XDefineCursor(XtDisplay(new), XtWindow(new), p_new->pixmap.cursor);
  
    return FALSE;
}

Boolean PWQueryProportional(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;

    return (PW->pixmap.proportional);
}

void PWSwitchProportional(w)
    Widget w;
{
    PixmapWidget PW = (PixmapWidget) w;

    PW->pixmap.proportional ^= True;

    Resize(PW);
    if (PW->core.visible)
	XClearArea(dpy, XtWindow(PW),
		   0, 0, 
		   PW->core.width, PW->core.height,
		   True);
}

void PWProportional(w, _switch)
    Widget w;
    Boolean _switch;
{
    PixmapWidget PW = (PixmapWidget) w;

    if (PW->pixmap.proportional != _switch)
	PWSwitchProportional(w);
}
