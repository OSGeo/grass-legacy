#include "xgrass_dlib.h"

void
#ifdef _NO_PROTO
XgdSetLabelColor(obj, color)
     XgdObject *obj;
     Pixel      color;
#else
XgdSetLabelColor(XgdObject *obj, Pixel color)
#endif
{
  obj->fg = color;
  XSetForeground(obj->display, obj->objgc, color);
  XgdDrawLabel(obj, NULL);
}

void
#ifdef _NO_PROTO
XgdSetLabelString(obj, string, numlines)
     XgdObject *obj;
     char      **string;
#else
XgdSetLabelString(XgdObject *obj, char **string, int numlines)
#endif
{
  int loop;
  
  if (!string)
    return;

  obj->Obj.Label.numlines = numlines;
  
  obj->Obj.Label.lblstr = (char **) malloc (sizeof(char *) * numlines);
  for (loop = 0; loop < numlines; loop++){
    obj->Obj.Label.lblstr[loop] = (char *) malloc (strlen(string[loop]) + 1);
    strcpy(obj->Obj.Label.lblstr[loop], string[loop]);
  }
}

void
#ifdef _NO_PROTO
XgdSetLabelFontName(obj, name)
     XgdObject *obj;
     char *name;
#else
XgdSetLabelFontName(XgdObject *obj, char *name)
#endif
{
    if ( obj->Obj.Label.fontname ) XtFree(obj->Obj.Label.fontname);
    obj->Obj.Label.fontname = XtNewString(name);
}

void  
#ifdef _NO_PROTO
XgdSetLabelFont(obj, font, draw)
     XgdObject *obj;
     XFontStruct *font;
     Boolean draw;
#else
XgdSetLabelFont(XgdObject *obj, XFontStruct *font, Boolean draw)
#endif
{
  XgdUnDrawObject(obj, obj->bg, True);
  obj->Obj.Label.font = font;
  XSetFont(obj->display, obj->objgc, font->fid);
  if ( draw )
      XgdConfigureObject(obj->objgc, obj, obj->x, obj->y, obj->width,
		     obj->height, True);
}

void
#ifdef _NO_PROTO
XgdDrawLabel(obj, pix)
     XgdObject *obj;
     Pixmap    pix;
#else
XgdDrawLabel(XgdObject *obj, Pixmap pix)
#endif
{
  int y;
  int loop;
  
  if (obj->Obj.Label.lblstr){
    y = obj->y;
    for (loop = 0; loop < obj->Obj.Label.numlines; loop++){
      y += obj->Obj.Label.font->ascent;
      XDrawString(obj->display, pix?pix:obj->window, obj->objgc,
		  obj->x, y, 
		  obj->Obj.Label.lblstr[loop],
		  strlen(obj->Obj.Label.lblstr[loop]));
    }
  }
}
    
