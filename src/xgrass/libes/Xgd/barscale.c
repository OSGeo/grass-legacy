#include "xgrass_dlib.h"

void 
#ifdef _NO_PROTO
XgdUpdateBarscale(obj, length, intervals, linewidth, color, style, unit, fid, textcolor, winwidth)
    XgdObject *obj;
    double length;
    int intervals;
    int linewidth;
    Pixel color;
    int style;
    int unit;
    Font fid;
    Pixel textcolor;
    int winwidth;
#else
XgdUpdateBarscale(
    XgdObject *obj, 
    double length,
    int intervals,
    int linewidth,
    Pixel color,
    int style,
    int unit,
    Font fid,
    Pixel textcolor,
    int winwidth)
#endif
{
    XgdSetBarscaleLength(obj, length);  
    XgdSetBarscaleInterval(obj, intervals);  
    XgdSetBarscaleLineWidth(obj, linewidth);
    XgdSetBarscaleColor(obj, color);  
    XgdSetBarscaleStyle(obj, style);  
    XgdSetBarscaleUnit(obj, unit);  
    XgdSetBarscaleFont(obj, fid);  
    XgdSetBarscaleTextColor(obj, textcolor);  
    XgdSetBarscaleWinWidth(obj, winwidth);
    XgdDrawBarscale(obj, True, NULL); 
}

void 
#ifdef _NO_PROTO
XgdSetBarscaleLength(obj, length)
    XgdObject *obj;
    double	      length;
#else
XgdSetBarscaleLength(XgdObject *obj, double length)
#endif
{
    obj->Obj.Barscale.length = length;
}

void
#ifdef _NO_PROTO
XgdSetBarscaleInterval(obj, intervals)
    XgdObject *obj;
    int intervals;
#else
XgdSetBarscaleInterval(XgdObject *obj, int intervals)
#endif
{
    obj->Obj.Barscale.intervals = intervals;
}

void 
#ifdef _NO_PROTO
XgdSetBarscaleLineWidth(obj, linewidth)
    XgdObject *obj;
    int linewidth;
#else
XgdSetBarscaleLineWidth(XgdObject *obj, int linewidth)
#endif
{
    obj->Obj.Barscale.linewidth = linewidth;
} 

void 
#ifdef _NO_PROTO
XgdSetBarscaleColor(obj, color)
    XgdObject *obj;
    Pixel color;
#else
XgdSetBarscaleColor(XgdObject *obj, Pixel color)
#endif
{
    obj->Obj.Barscale.color = color;
} 

void 
#ifdef _NO_PROTO
XgdSetBarscaleStyle(obj, style)
    XgdObject *obj;
    int style;
#else
XgdSetBarscaleStyle(XgdObject *obj, int style)
#endif
{
    obj->Obj.Barscale.style = style;
} 

void
#ifdef _NO_PROTO
_XgdMagnitudeRound(in)
    int *in;
#else
_XgdMagnitudeRound(int *in)
#endif
{
    double magnitude = 1.0;

    if ( *in < 10 ) return;

    while ( *in / magnitude > 10.0 ) magnitude *= 10.0;
    if ( magnitude > 10.0 ) magnitude /= 10.0;

    *in = (int)((int)((double)*in / magnitude + 0.5) * magnitude);
    return;
}

#define XGD_KILOMETERS_TO_MILES      ((double)0.6213712)

void 
#ifdef _NO_PROTO
XgdSetBarscaleUnit(obj, unit)
    XgdObject *obj;
    int unit;
#else
XgdSetBarscaleUnit(XgdObject *obj, int unit)
#endif
{
    int oldunits = obj->Obj.Barscale.unit;
    double length = obj->Obj.Barscale.length;

    obj->Obj.Barscale.unit = unit;
    switch ( oldunits ) {
    case XGD_KILOMETERS:
	switch ( unit ) {
	case XGD_KILOMETERS:
            /* no-op */
            return;
	case XGD_METERS:
            length = length * 1000.0;
            break;
	case XGD_MILES:
            length = length * XGD_KILOMETERS_TO_MILES;
            break;
	case XGD_FEET:
            length = length * XGD_KILOMETERS_TO_MILES * 5280.0;
            break;
	}
        break;
    case XGD_METERS:
	switch ( unit ) {
	case XGD_KILOMETERS:
            length = length / 1000.0;
            break;
	case XGD_METERS:
            /* no-op */
            return;
	case XGD_MILES:
            length = length / 1000.0;
            length = length * XGD_KILOMETERS_TO_MILES;
            break;
	case XGD_FEET:
            length = length / 1000.0;
            length = length * XGD_KILOMETERS_TO_MILES * 5280.0;
            break;
	}
        break;
    case XGD_MILES:
	switch ( unit ) {
	case XGD_KILOMETERS:
            length = length / XGD_KILOMETERS_TO_MILES;
            break;
	case XGD_METERS:
            length = length / XGD_KILOMETERS_TO_MILES;
            length = length * 1000.0 ;
            break;
	case XGD_MILES:
            /* no-op */
            return;
	case XGD_FEET:
            length = length * 5280.0 ;
            break;
	}
        break;
    case XGD_FEET:
	switch ( unit ) {
	case XGD_KILOMETERS:
            length = length / 5280.0;
            length = length / XGD_KILOMETERS_TO_MILES;
            break;
	case XGD_METERS:
            length = length / 5280.0;
            length = length / XGD_KILOMETERS_TO_MILES;
            length = length * 1000.0;
            break;
	case XGD_MILES:
            length = length / 5280.0 ;
            break;
	case XGD_FEET:
            /* no-op */
            return;
	}
        break;
    }
    obj->Obj.Barscale.length = length;
} 

void
#ifdef _NO_PROTO
XgdSetBarscaleFontName(obj, name)
     XgdObject *obj;
     char *name;
#else
XgdSetBarscaleFontName(XgdObject *obj, char *name)
#endif
{
    if ( obj->Obj.Barscale.fontname ) XtFree(obj->Obj.Barscale.fontname);
    obj->Obj.Barscale.fontname = XtNewString(name);
}


void 
#ifdef _NO_PROTO
XgdSetBarscaleFont(obj, fid)
    XgdObject *obj;
    Font fid;
#else
XgdSetBarscaleFont(XgdObject *obj, Font fid)
#endif
{
    if ( obj->Obj.Barscale.fid == fid ) return;
    obj->Obj.Barscale.fid = fid;
    XSetFont(obj->display, obj->objgc, fid);
} 

void 
#ifdef _NO_PROTO
XgdSetBarscaleTextColor(obj, textcolor)
    XgdObject *obj;
    Pixel textcolor;
#else
XgdSetBarscaleTextColor(XgdObject *obj, Pixel textcolor)
#endif
{
    obj->Obj.Barscale.textcolor = textcolor;
}

void 
#ifdef _NO_PROTO
XgdSetBarscaleWinWidth(obj, winwidth)
    XgdObject *obj;
    int winwidth; 
#else
XgdSetBarscaleWinWidth(XgdObject *obj, int winwidth)
#endif
{
    obj->Obj.Barscale.winwidth = winwidth;
}
