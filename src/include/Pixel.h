#ifndef _Pixel_h
#define _Pixel_h

#include <Xm/Xm.h>

#ifndef XmPIXEL_BIT
#define XmPIXEL_BIT (50)
#endif

/* Class record constants */

externalref WidgetClass pixelWidgetClass;

typedef struct _PixelClassRec * PixelWidgetClass;
typedef struct _PixelRec      * PixelWidget;


#ifndef XgIsPixel
#define XgIsPixel(w) (_XmIsFastSubclass(XtClass(w),XmPIXEL_BIT))
#endif

#define XmNscaleTypeMask "scaleTypeMask"
#define XmCScaleTypeMask "ScaleTypeMask"
#define XmRScaleTypeMask "ScaleTypeMask"

#ifndef XmNcolormap
#define XmNcolormap "colormap"
#endif
#ifndef XmCColormap
#define XmCColormap "Colormap"
#endif
#define XmRColormap "Colormap"

#define XmNscale1LabelString "scale1LabelString"
#define XmCScale1LabelString "Scale1LabelString"
#define XmRScale1LabelString "Scale1LabelString"

#define XmNscale2LabelString "scale2LabelString"
#define XmCScale2LabelString "Scale2LabelString"
#define XmRScale2LabelString "Scale2LabelString"

#define XmNscale3LabelString "scale3LabelString"
#define XmCScale3LabelString "Scale3LabelString"
#define XmRScale3LabelString "Scale3LabelString"

#define XmNxColor "xColor"
#define XmCXColor "XColor"
#define XmRXColor "XColor"

#define XmNxColorOriginal "xColorOriginal"
#define XmCXColorOriginal "XColorOriginal"
#define XmRXColorOriginal "XColorOriginal"

#define XmNcancelRestore "cancelRestore"
#define XmCCancelRestore "CancelRestore"
#define XmRCancelRestore "CancelRestore"

#define XgRGB 0
#define XgHSV 1
#define XgCMY 2

/*  Creation entry points:
*/
#ifdef _NO_PROTO

extern Widget XgPixelGetChild() ;
extern Widget XgCreatePixel() ;
extern Widget XgCreatePixelDialog() ;

#else /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif


extern Widget XgPixelGetChild( Widget fs, unsigned char which) ;
extern Widget XgCreatePixel( Widget p, String name, ArgList args,
                                                                  Cardinal n) ;
extern Widget XgCreatePixelDialog( Widget ds_p, String name,
                                            ArgList fsb_args, Cardinal fsb_n) ;
#endif /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif


#endif /* _Pixel_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
