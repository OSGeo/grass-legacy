#ifndef _PixelP_h
#define _PixelP_h

#include <InteractP.h>
#include <Pixel.h>

#ifndef XmPIXEL_BIT
#define XmPIXEL_BIT     (50)
#endif

/*  New fields for the Pixel widget class record  */

typedef struct
{
    caddr_t             extension;      /* Pointer to extension record */
} PixelClassPart;


/* Full class record declaration */

typedef struct _PixelClassRec
{
   CoreClassPart               core_class;
   CompositeClassPart          composite_class;
   ConstraintClassPart         constraint_class;
   XmManagerClassPart          manager_class;
   XmBulletinBoardClassPart    bulletin_board_class;
   InteractorClassPart       interactor_class;
   PixelClassPart            pixel_class;
} PixelClassRec;

externalref PixelClassRec pixelClassRec;


/* New fields for the Pixel widget record */

typedef struct
{
   unsigned char scale_type_mask; /* XgRGB, XgHSV, or XgCMY */
   /* R, H, or C scale stuff */
   XmString scale1_label_string; 
   Widget scale1_label;
   Widget scale1;
   Widget scale1_text;
   XmString scale1_value;
   /* G, S, or M scale stuff */
   XmString scale2_label_string;
   Widget scale2_label;
   Widget scale2;
   Widget scale2_text;
   XmString scale2_value;
   /* B, V, or Y scale stuff */
   XmString scale3_label_string;
   Widget scale3_label;
   Widget scale3;
   Widget scale3_text;
   XmString scale3_value;
   /* display area */
   Widget display_area;
   /* input xcolor */
   XColor *xcolor;
   /* saved xcolor */
   XColor *saved;
   /* should we restore colormap automatically on cancel? */
   Boolean cancel_restore;
} PixelPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _PixelRec
{
    CorePart	            core;
    CompositePart           composite;
    ConstraintPart          constraint;
    XmManagerPart           manager;
    XmBulletinBoardPart     bulletin_board;
    InteractorPart        interactor;
    PixelPart             pixel;
} PixelRec;


#define XgSCALE1 1
#define XgSCALE2 2
#define XgSCALE3 3

#define XgPIXEL_DISPLAY_AREA	0
#define XgPIXEL_SCALE1_LABEL	1
#define XgPIXEL_SCALE1		2
#define XgPIXEL_SCALE1_TEXT	3
#define XgPIXEL_SCALE2_LABEL	4
#define XgPIXEL_SCALE2		5
#define XgPIXEL_SCALE2_TEXT	6
#define XgPIXEL_SCALE3_LABEL	7
#define XgPIXEL_SCALE3		8
#define XgPIXEL_SCALE3_TEXT	9

/* Access macros */

#define P_ScaleType( w) (((PixelWidget) (w))->pixel.scale_type_mask)
#define P_Scale1LabelString( w) (((PixelWidget) (w))->pixel.scale1_label_string)
#define P_Scale1Label( w) (((PixelWidget) (w))->pixel.scale1_label)
#define P_Scale1( w) (((PixelWidget) (w))->pixel.scale1)
#define P_Scale1Text( w) (((PixelWidget) (w))->pixel.scale1_text)
#define P_Scale1Value( w) (((PixelWidget) (w))->pixel.scale1_value)
#define P_Scale2LabelString( w) (((PixelWidget) (w))->pixel.scale2_label_string)
#define P_Scale2Label( w) (((PixelWidget) (w))->pixel.scale2_label)
#define P_Scale2( w) (((PixelWidget) (w))->pixel.scale2)
#define P_Scale2Text( w) (((PixelWidget) (w))->pixel.scale2_text)
#define P_Scale2Value( w) (((PixelWidget) (w))->pixel.scale2_value)
#define P_Scale3LabelString( w) (((PixelWidget) (w))->pixel.scale3_label_string)
#define P_Scale3Label( w) (((PixelWidget) (w))->pixel.scale3_label)
#define P_Scale3( w) (((PixelWidget) (w))->pixel.scale3)
#define P_Scale3Text( w) (((PixelWidget) (w))->pixel.scale3_text)
#define P_Scale3Value( w) (((PixelWidget) (w))->pixel.scale3_value)
#define P_DisplayArea( w) (((PixelWidget) (w))->pixel.display_area)
#define P_XColor( w) (((PixelWidget) (w))->pixel.xcolor)
#define P_XColorSaved( w) (((PixelWidget) (w))->pixel.saved)
#define P_Colormap( w) (((PixelWidget) (w))->core.colormap)
#define P_CancelRestore( w) (((PixelWidget) (w))->pixel.cancel_restore)

#endif /* _PixelP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
