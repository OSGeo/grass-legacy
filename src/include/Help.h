/* $Log: Help.h,v $
 * Revision 0.0  1992/05/05  14:56:43  sink
 * auto checkin: Tue May  5 09:56:42 CDT 1992
 *
 * Revision 0.0  1992/03/08  17:56:24  kurt
 * auto checkin: Sun Mar  8 11:56:24 CST 1992
 *
 * Revision 1.3  92/02/21  14:38:16  jinyuan
 * shink and wrap -- 2nd iteration
 * 
 * Revision 1.2  92/02/21  10:43:54  jinyuan
 * shink and warp .... 1st iteration
 *  */

#ifndef _Help_h
#define _Help_h

#include <Xm/Xm.h>
#define XmHELP_BIT (52)

#ifndef XgIsHelp
#define XgIsHelp(w) (_XmIsFastSubclass(XtClass(w),XmHELP_BIT))
#endif

/* Class record constants */

externalref WidgetClass helpWidgetClass;

typedef struct _HelpClassRec *HelpWidgetClass;
typedef struct _HelpRec      *HelpWidget;


/* define the resources names here */

#define XmNdismissOnly "dismissOnly"
#define XmCDismissOnly "DismissOnly"

#define XmNhelpFile "helpFile"
#define XmCHelpFile "HelpFile"

#define XmNregularFontList "regularFontList"
#define XmCRegularFontList "RegularFontList"

#define XmNhotwordFontList "hotwordFontList"
#define XmCHotwordFontList "HotwordFontList"

#define XmNregularForeground "regularForeground"
#define XmCRegularForeground "RegularForeground"

#define XmNhotwordForeground "hotwordForeground"
#define XmCHotwordForeground "HotwordForeground"

#define XmNhelpWidth "helpWidth"
#define XmCHelpWidth "HelpWidth"

#define XmNhelpHeight "helpHeight"
#define XmCHelpHeight "HelpHeight"

/*  Creation entry points: */
#ifdef _NO_PROTO

Widget XgCreateHelpDialog();

#else /* _NO_PROTO */

Widget XgCreateHelpDialog(Widget parent, String name, ArgList al, Cardinal ac);
#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#endif /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif


#endif /* _Help_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
