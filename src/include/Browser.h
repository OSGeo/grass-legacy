/*
 * File: Browser.h
 *
 * Desc: Public interface file for Browser widget
 *
 * Auth: Eric W. Sink
 *
 * Date: 24 Feb 1992
 *
 * Modification History:
 *
 *
 */
#ifndef _Browser_h
#define _Browser_h

#include <Xm/Xm.h>

/* Class record constants */

#define XG_RASTER 1
#define XG_VECTOR 2
#define XG_SITE 3
#define XG_REGION 4
#define XG_ICON 5
#define XG_LABEL 6
#define XG_GROUP 7
#define XG_ASCII_VECTOR 8
#define XG_ASCII_DLG 9
#define XG_DLG 10
#define XG_SEGMENT 11
#define XG_USER_DEFINED 12

#define XG_SINGLE_SELECT 1
#define XG_MULTIPLE_SELECT 2

externalref WidgetClass         browserWidgetClass;

typedef struct _BrowserClassRec *BrowserWidgetClass;
typedef struct _BrowserRec     *BrowserWidget;

#ifndef XmBROWSER_BIT
#define XmBROWSER_BIT (51)
#endif

#ifndef XgIsBrowser
#define XgIsBrowser(w) (_XmIsFastSubclass(XtClass(w),XmBROWSER_BIT))
#endif

/* define XmNresources */

#define XmNlistAllMapsets "listAllMapsets"
#define XmNnumLists "numLists"
#define XmNlist1IsStatic "list1IsStatic"
#define XmNlist2IsStatic "list2IsStatic"
#define XmNlist3IsStatic "list3IsStatic"
#define XmNbrowseMode "browseMode"
#define XmNselMode "selMode"
#ifndef XmNpromptString
#define XmNpromptString "promptString"
#endif
#define XmNinitialMapset1 "initialMapset1"
#define XmNinitialMapset2 "initialMapset2"
#define XmNinitialMapset3 "initialMapset3"
#define XmNresultString "resultString"
#define XmNresultArray "resultArray"
#define XmNuserDefined "userDefined"
#define XmNuserDBElement "userDBElement"

#define XmCListAllMapsets "ListAllMapsets"
#define XmCNumLists "NumLists"
#define XmCList1IsStatic "List1IsStatic"
#define XmCList2IsStatic "List2IsStatic"
#define XmCList3IsStatic "List3IsStatic"
#define XmCBrowseMode "BrowseMode"
#define XmCSelMode "SelMode"
#ifndef XmCPromptString
#define XmCPromptString "PromptString"
#endif
#define XmCInitialMapset1 "InitialMapset1"
#define XmCInitialMapset2 "InitialMapset2"
#define XmCInitialMapset3 "InitialMapset3"
#define XmCResultString "ResultString"
#define XmCResultArray "ResultArray"
#define XmCUserDefined "UserDefined"
#define XmCUserDBElement "UserDBElement"

#define XmRNumLists "NumLists"
#define XmRList1IsStatic "List1IsStatic"
#define XmRList2IsStatic "List2IsStatic"
#define XmRList3IsStatic "List3IsStatic"
#define XmRBrowseMode "BrowseMode"
#define XmRSelMode "SelMode"
#define XmRPromptString "PromptString"
#define XmRInitialMapset1 "InitialMapset1"
#define XmRInitialMapset2 "InitialMapset2"
#define XmRInitialMapset3 "InitialMapset3"
#define XmRResultString "ResultString"
#define XmRResultArray "ResultArray"
#define XmRUserDefined "UserDefined"
#define XmRUserDBElement "UserDBElement"


/*
 * Creation entry points:
 */
#ifdef _NO_PROTO

extern                          Widget
                                XgBrowserGetChild();
extern                          Widget
                                XgCreateBrowser();
extern                          Widget
                                XgCreateBrowserDialog();
extern void
                                XgBrowserRescan();

#else                           /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
extern                          "C" {
#endif


    extern Widget                   XgBrowserGetChild(Widget fs, unsigned char which);
    extern Widget                   XgCreateBrowser(Widget p, String name, ArgList args,
                                                                Cardinal n);
    extern Widget                   XgCreateBrowserDialog(Widget ds_p, String name,
                                          ArgList fsb_args, Cardinal fsb_n);
    extern void                     XgBrowserRescan(Widget);
#endif                          /* _NO_PROTO */

#if defined(__cplusplus) || defined(c_plusplus)
}

#endif


#endif                          /* _Browser_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */
