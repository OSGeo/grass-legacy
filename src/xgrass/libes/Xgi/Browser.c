static char     rcsid[] = "@(#)XGRASS $Id: Browser.c,v 0.0 1992/05/05 14:55:50 sink Exp sink $";
/*
 * File: Browser.c
 * 
 * Desc: Implementation of Browser widget
 * 
 * Auth: Eric W. Sink
 * 
 * Date: 24 Feb 1992
 * 
 * Modification History:
 * 
 * 
 */

/*
 * The XGRASS Browser is a general purpose widget for browsing the GRASS
 * database.  It contains up to three list widgets, each containing a list of
 * maps or some other database element.  Each list box corresponds to a
 * mapset.  With each list box is an option menu allowing the user to select
 * another mapset.  The widget is configurable.  It allows the user to set
 * the number of lists (up to three), and allows the user to  set any of the
 * lists as static (ie no option menu available).
 */

#include <BrowserP.h>
#include <color.h>

#include <X11/StringDefs.h>
#include <Xm/LabelG.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include <Xm/DrawingA.h>
#include <Xm/Scale.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/DialogS.h>
#include <Xm/PushBG.h>
#include "XmLinux.h"
#include "hourglass.h"

#include <stdio.h>

extern char    *_XgStrDup();
extern char   **_XgListMapsetsWithMaps();
extern char   **_XgSearchPath();
extern char   **_XgStringArrayAdd();
extern Widget   _XgFindRootAncestor();

/*---------------------------------------------------*/
/* forward declarations                              */
/* */
/* this is a list of all private procedures in this  */
/* module                                            */
/*---------------------------------------------------*/

#ifdef _NO_PROTO

static void     BrowserBuildResult();
static void     ClassInitialize();
static void     ClassPartInitialize();
static void     Initialize();
static void     Destroy();
static void     DeleteChild();
static Boolean  SetValues();
static void     BrowserDataSetup();
static void     BrowserCreateWidgets();
static void     BrowserAdjustAndAttach();

#else                           /* _NO_PROTO */

static void     ClassInitialize();
static void     ClassPartInitialize(BrowserWidgetClass xgbc);
static void
                Initialize(BrowserWidget request, BrowserWidget new);
static void     Destroy(BrowserWidget xgb);
static void     DeleteChild(Widget w);
static void     BrowserDataSetup(Widget w);
static void     BrowserBuildResult(Widget w);
static          Boolean
SetValues(BrowserWidget current,
          BrowserWidget request,
          BrowserWidget new);
static void     BrowserCreateWidgets(BrowserWidget xgb);
static void     BrowserAdjustAndAttach(BrowserWidget xgb, Boolean attach);

#endif                          /* _NO_PROTO */

/*---------------------------------------------------*/
/* widget resources                                  */
/*---------------------------------------------------*/
static XtResource resources[] =
{
    /* Browser specific resources */

    {XmNlistAllMapsets,
        XmCListAllMapsets,
        XmRBoolean,
        sizeof(Boolean),
        XtOffset(BrowserWidget, browser.list_all_mapsets),
        XmRImmediate,
        (XtPointer) FALSE
    },
    {XmNnumLists,
        XmCNumLists,
        XmRInt,
        sizeof(int),
        XtOffset(BrowserWidget, browser.num_lists),
        XmRImmediate,
        (XtPointer) 3
    },
    {XmNlist1IsStatic,
        XmCList1IsStatic,
        XmRBoolean,
        sizeof(Boolean),
        XtOffset(BrowserWidget, browser.list1_isstatic),
        XmRImmediate,
        (XtPointer) FALSE
    },
    {XmNlist2IsStatic,
        XmCList2IsStatic,
        XmRBoolean,
        sizeof(Boolean),
        XtOffset(BrowserWidget, browser.list2_isstatic),
        XmRImmediate,
        (XtPointer) FALSE
    },
    {XmNlist3IsStatic,
        XmCList3IsStatic,
        XmRBoolean,
        sizeof(Boolean),
        XtOffset(BrowserWidget, browser.list3_isstatic),
        XmRImmediate,
        (XtPointer) FALSE
    },
    {XmNbrowseMode,
        XmCBrowseMode,
        XmRInt,
        sizeof(int),
        XtOffset(BrowserWidget, browser.browse_mode),
        XmRImmediate,
        (XtPointer) XG_RASTER
    },
    {XmNselMode,
        XmCSelMode,
        XmRInt,
        sizeof(int),
        XtOffset(BrowserWidget, browser.sel_mode),
        XmRImmediate,
        (XtPointer) XG_SINGLE_SELECT
    },
    {XmNinitialMapset1,
        XmCInitialMapset1,
        XmRXmString,
        sizeof(XmString),
        XtOffset(BrowserWidget, browser.initial_mapset1),
        XmRImmediate,
        (XtPointer) 0
    },
    {XmNinitialMapset2,
        XmCInitialMapset2,
        XmRXmString,
        sizeof(XmString),
        XtOffset(BrowserWidget, browser.initial_mapset2),
        XmRImmediate,
        (XtPointer) 0
    },
    {XmNinitialMapset3,
        XmCInitialMapset3,
        XmRXmString,
        sizeof(XmString),
        XtOffset(BrowserWidget, browser.initial_mapset3),
        XmRImmediate,
        (XtPointer) 0
    },
    {XmNresultString,
        XmCResultString,
        XmRXmString,
        sizeof(XmString),
        XtOffset(BrowserWidget, browser.result_string),
        XmRImmediate,
        (XtPointer) 0
    },
    {XmNuserDBElement,
        XmCUserDBElement,
        XmRXmString,
        sizeof(XmString),
        XtOffset(BrowserWidget, browser.db_element),
        XmRImmediate,
        (XtPointer) 0
    },

    /* superclass resource default overrides */

    {XmNautoUnmanage,
        XmCAutoUnmanage,
        XmRBoolean,
        sizeof(Boolean),
        XtOffset(BrowserWidget, bulletin_board.auto_unmanage),
        XmRImmediate,
        (XtPointer) TRUE
    },
    {XmNdialogType,
        XmCDialogType,
        XmRDialogType,
        sizeof(unsigned char),
        XtOffset(BrowserWidget, interactor.dialog_type),
        XmRImmediate,
        (XtPointer) XmINTERACT_WORK_AREA_TYPE
    },
};

static XmSyntheticResource syn_resources[] =
{
    {NULL, 0, 0, NULL, NULL}
};

externaldef(xgbrowserclassrec)
    BrowserClassRec browserClassRec =
    {
        {                       /* core class record        */
             /* superclass          */ (WidgetClass) & interactorClassRec,
             /* class_name          */ "Browser",
             /* widget_size         */ sizeof(BrowserRec),
             /* class_initialize    */ ClassInitialize,
             /* class part init     */ ClassPartInitialize,
             /* class_inited        */ FALSE,
             /* initialize          */ Initialize,
             /* initialize hook     */ NULL,
             /* realize             */ _XtInherit,
             /* actions             */ NULL,
             /* num_actions         */ 0,
             /* resources           */ resources,
             /* num_resources       */ XtNumber(resources),
             /* xrm_class           */ NULLQUARK,
             /* compress_motion     */ TRUE,
             /* compress_exposure   */ XtExposeCompressMaximal,
             /* compress crossing   */ FALSE,
             /* visible_interest    */ FALSE,
             /* destroy             */ Destroy,
             /* resize              */ _XtInherit,
             /* expose              */ _XtInherit,
             /* set_values          */ SetValues,
             /* set_values_hook     */ NULL,
             /* set_values_almost   */ _XtInherit,
             /* get_values_hook     */ NULL,
             /* accept_focus        */ NULL,
             /* version             */ XtVersion,
             /* callback_offsets    */ NULL,
             /* tm_table            */ XtInheritTranslations,
             /* query_geometry      */ (XtGeometryHandler) _XtInherit,
             /* display_accelerator */ NULL,
             /* extension           */ NULL,
        },
        {                       /* composite class record   */
             /* geometry manager */ (XtGeometryHandler) _XtInherit,
             /* set changed proc */ _XtInherit,
             /* insert_child     */ _XtInherit,
             /* delete_child     */ DeleteChild,
             /* extension        */ NULL,
        },
        {                       /* constraint class record  */
             /* no additional resources  */ NULL,
             /* num additional resources */ 0,
             /* size of constraint rec   */ 0,
             /* constraint_initialize    */ NULL,
             /* constraint_destroy       */ NULL,
             /* constraint_setvalue      */ NULL,
             /* extension                */ NULL,
        },
        {                       /* manager class record     */
             /* translations                 */ XtInheritTranslations,
             /* get_resources                */ syn_resources,
             /* num_syn_resources            */ XtNumber(syn_resources),
             /* constraint_syn_resources     */ NULL,
             /* num_constraint_syn_resources */ 0,
             /* parent_process               */ XmInheritParentProcess,
             /* extension                    */ NULL,
        },
        {                       /* bulletinBoard class record */
             /* always_install_accelerators */ TRUE,
             /* geo_matrix_create           */ (XmGeoCreateProc) _XtInherit,
             /* focus_moved_proc            */ NULL,
             /* extension                   */ NULL,
        },
        {                       /* interactor class record */
             /* extension */ NULL,
        },
        {                       /* browser class record */
             /* extension */ NULL,
        }
    };

externaldef(xgbrowserwidgetclass) WidgetClass
browserWidgetClass = (WidgetClass) & browserClassRec;

/****************************************************************/
    static void
                    ClassInitialize()
/*****************/
{

    return;
}

#ifdef _NO_PROTO
static void
BrowserBuildResult(xgb)
    Widget          xgb;
#else
static void
BrowserBuildResult(Widget xgb)
#endif
{
    /*
     * Based on the state of the browser selections, we construct the string
     * which the browser will return.
     */
    int             firstdone = 0;
    int             i;
    int             List1Length;
    int             List2Length;
    int             List3Length;
    char           *BrowserResult;
    if (B_List1Count(xgb) + B_List2Count(xgb) + B_List3Count(xgb)) {
        /* Allocate mem for BrowserResult */
        if (B_ResultString(xgb))
            XmStringFree(B_ResultString(xgb));
        List1Length = _XgStringArrayTotalLength(B_List1Items(xgb),
                                                B_List1Count(xgb));
        List2Length = _XgStringArrayTotalLength(B_List2Items(xgb),
                                                B_List2Count(xgb));
        List3Length = _XgStringArrayTotalLength(B_List3Items(xgb),
                                                B_List3Count(xgb));
        BrowserResult = (char *) _XgMalloc(List1Length +
                                           List2Length +
                                           List3Length +
                                           (B_List1Count(xgb)
                                            + B_List2Count(xgb)
                                            + B_List3Count(xgb)) * 2);
        BrowserResult[0] = 0;
        for (i = 0; i < B_List1Count(xgb); i++) {
            if (firstdone) {
                (void) strcat(BrowserResult, ",");
            }
            firstdone++;
            (void) strcat(BrowserResult, B_List1Items(xgb)[i]);
        }
        for (i = 0; i < B_List2Count(xgb); i++) {
            if (firstdone) {
                (void) strcat(BrowserResult, ",");
            }
            firstdone++;
            (void) strcat(BrowserResult, B_List2Items(xgb)[i]);
        }
        for (i = 0; i < B_List3Count(xgb); i++) {
            if (firstdone) {
                (void) strcat(BrowserResult, ",");
            }
            firstdone++;
            (void) strcat(BrowserResult, B_List3Items(xgb)[i]);
        }
    } else {
        if (B_ResultString(xgb))
            XmStringFree(B_ResultString(xgb));
        BrowserResult = NULL;
    }
    if (BrowserResult)
        B_ResultString(xgb) = XmStringCreateSimple(BrowserResult);
    else
        B_ResultString(xgb) = NULL;

}


/****************************************************************/
static void
#ifdef _NO_PROTO
ClassPartInitialize(xgb)
    BrowserWidgetClass xgb;
#else
ClassPartInitialize(BrowserWidgetClass xgb)
#endif
/****************
 * Class Initialization.  Sets up accelerators and fast subclassing.
 ****************/
{
    /****************/

    _XmFastSubclassInit(xgb, XmBROWSER_BIT);

    return;
}

/****************************************************************/
static void
#ifdef _NO_PROTO
Initialize(request, new)
    BrowserWidget   request;
    BrowserWidget   new;
#else
Initialize(BrowserWidget request,
           BrowserWidget new)
#endif
/****************
 * This routine initializes an instance of the browser widget.
 ****************/
{
    Arg             args[16];
    int             numArgs;
    /****************/

    G_gisinit("Browser");
    B_ResultString(new) = NULL;
    B_List1Items(new) = NULL;
    B_List2Items(new) = NULL;
    B_List3Items(new) = NULL;
    B_List1Count(new) = 0;
    B_List2Count(new) = 0;
    B_List3Count(new) = 0;

    BrowserDataSetup((Widget) new);
    BrowserCreateWidgets(new);

    if (request->manager.navigation_type != XmEXCLUSIVE_TAB_GROUP)
        _XmChangeNavigationType((Widget) new,
                           (XmNavigationType) ((XtIsShell(XtParent(request))
                                      ? XmSTICKY_TAB_GROUP : XmTAB_GROUP)));

    BrowserAdjustAndAttach(new, True);

    if ( I_OkButton(new) && XtIsManaged(I_OkButton(new)) && !I_OkLabelString(new)) {
	XmString xms = XmStringCreateSimple("Done");

	XtVaSetValues(new, XmNokLabelString, xms, NULL);
	XmStringFree(xms);
    }

    return;
}

static void
#ifdef _NO_PROTO
_XgPutStringArrayInOptionMenu(dirlist, menu, callback, dirsize, firstnum, firstitem)
    char          **dirlist;
    Widget          menu;
    int             (*callback) ();
    int             dirsize;
    int             firstnum;
    Widget         *firstitem;
#else
_XgPutStringArrayInOptionMenu(char **dirlist,
                              Widget menu,
                              int (*callback) (),
                              int dirsize,
                              int firstnum,
                              Widget * firstitem)
#endif
{
    /*
     * This routine accepts a directory string array and a menu, and stuffs
     * the directory into the menu.  The arg firstnum specifies which
     * numbered item will be the first one selected (assuming that this is an
     * option menu.  firstitem is used to return the actual widget
     * corresponding to that first item.
     */
    int             i;
    Widget          menuitem;

    for (i = 0; i < dirsize; i++) {
        menuitem = XtVaCreateManagedWidget(dirlist[i], xmPushButtonGadgetClass,
                                           menu, NULL);
        XtAddCallback(menuitem, XmNactivateCallback, callback, i);
        if (i == firstnum)
            *firstitem = menuitem;
    }
}

/*
 * BrowserUpdateList* is the callback which is called when the mapset is
 * changed with the option menu.
 */

static int
#ifdef _NO_PROTO
BrowserUpdateList1(w, num)
    Widget          w;
    int             num;
#else
BrowserUpdateList1(Widget w, int num)
#endif
{
    /*
     * Change mapset, reset list to totally different set.  Deselect all
     * items.
     */
    char            path[512];
    int             count;
    int             i;
    Widget          xgb;
    xgb = w;
    while (!XgIsBrowser(xgb))
        xgb = XtParent(xgb);
    B_Menu1Which(xgb) = num;
    if (B_OptionsItems(xgb)) {
        switch (B_BrowseMode(xgb)) {
        case XG_RASTER:
            (void) sprintf(path, "%s/%s/cell", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu1Which(xgb)]);
            break;
        case XG_ASCII_DLG:
            (void) sprintf(path, "%s/%s/dlg", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu1Which(xgb)]);
            break;
        case XG_DLG:
            (void) sprintf(path, "%s/%s/bdlg", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu1Which(xgb)]);
            break;
        case XG_ASCII_VECTOR:
            (void) sprintf(path, "%s/%s/dig_ascii", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu1Which(xgb)]);
            break;
        case XG_SEGMENT:
            (void) sprintf(path, "%s/%s/seg", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu1Which(xgb)]);
            break;
        case XG_VECTOR:
            (void) sprintf(path, "%s/%s/dig", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu1Which(xgb)]);
            break;
        case XG_SITE:
            (void) sprintf(path, "%s/%s/site_lists", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu1Which(xgb)]);
            break;
        case XG_REGION:
            (void) sprintf(path, "%s/%s/windows", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu1Which(xgb)]);
            break;
        case XG_ICON:
            (void) sprintf(path, "%s/%s/icons", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu1Which(xgb)]);
            break;
        case XG_LABEL:
            (void) sprintf(path, "%s/%s/paint", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu1Which(xgb)]);
            break;
        case XG_GROUP:
            (void) sprintf(path, "%s/%s/group", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu1Which(xgb)]);
            break;
        case XG_USER_DEFINED:
            {
	    char           *s;
            XmStringGetLtoR(B_DBElement(xgb), XmSTRING_DEFAULT_CHARSET, &s);
            (void) sprintf(path, "%s/%s/%s", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu1Which(xgb)], s);
            }
            break;
        }
    }
    _XgPutDirectoryInList(path, B_List1(xgb), 0);
    XmListDeselectAllItems(B_List1(xgb));
    if (B_List1Items(xgb)) {
        _XgFreeDirectoryListing(B_List1Items(xgb), B_List1Count(xgb));
        B_List1Items(xgb) = NULL;
    }
    B_List1Count(xgb) = 0;
    BrowserBuildResult(xgb);
}

static int
#ifdef _NO_PROTO
BrowserUpdateList2(w, num)
    Widget          w;
    int             num;
#else
BrowserUpdateList2(Widget w, int num)
#endif
{
    /*
     * Change mapset, reset list to totally different set.  Deselect all
     * items.
     */
    char            path[512];
    int             count;
    int             i;
    Widget          xgb;
    xgb = w;
    while (!XgIsBrowser(xgb))
        xgb = XtParent(xgb);
    B_Menu2Which(xgb) = num;
    if (B_OptionsItems(xgb)) {
        switch (B_BrowseMode(xgb)) {
        case XG_RASTER:
            (void) sprintf(path, "%s/%s/cell", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu2Which(xgb)]);
            break;
        case XG_ASCII_DLG:
            (void) sprintf(path, "%s/%s/dlg", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu2Which(xgb)]);
            break;
        case XG_DLG:
            (void) sprintf(path, "%s/%s/bdlg", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu2Which(xgb)]);
            break;
        case XG_ASCII_VECTOR:
            (void) sprintf(path, "%s/%s/dig_ascii", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu2Which(xgb)]);
            break;
        case XG_SEGMENT:
            (void) sprintf(path, "%s/%s/seg", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu2Which(xgb)]);
            break;
        case XG_VECTOR:
            (void) sprintf(path, "%s/%s/dig", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu2Which(xgb)]);
            break;
        case XG_SITE:
            (void) sprintf(path, "%s/%s/site_lists", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu2Which(xgb)]);
            break;
        case XG_REGION:
            (void) sprintf(path, "%s/%s/windows", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu2Which(xgb)]);
            break;
        case XG_ICON:
            (void) sprintf(path, "%s/%s/icons", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu2Which(xgb)]);
            break;
        case XG_LABEL:
            (void) sprintf(path, "%s/%s/paint", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu2Which(xgb)]);
            break;
        case XG_GROUP:
            (void) sprintf(path, "%s/%s/group", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu2Which(xgb)]);
            break;
        case XG_USER_DEFINED:
            (void) sprintf(path, "%s/%s/%s", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu1Which(xgb)],
                           B_DBElement(xgb));
            break;
        }
    }
    _XgPutDirectoryInList(path, B_List2(xgb), 0);
    XmListDeselectAllItems(B_List2(xgb));
    if (B_List2Items(xgb)) {
        _XgFreeDirectoryListing(B_List2Items(xgb), B_List2Count(xgb));
        B_List2Items(xgb) = NULL;
    }
    B_List2Count(xgb) = 0;
    BrowserBuildResult(xgb);
}

static int
#ifdef _NO_PROTO
BrowserUpdateList3(w, num)
    Widget          w;
    int             num;
#else
BrowserUpdateList3(Widget w, int num)
#endif
{
    /*
     * Change mapset, reset list to totally different set.  Deselect all
     * items.
     */
    char            path[512];
    int             count;
    int             i;
    Widget          xgb;
    xgb = w;
    while (!XgIsBrowser(xgb))
        xgb = XtParent(xgb);
    B_Menu3Which(xgb) = num;
    if (B_OptionsItems(xgb)) {
        switch (B_BrowseMode(xgb)) {
        case XG_RASTER:
            (void) sprintf(path, "%s/%s/cell", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu3Which(xgb)]);
            break;
        case XG_ASCII_DLG:
            (void) sprintf(path, "%s/%s/dlg", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu3Which(xgb)]);
            break;
        case XG_DLG:
            (void) sprintf(path, "%s/%s/bdlg", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu3Which(xgb)]);
            break;
        case XG_ASCII_VECTOR:
            (void) sprintf(path, "%s/%s/dig_ascii", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu3Which(xgb)]);
            break;
        case XG_SEGMENT:
            (void) sprintf(path, "%s/%s/seg", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu3Which(xgb)]);
            break;
        case XG_VECTOR:
            (void) sprintf(path, "%s/%s/dig", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu3Which(xgb)]);
            break;
        case XG_SITE:
            (void) sprintf(path, "%s/%s/site_lists", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu3Which(xgb)]);
            break;
        case XG_REGION:
            (void) sprintf(path, "%s/%s/windows", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu3Which(xgb)]);
            break;
        case XG_ICON:
            (void) sprintf(path, "%s/%s/icons", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu3Which(xgb)]);
            break;
        case XG_LABEL:
            (void) sprintf(path, "%s/%s/paint", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu3Which(xgb)]);
            break;
        case XG_GROUP:
            (void) sprintf(path, "%s/%s/group", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu3Which(xgb)]);
            break;
        case XG_USER_DEFINED:
            (void) sprintf(path, "%s/%s/%s", G_location_path(),
                           B_OptionsItems(xgb)[B_Menu1Which(xgb)],
                           B_DBElement(xgb));
            break;
        }
    }
    _XgPutDirectoryInList(path, B_List3(xgb), 0);
    XmListDeselectAllItems(B_List3(xgb));
    if (B_List3Items(xgb)) {
        _XgFreeDirectoryListing(B_List3Items(xgb), B_List3Count(xgb));
        B_List3Items(xgb) = NULL;
    }
    B_List3Count(xgb) = 0;
    BrowserBuildResult(xgb);
}

void
#ifdef _NO_PROTO
XgBrowserRescan(xgb)
    Widget          xgb;
#else
XgBrowserRescan(Widget xgb)
#endif
{
    if (B_List1(xgb))
        BrowserUpdateList1(B_List1(xgb), B_Menu1Which(xgb));
    if (B_List2(xgb))
        BrowserUpdateList2(B_List2(xgb), B_Menu2Which(xgb));
    if (B_List3(xgb))
        BrowserUpdateList3(B_List3(xgb), B_Menu3Which(xgb));
}

/* BrowserListCallback is called when something in a list is selected */
static void
#ifdef _NO_PROTO
BrowserListCallback(list_w, client_data, cbs)
    Widget          list_w;
    XtPointer       client_data;
    XmListCallbackStruct *cbs;
#else
BrowserListCallback(Widget list_w,
                    XtPointer client_data,
                    XmListCallbackStruct * cbs)
#endif
{
    /* BrowserListCallback is called when something in a list is selected */
    char           *choice;
    char            mapspec[256];
    int             i;
    Widget          xgb;

    xgb = list_w;
    while (!XgIsBrowser(xgb))
        xgb = XtParent(xgb);

    if (list_w == B_List1(xgb)) {
        if (B_OptionsItems(xgb)) {
            if ((cbs->reason == XmCR_MULTIPLE_SELECT) &&
                (B_SelMode(xgb) != XG_SINGLE_SELECT)) {
                if (B_List1Items(xgb))
                    _XgFreeDirectoryListing(B_List1Items(xgb), B_List1Count(xgb));
                B_List1Count(xgb) = cbs->selected_item_count;
                B_List1Items(xgb) = (char **) _XgCalloc(B_List1Count(xgb), sizeof(char *));
                for (i = 0; i < cbs->selected_item_count; i++) {
                    XmStringGetLtoR(cbs->selected_items[i], XmSTRING_DEFAULT_CHARSET, &choice);
                    (void) sprintf(mapspec, "%s@%s", choice, B_OptionsItems(xgb)[B_Menu1Which(xgb)]);
                    B_List1Items(xgb)[i] = _XgStrDup(mapspec);
                    XtFree(choice);
                }
            } else {
                XmStringGetLtoR(cbs->item, XmSTRING_DEFAULT_CHARSET, &choice);
                (void) sprintf(mapspec, "%s@%s", choice, B_OptionsItems(xgb)[B_Menu1Which(xgb)]
                    );
                if (B_List1Items(xgb) && B_List1Count(xgb)) {
                    if (strcmp(mapspec, B_List1Items(xgb)[0])) {
                        _XgFreeDirectoryListing(B_List1Items(xgb), B_List1Count(xgb));
                        B_List1Count(xgb) = 1;
                        B_List1Items(xgb) = (char **) _XgCalloc(B_List1Count(xgb), sizeof(char
                                                                        *));
                        B_List1Items(xgb)[0] = _XgStrDup(mapspec);
                    } else {
                        _XgFreeDirectoryListing(B_List1Items(xgb), B_List1Count(xgb));
                        B_List1Count(xgb) = 0;
                    }
                } else {
                    B_List1Count(xgb) = 1;
                    B_List1Items(xgb) = (char **) _XgCalloc(B_List1Count(xgb), sizeof(char *));
                    B_List1Items(xgb)[0] = _XgStrDup(mapspec);
                }
                XtFree(choice);
            }
            if (B_SelMode(xgb) == XG_SINGLE_SELECT) {
                if (B_List2(xgb)) {
                    XmListDeselectAllItems(B_List2(xgb));
                    if (B_List2Items(xgb))
                        _XgFreeDirectoryListing(B_List2Items(xgb), B_List2Count(xgb));
                    B_List2Count(xgb) = 0;
                }
                if (B_List3(xgb)) {
                    XmListDeselectAllItems(B_List3(xgb));
                    if (B_List3Items(xgb))
                        _XgFreeDirectoryListing(B_List3Items(xgb), B_List3Count(xgb));
                    B_List3Count(xgb) = 0;
                }
            }
        }
    } else if (list_w == B_List2(xgb)) {
        if (B_OptionsItems(xgb)) {
            if ((cbs->reason == XmCR_MULTIPLE_SELECT) &&
                (B_SelMode(xgb) != XG_SINGLE_SELECT)) {
                if (B_List2Items(xgb))
                    _XgFreeDirectoryListing(B_List2Items(xgb), B_List2Count(xgb));
                B_List2Count(xgb) = cbs->selected_item_count;
                B_List2Items(xgb) = (char **) _XgCalloc(B_List2Count(xgb), sizeof(char *));
                for (i = 0; i < cbs->selected_item_count; i++) {
                    XmStringGetLtoR(cbs->selected_items[i], XmSTRING_DEFAULT_CHARSET, &choice);
                    (void) sprintf(mapspec, "%s@%s", choice, B_OptionsItems(xgb)[B_Menu2Which(xgb)]);
                    B_List2Items(xgb)[i] = _XgStrDup(mapspec);
                    XtFree(choice);
                }
            } else {
                XmStringGetLtoR(cbs->item, XmSTRING_DEFAULT_CHARSET, &choice);
                (void) sprintf(mapspec, "%s@%s", choice, B_OptionsItems(xgb)[B_Menu2Which(xgb)]
                    );
                if (B_List2Items(xgb) && B_List2Count(xgb)) {
                    if (strcmp(mapspec, B_List2Items(xgb)[0])) {
                        _XgFreeDirectoryListing(B_List2Items(xgb), B_List2Count(xgb));
                        B_List2Count(xgb) = 1;
                        B_List2Items(xgb) = (char **) _XgCalloc(B_List2Count(xgb), sizeof(char
                                                                        *));
                        B_List2Items(xgb)[0] = _XgStrDup(mapspec);
                    } else {
                        _XgFreeDirectoryListing(B_List2Items(xgb), B_List2Count(xgb));
                        B_List2Count(xgb) = 0;
                    }
                } else {
                    B_List2Count(xgb) = 1;
                    B_List2Items(xgb) = (char **) _XgCalloc(B_List2Count(xgb), sizeof(char *));
                    B_List2Items(xgb)[0] = _XgStrDup(mapspec);
                }
                XtFree(choice);
            }
            if (B_SelMode(xgb) == XG_SINGLE_SELECT) {
                if (B_List1(xgb)) {
                    XmListDeselectAllItems(B_List1(xgb));
                    if (B_List1Items(xgb))
                        _XgFreeDirectoryListing(B_List1Items(xgb), B_List1Count(xgb));
                    B_List1Count(xgb) = 0;
                }
                if (B_List3(xgb)) {
                    XmListDeselectAllItems(B_List3(xgb));
                    if (B_List3Items(xgb))
                        _XgFreeDirectoryListing(B_List3Items(xgb), B_List3Count(xgb));
                    B_List3Count(xgb) = 0;
                }
            }
        }
    } else if (list_w == B_List3(xgb)) {
        if (B_OptionsItems(xgb)) {
            if ((cbs->reason == XmCR_MULTIPLE_SELECT) &&

                (B_SelMode(xgb) != XG_SINGLE_SELECT)) {
                if (B_List3Items(xgb))
                    _XgFreeDirectoryListing(B_List3Items(xgb), B_List3Count(xgb));
                B_List3Count(xgb) = cbs->selected_item_count;
                B_List3Items(xgb) = (char **) _XgCalloc(B_List3Count(xgb), sizeof(char *));
                for (i = 0; i < cbs->selected_item_count; i++) {
                    XmStringGetLtoR(cbs->selected_items[i], XmSTRING_DEFAULT_CHARSET, &choice);
                    (void) sprintf(mapspec, "%s@%s", choice, B_OptionsItems(xgb)[B_Menu3Which(xgb)]);
                    B_List3Items(xgb)[i] = _XgStrDup(mapspec);
                    XtFree(choice);
                }
            } else {
                XmStringGetLtoR(cbs->item, XmSTRING_DEFAULT_CHARSET, &choice);
                (void) sprintf(mapspec, "%s@%s", choice, B_OptionsItems(xgb)[B_Menu3Which(xgb)]
                    );
                if (B_List3Items(xgb) && B_List3Count(xgb)) {
                    if (strcmp(mapspec, B_List3Items(xgb)[0])) {
                        _XgFreeDirectoryListing(B_List3Items(xgb), B_List3Count(xgb));
                        B_List3Count(xgb) = 1;
                        B_List3Items(xgb) = (char **) _XgCalloc(B_List3Count(xgb), sizeof(char
                                                                        *));
                        B_List3Items(xgb)[0] = _XgStrDup(mapspec);
                    } else {
                        _XgFreeDirectoryListing(B_List3Items(xgb), B_List3Count(xgb));
                        B_List3Count(xgb) = 0;
                    }
                } else {
                    B_List3Count(xgb) = 1;
                    B_List3Items(xgb) = (char **) _XgCalloc(B_List3Count(xgb), sizeof(char *));
                    B_List3Items(xgb)[0] = _XgStrDup(mapspec);
                }
                XtFree(choice);
            }
            if (B_SelMode(xgb) == XG_SINGLE_SELECT) {
                if (B_List2(xgb)) {
                    XmListDeselectAllItems(B_List2(xgb));
                    if (B_List2Items(xgb))
                        _XgFreeDirectoryListing(B_List2Items(xgb), B_List2Count(xgb));
                    B_List2Count(xgb) = 0;
                }
                if (B_List1(xgb)) {
                    XmListDeselectAllItems(B_List1(xgb));
                    if (B_List1Items(xgb))
                        _XgFreeDirectoryListing(B_List1Items(xgb), B_List1Count(xgb));
                    B_List1Count(xgb) = 0;
                }
            }
        }
    }
    BrowserBuildResult(xgb);
}


/****************************************************************/
/*
 * BrowserDataSetup searches the database (if necessary) and constructs the
 * data structures necessary for the browser.
 */
static void
#ifdef _NO_PROTO
BrowserDataSetup(xgb)
    Widget          xgb;
#else
BrowserDataSetup(Widget xgb)
#endif
{
    Boolean         needSearch = False;

    /*
     * If the user sets XmNnumLists to 1,2 or 3, then we force that number of
     * lists.  If it is left at 0, then we decide how many lists to make.  If
     * the user sets the initialMapset for all of the forced lists, and makes
     * them all static, we don't even do the database search
     */

    B_OptionsItems(xgb) = NULL;
    B_OptionsCount(xgb) = 0;
    if (B_NumLists(xgb)) {
        /* This means the user specified the number of desired lists. */
        if (B_NumLists(xgb) >= 1) {
            if (!B_List1IsStatic(xgb)) {
                needSearch = True;
            }
        }
        if (B_NumLists(xgb) >= 2) {
            if (!B_List2IsStatic(xgb)) {
                needSearch = True;
            }
        }
        if (B_NumLists(xgb) >= 3) {
            if (!B_List3IsStatic(xgb)) {
                needSearch = True;
            }
        }
    } else {
        if (!B_List1IsStatic(xgb)) {
            needSearch = True;
        }
        if (!B_List2IsStatic(xgb)) {
            needSearch = True;
        }
        if (!B_List3IsStatic(xgb)) {
            needSearch = True;
        }
    }

    B_Menu1Which(xgb) = 0;
    B_Menu2Which(xgb) = 1;
    B_Menu3Which(xgb) = 2;


    if (needSearch) {


        Pixmap          hourglassPixmap;
        Pixmap          hourglassmaskPixmap;
        Cursor          hourglassCursor;
        XColor          blackColor, whiteColor, exact;
        Status          resBlack, resWhite;
        Window          win;
        Widget          curWidget;

        for (curWidget = xgb; curWidget; curWidget = XtParent(curWidget)) {
            win = XtWindow(curWidget);

            if (win) {
                resBlack = XAllocNamedColor(XtDisplay(xgb),
                               XDefaultColormap(XtDisplay(xgb), 0), "black",
                                            &exact, &blackColor);
                resWhite = XAllocNamedColor(XtDisplay(xgb),
                               XDefaultColormap(XtDisplay(xgb), 0), "white",
                                            &exact, &whiteColor);
                hourglassPixmap = XCreateBitmapFromData(XtDisplay(xgb),
                                                        win,
                                                        hourglass_bits,
                                                        hourglass_width,
                                                        hourglass_height);
                hourglassmaskPixmap = XCreateBitmapFromData(XtDisplay(xgb),
                                                            win,
                                                         hourglassmask_bits,
                                                        hourglassmask_width,
                                                      hourglassmask_height);
                hourglassCursor = XCreatePixmapCursor(XtDisplay(xgb),
                                                      hourglassPixmap,
                                                      hourglassmaskPixmap,
                                                      &blackColor,
                                                      &whiteColor,
                                                      hourglass_x_hot,
                                                      hourglass_y_hot);
                XDefineCursor(XtDisplay(xgb), win, hourglassCursor);
            }
        }
        XFlush(XtDisplay(xgb));

        if ( B_BrowseMode(xgb) == XG_USER_DEFINED ) {
            if ( B_DBElement(xgb) == NULL ) {
                _XmWarning(xgb, 
	   "Must set userDBElement in userDefined mode. Defaulting to cell");
                B_DBElement(xgb) = "cell";
            }
        }
        if (B_ListAllMapsets(xgb)) {
            B_OptionsItems(xgb) = _XgListMapsetsWithMaps(B_BrowseMode(xgb), 
                                                 B_DBElement(xgb),
                                                 &(B_OptionsCount(xgb)), 0);
        } else {
            B_OptionsItems(xgb) = _XgSearchPath(&(B_OptionsCount(xgb)));
        }

        for (curWidget = xgb; curWidget; curWidget = XtParent(curWidget)) {
            win = XtWindow(curWidget);
            if (win) {
                XUndefineCursor(XtDisplay(xgb), win);
            }
        }

    } else {
        B_OptionsItems(xgb) = NULL;
        B_OptionsCount(xgb) = 0;
    }
    if (B_InitialMapset1(xgb)) {
        /* If the initial mapset 1 is not in the list, add it */
        char           *s;
        XmStringGetLtoR(B_InitialMapset1(xgb), XmSTRING_DEFAULT_CHARSET, &s);
        if (_XgStringArraySearch(B_OptionsItems(xgb),
                                 B_OptionsCount(xgb), s) == -1) {
            B_OptionsItems(xgb) = _XgStringArrayAdd(B_OptionsItems(xgb),
                                                (B_OptionsCount(xgb))++, s);
        }
        B_Menu1Which(xgb) = _XgStringArraySearch(B_OptionsItems(xgb),
                                                 B_OptionsCount(xgb), s);
    }
    if (B_InitialMapset2(xgb)) {
        /* If the initial mapset 2 is not in the list, add it */
        char           *s;
        XmStringGetLtoR(B_InitialMapset2(xgb), XmSTRING_DEFAULT_CHARSET, &s);
        if (_XgStringArraySearch(B_OptionsItems(xgb),
                                 B_OptionsCount(xgb), s) == -1) {
            B_OptionsItems(xgb) = _XgStringArrayAdd(B_OptionsItems(xgb),
                                                (B_OptionsCount(xgb))++, s);
        }
        B_Menu2Which(xgb) = _XgStringArraySearch(B_OptionsItems(xgb),
                                                 B_OptionsCount(xgb), s);
    }
    if (B_InitialMapset3(xgb)) {
        /* If the initial mapset 3 is not in the list, add it */
        char           *s;
        XmStringGetLtoR(B_InitialMapset3(xgb), XmSTRING_DEFAULT_CHARSET, &s);
        if (_XgStringArraySearch(B_OptionsItems(xgb),
                                 B_OptionsCount(xgb), s) == -1) {
            B_OptionsItems(xgb) = _XgStringArrayAdd(B_OptionsItems(xgb),
                                                (B_OptionsCount(xgb))++, s);
        }
        B_Menu3Which(xgb) = _XgStringArraySearch(B_OptionsItems(xgb),
                                                 B_OptionsCount(xgb), s);
    }
    if (!B_NumLists(xgb)) {
        B_NumLists(xgb) = (B_OptionsCount(xgb) >= 3) ? 3 : B_OptionsCount(xgb);
    }
}

/****************************************************************/
static void
#ifdef _NO_PROTO
BrowserCreateWidgets(xgb)
    BrowserWidget   xgb;
#else
BrowserCreateWidgets(BrowserWidget xgb)
#endif
{
    Widget          form;
    Arg             args[5];
    int             nargs = 0;
    XmString        s;
    Widget          firstItem;

    form = XmCreateForm((Widget) xgb, "xgbrowser_form", args, nargs);
    XgAddHelpCallBackFromFile(I_WorkArea(xgb), "xgbrowser_form");

    /* Create list unit */

    if (B_NumLists(xgb) >= 1) {
        B_List1Container(xgb) =
            XtVaCreateManagedWidget("xgbrowser_list1container",
                                    xmFormWidgetClass, form,
                                    NULL);

        B_Between1(xgb) = XmCreateFrame(B_List1Container(xgb), "xgbrowser_between1", NULL, 0);
        XtManageChild(B_Between1(xgb));
        B_Menu1(xgb) = XmCreatePulldownMenu(B_Between1(xgb), "xgbrowser_menu1", NULL, 0);
        s = XmStringCreateSimple("Mapset :");

        nargs = 0;
        XtSetArg(args[nargs], XmNsubMenuId, B_Menu1(xgb));
        nargs++;
        XtSetArg(args[nargs], XmNlabelString, s);
        nargs++;
        B_Menu1Button(xgb) = XmCreateOptionMenu(B_Between1(xgb), "xgbrowser_menu1_button", args, nargs);
        XmStringFree(s);

        _XgPutStringArrayInOptionMenu(B_OptionsItems(xgb), B_Menu1(xgb), BrowserUpdateList1, B_OptionsCount(xgb), B_Menu1Which(xgb), &firstItem);
        XtVaSetValues(B_Menu1Button(xgb), XmNmenuHistory, firstItem, NULL);
        XtManageChild(B_Menu1Button(xgb));
        if (B_List1IsStatic(xgb)) {
            XtSetSensitive(B_Menu1Button(xgb), False);
            XgAddHelpCallBackFromFile(B_Menu1Button(xgb), "xgb_men1_stat");
        } else {
            XgAddHelpCallBackFromFile(B_Menu1Button(xgb), "xgb_men1_dynam");
        }
        if (B_SelMode(xgb) == XG_SINGLE_SELECT) {
            XtSetArg(args[0], XmNselectionPolicy, XmSINGLE_SELECT);
        } else {
            XtSetArg(args[0], XmNselectionPolicy, XmMULTIPLE_SELECT);
        }
        XtSetArg(args[1], XmNvisibleItemCount, 10);
        B_List1(xgb) = XmCreateScrolledList(B_List1Container(xgb), "xgbrow_list1", args, 2);
        XtVaSetValues(B_Between1(xgb),
                      XmNtopAttachment, XmATTACH_FORM,
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      NULL);
        XtVaSetValues(XtParent(B_List1(xgb)),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, B_Between1(xgb),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      XmNbottomAttachment, XmATTACH_FORM,
                      NULL);

        XgAddHelpCallBackFromFile(B_List1(xgb), "xgbrow_list1");

        XtAddCallback(B_List1(xgb), XmNsingleSelectionCallback, BrowserListCallback, NULL);
        XtAddCallback(B_List1(xgb), XmNmultipleSelectionCallback, BrowserListCallback, NULL);
        XtAddCallback(B_List1(xgb), XmNextendedSelectionCallback, BrowserListCallback, NULL);
        /* Initialize the list */
        BrowserUpdateList1(B_List1(xgb), B_Menu1Which(xgb));
        XtManageChild(B_List1(xgb));
    } else {
        B_List1(xgb) = B_List1Container(xgb) = B_Menu1(xgb) = B_Menu1Button(xgb) = B_Between1(xgb) = NULL;
    }

    /* Create list unit */

    if (B_NumLists(xgb) >= 2) {
        B_List2Container(xgb) = XtVaCreateManagedWidget("xgbrowser_list2container", xmFormWidgetClass, form,
                                                        NULL);
        B_Between2(xgb) = XmCreateFrame(B_List2Container(xgb), "xgbrowser_between2", NULL, 0);
        XtManageChild(B_Between2(xgb));
        B_Menu2(xgb) = XmCreatePulldownMenu(B_Between2(xgb), "xgbrowser_menu2", NULL, 0);
        s = XmStringCreateSimple("Mapset :");

        nargs = 0;
        XtSetArg(args[nargs], XmNsubMenuId, B_Menu2(xgb));
        nargs++;
        XtSetArg(args[nargs], XmNlabelString, s);
        nargs++;
        B_Menu2Button(xgb) = XmCreateOptionMenu(B_Between2(xgb), "xgbrowser_menu2_button", args, nargs);
        XmStringFree(s);

        _XgPutStringArrayInOptionMenu(B_OptionsItems(xgb), B_Menu2(xgb), BrowserUpdateList2, B_OptionsCount(xgb), B_Menu2Which(xgb), &firstItem);
        XtVaSetValues(B_Menu2Button(xgb), XmNmenuHistory, firstItem, NULL);
        XtManageChild(B_Menu2Button(xgb));
        if (B_List2IsStatic(xgb)) {
            XtSetSensitive(B_Menu2Button(xgb), False);
            XgAddHelpCallBackFromFile(B_Menu2Button(xgb), "xgb_men2_stat");
        } else {
            XgAddHelpCallBackFromFile(B_Menu2Button(xgb), "xgb_men2_dynam");
        }
        if (B_SelMode(xgb) == XG_SINGLE_SELECT) {
            XtSetArg(args[0], XmNselectionPolicy, XmSINGLE_SELECT);
        } else {
            XtSetArg(args[0], XmNselectionPolicy, XmMULTIPLE_SELECT);
        }
        XtSetArg(args[1], XmNvisibleItemCount, 10);
        B_List2(xgb) = XmCreateScrolledList(B_List2Container(xgb), "xgbrow_list2", args, 2);
        XtVaSetValues(B_Between2(xgb),
                      XmNtopAttachment, XmATTACH_FORM,
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      NULL);
        XtVaSetValues(XtParent(B_List2(xgb)),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, B_Between2(xgb),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      XmNbottomAttachment, XmATTACH_FORM,
                      NULL);

        XgAddHelpCallBackFromFile(B_List2(xgb), "xgbrow_list2");

        XtAddCallback(B_List2(xgb), XmNsingleSelectionCallback, BrowserListCallback, NULL);
        XtAddCallback(B_List2(xgb), XmNmultipleSelectionCallback, BrowserListCallback, NULL);
        XtAddCallback(B_List2(xgb), XmNextendedSelectionCallback, BrowserListCallback, NULL);
        /* Initialize the list */
        BrowserUpdateList2(B_List2(xgb), B_Menu2Which(xgb));
        XtManageChild(B_List2(xgb));
    } else {
        B_List2(xgb) = B_List2Container(xgb) = B_Menu2(xgb) = B_Menu2Button(xgb) = B_Between2(xgb) = NULL;
    }

    /* Create list unit */

    if (B_NumLists(xgb) >= 3) {
        B_List3Container(xgb) = XtVaCreateManagedWidget("xgbrowser_list3container", xmFormWidgetClass, form,
                                                        NULL);
        B_Between3(xgb) = XmCreateFrame(B_List3Container(xgb), "xgbrowser_between3", NULL, 0);
        XtManageChild(B_Between3(xgb));
        B_Menu3(xgb) = XmCreatePulldownMenu(B_Between3(xgb), "xgbrowser_menu3", NULL, 0);
        s = XmStringCreateSimple("Mapset :");

        nargs = 0;
        XtSetArg(args[nargs], XmNsubMenuId, B_Menu3(xgb));
        nargs++;
        XtSetArg(args[nargs], XmNlabelString, s);
        nargs++;
        B_Menu3Button(xgb) = XmCreateOptionMenu(B_Between3(xgb), "xgbrowser_menu3_button", args, nargs);
        XmStringFree(s);

        _XgPutStringArrayInOptionMenu(B_OptionsItems(xgb), B_Menu3(xgb), BrowserUpdateList3, B_OptionsCount(xgb), B_Menu3Which(xgb), &firstItem);
        XtVaSetValues(B_Menu3Button(xgb), XmNmenuHistory, firstItem, NULL);
        XtManageChild(B_Menu3Button(xgb));
        if (B_List3IsStatic(xgb)) {
            XtSetSensitive(B_Menu3Button(xgb), False);
            XgAddHelpCallBackFromFile(B_Menu3Button(xgb), "xgb_men3_stat");
        } else {
            XgAddHelpCallBackFromFile(B_Menu3Button(xgb), "xgb_men3_dynam");
        }
        if (B_SelMode(xgb) == XG_SINGLE_SELECT) {
            XtSetArg(args[0], XmNselectionPolicy, XmSINGLE_SELECT);
        } else {
            XtSetArg(args[0], XmNselectionPolicy, XmMULTIPLE_SELECT);
        }
        XtSetArg(args[1], XmNvisibleItemCount, 10);
        B_List3(xgb) = XmCreateScrolledList(B_List3Container(xgb), "xgbrow_list3", args, 2);

        XtVaSetValues(B_Between3(xgb),
                      XmNtopAttachment, XmATTACH_FORM,
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      NULL);
        XtVaSetValues(XtParent(B_List3(xgb)),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, B_Between3(xgb),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      XmNbottomAttachment, XmATTACH_FORM,
                      NULL);
        XgAddHelpCallBackFromFile(B_List3(xgb), "xgbrow_list3");

        XtAddCallback(B_List3(xgb), XmNsingleSelectionCallback, BrowserListCallback, NULL);
        XtAddCallback(B_List3(xgb), XmNmultipleSelectionCallback, BrowserListCallback, NULL);
        XtAddCallback(B_List3(xgb), XmNextendedSelectionCallback, BrowserListCallback, NULL);
        /* Initialize the list */
        BrowserUpdateList3(B_List3(xgb), B_Menu3Which(xgb));
        XtManageChild(B_List3(xgb));
    } else {
        B_List3(xgb) = B_List3Container(xgb) = B_Menu3(xgb) = B_Menu3Button(xgb) = B_Between3(xgb) = NULL;
    }
    XtManageChild(form);
    return;
}

/****************************************************************/
static void
#ifdef _NO_PROTO
BrowserAdjustAndAttach(xgb, attach)
    BrowserWidget   xgb;
    Boolean         attach;
#else
BrowserAdjustAndAttach(BrowserWidget xgb, Boolean attach)
#endif
{
    Dimension       width = 0;
    Dimension       maxWidth = 0;
    Dimension       height = 0;
    Dimension       maxHeight = 0;

    if (attach) {
        if (B_List1Container(xgb)) {
            XtVaSetValues(B_List1Container(xgb),
                          XmNtopAttachment, XmATTACH_FORM,
                          XmNleftAttachment, XmATTACH_FORM,
                          XmNbottomAttachment, XmATTACH_FORM,
                          NULL);

            if (!B_List2Container(xgb)) {

                XtVaSetValues(B_List1Container(xgb),
                              XmNrightAttachment, XmATTACH_FORM,
                              NULL);

            } else {
                XtVaSetValues(B_List2Container(xgb),
                            XmNy, maxHeight + xgb->manager.shadow_thickness,
                              XmNtopAttachment, XmATTACH_FORM,
                              XmNleftOffset, BB_MarginWidth(xgb),
                              XmNleftAttachment, XmATTACH_WIDGET,
                              XmNleftWidget, B_List1Container(xgb),
                              XmNbottomAttachment, XmATTACH_FORM,
                              NULL);

                if (!B_List3Container(xgb)) {
                    XtVaSetValues(B_List2Container(xgb),
                                  XmNrightAttachment, XmATTACH_FORM,
                                  NULL);

                } else {
                    XtVaSetValues(B_List3Container(xgb),
                                  XmNtopAttachment, XmATTACH_FORM,
                                  XmNleftOffset, BB_MarginWidth(xgb),
                                  XmNleftAttachment, XmATTACH_WIDGET,
                                  XmNleftWidget, B_List2Container(xgb),
                                  XmNrightAttachment, XmATTACH_FORM,
                                  XmNbottomAttachment, XmATTACH_FORM,
                                  NULL);
                }
            }
        }
    }
}

/****************************************************************/
static void
#ifdef _NO_PROTO
Destroy(xgb)
    BrowserWidget   xgb;
#else
Destroy(
        BrowserWidget xgb)
#endif
{
    /* There are memory leaks here. */
    return;
}

/****************************************************************/
static void
#ifdef _NO_PROTO
DeleteChild(w)
    Widget          w;
#else
DeleteChild(
            Widget w)
#endif
/****************
 * This procedure is called to remove the child from
 *   the child list, and to allow the parent to do any
 *   neccessary clean up.
 ****************/
{
    BrowserWidget   xgb;
    Widget          child = w;
    /****************/

    xgb = (BrowserWidget) w;
    while (!XgIsBrowser(xgb))
        xgb = (BrowserWidget) XtParent(xgb);
    if (child == B_List1Container(xgb)) {
        B_List1Container(xgb) = NULL;
        B_List1(xgb) = NULL;
        B_Menu1(xgb) = NULL;
        B_Menu1Button(xgb) = NULL;
        B_Between1(xgb) = NULL;
    }
    if (child == B_List2Container(xgb)) {
        B_List2Container(xgb) = NULL;
        B_List2(xgb) = NULL;
        B_Menu2(xgb) = NULL;
        B_Menu2Button(xgb) = NULL;
        B_Between2(xgb) = NULL;
    }
    if (child == B_List3Container(xgb)) {
        B_List3Container(xgb) = NULL;
        B_List3(xgb) = NULL;
        B_Menu3(xgb) = NULL;
        B_Menu3Button(xgb) = NULL;
        B_Between3(xgb) = NULL;
    }
    (*((InteractorWidgetClass) interactorWidgetClass)
     ->composite_class.delete_child) (w);
}

/****************************************************************/
static          Boolean
#ifdef _NO_PROTO
SetValues(current, request, new)
    BrowserWidget   current;
    BrowserWidget   request;
    BrowserWidget   new;
#else
SetValues(
          BrowserWidget current,
          BrowserWidget request,
          BrowserWidget new)
#endif
/****************
 * This routine detects differences in two versions
 *   of a widget, when a difference is found the
 *   appropriate action is taken.
 ****************/
{
    Arg             args[10];
    int             n;
    String          newString;
    /****************/

    BB_InSetValues(new) = TRUE;

    /*
     * Current, no Browser resources are modifiable through XtSetValues
     */

    if (B_NumLists(new) != B_NumLists(current)) {
        B_NumLists(new) = B_NumLists(current);
    }
    if (B_List1IsStatic(new) != B_List1IsStatic(current)) {
        B_List1IsStatic(new) = B_List1IsStatic(current);
    }
    if (B_List2IsStatic(new) != B_List2IsStatic(current)) {
        B_List2IsStatic(new) = B_List2IsStatic(current);
    }
    if (B_List3IsStatic(new) != B_List3IsStatic(current)) {
        B_List3IsStatic(new) = B_List3IsStatic(current);
    }
    if (B_BrowseMode(new) != B_BrowseMode(current)) {
        B_BrowseMode(new) = B_BrowseMode(current);
    }
    if (B_SelMode(new) != B_SelMode(current)) {
        B_SelMode(new) = B_SelMode(current);
    }
    if (B_InitialMapset1(new) != B_InitialMapset1(current)) {
        B_InitialMapset1(new) = B_InitialMapset1(current);
    }
    if (B_InitialMapset2(new) != B_InitialMapset2(current)) {
        B_InitialMapset2(new) = B_InitialMapset2(current);
    }
    if (B_InitialMapset3(new) != B_InitialMapset3(current)) {
        B_InitialMapset3(new) = B_InitialMapset3(current);
    }
    if (B_ResultString(new) != B_ResultString(current)) {
        B_ResultString(new) = B_ResultString(current);
    }
    BB_InSetValues(new) = FALSE;

    if (XtClass(new) == browserWidgetClass) {
        _XmBulletinBoardSizeUpdate((XmBulletinBoardWidget) new);
    }
    return (FALSE);
}

/*
 * Convenience routines
 */

Widget
#ifdef _NO_PROTO
XgCreateBrowser(p, name, args, n)
    Widget          p;          /* parent widget   */
    String          name;       /* widget name     */
    ArgList         args;       /* arg list        */
    Cardinal        n;          /* arg count       */
#else
XgCreateBrowser(
                Widget p,       /* parent widget   */
                String name,    /* widget name     */
                ArgList args,   /* arg list        */
                Cardinal n)     /* arg count       */
#endif
{
    return (XtCreateWidget(name, browserWidgetClass, p, args, n));
}

Widget
#ifdef _NO_PROTO
XgCreateBrowserDialog(ds_p, name, xgb_args, xgb_n)
    Widget          ds_p;       /* parent for shell    */
    String          name;       /* widget name         */
    ArgList         xgb_args;   /* arglist for xgb      */
    Cardinal        xgb_n;      /* argcount for xgb     */
#else
XgCreateBrowserDialog(
                      Widget ds_p,      /* parent for shell    */
                      String name,      /* widget name         */
                      ArgList xgb_args, /* arglist for xgb      */
                      Cardinal xgb_n)   /* argcount for xgb     */
#endif
/****************
 * This convenience function creates a DialogShell
 *   and a Browser child of the shell;
 *   returns the Browser widget.
 ****************/
{
    Widget          xgb;        /* new xgb widget      */
    Widget          ds;         /* DialogShell         */
    Arg             ds_args[10];/* arglist for shell  */
    char           *ds_name;
    /****************/

    /*
     * Create DialogShell parent.
     */
    ds_name = XtCalloc(strlen(name) + 1, sizeof(char));
    strcpy(ds_name, name);

    XtSetArg(ds_args[0], XmNallowShellResize, True);
    ds = XmCreateDialogShell(ds_p, ds_name, ds_args, 1);

    XtFree(ds_name);

    /*
     * Create Browser
     */
    xgb = XtCreateWidget(name, browserWidgetClass, ds,
                         xgb_args, xgb_n);
    XtAddCallback(xgb, XmNdestroyCallback, _XmDestroyParentCallback, NULL);

    return (xgb);
}
