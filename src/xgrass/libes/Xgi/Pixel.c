#include <PixelP.h>
#include <Interact.h>
#include <color.h>
#include <help.h>

#include <X11/StringDefs.h>
#include <Xm/LabelG.h>
#include <Xm/Text.h>
#include <Xm/DrawingA.h>
#include <Xm/Scale.h>
#include <Xm/ScaleP.h>
#include <Xm/Form.h>
#include <Xm/DialogS.h>

#include <stdio.h>

/*---------------------------------------------------*/
/* forward declarations                              */
/* */
/* this is a list of all private procedures in this  */
/* module                                            */
/*---------------------------------------------------*/

#ifdef _NO_PROTO

static void                     ClassInitialize();
static void                     ClassPartInitialize();
static void                     Initialize();
static void                     Destroy();
static void                     DeleteChild();
static Boolean                  SetValues();
static void                     XgPixelCallBack();
static void                     PixelCreateWidgets();
static void                     PixelCreateScaleWidgets();
static void                     PixelAdjustAndAttach();
static void                     UpdateScale();
static void                     UpdateText();
static void                     SetScaleValues();
static void                     RemoveHelpCallbacks();
static void                     AddHelpCallbacks();

#else                           /* _NO_PROTO */

static void                     ClassInitialize();
static void                     ClassPartInitialize(PixelWidgetClass xgpc);
static void
Initialize(
           PixelWidget request,
           PixelWidget new);
static void                     Destroy(PixelWidget xgp);
static void                     DeleteChild(Widget w);
static                          Boolean
SetValues(
          PixelWidget current,
          PixelWidget request,
          PixelWidget new);
static void
XgPixelCallBack(
          Widget wid,
          XtPointer which_button,
          XmAnyCallbackStruct * callback);
static void
                                PixelCreateWidgets(PixelWidget xgp);
static void
PixelCreateScaleWidgets(
                        PixelWidget xgp, Widget junk,
                        int which);
static void
                                PixelAdjustAndAttach(PixelWidget xgp, Boolean attach);
static void
UpdateScale(
            Widget wid,
            XtPointer client_data,
            XtPointer call_data);
static void
UpdateText(
           Widget wid,
           XtPointer client_data,
           XtPointer call_data);
static void
SetScaleValues(
               PixelWidget xgp,unsigned char junk);
static void
RemoveHelpCallbacks(
                    PixelWidget xgp);
static void
AddHelpCallbacks(
                 int which,
                 PixelWidget xgp);

#endif                          /* _NO_PROTO */

#ifndef MCCABE
static char                     defaultTranslations[] =
"<Key>osfSelect:         ManagerGadgetSelect()\n\
        <Key>osfHelp:           XgInteractorHelp()\n\
        <Key>osfCancel:         BulletinBoardCancel()\n\
        ~Shift ~Meta ~Alt <Key>space:   ManagerGadgetSelect()\n\
        <Key>:                  ManagerGadgetKeyInput()\n\
        <BtnMotion>:    ManagerGadgetButtonMotion()\n\
        <Btn1Down>:     ManagerGadgetArm()\n\
        <Btn1Down>,<Btn1Up>:    ManagerGadgetActivate()\n\
        <Btn1Up>:       ManagerGadgetActivate()\n\
        <Btn1Down>(2+): ManagerGadgetMultiArm()\n\
        <Btn1Up>(2+):   ManagerGadgetMultiActivate()";

static char                     defaultAccelerators[] =
"\043override\n\
        <Key>osfCancel:         BulletinBoardCancel()";
#else
static char                     defaultTranslations[];
static char                     defaultAccelerators[];
#endif

static XtActionsRec             actionsList[] =
{
    {"Enter", (XtActionProc) _XmManagerEnter},  /* Motif 1.0 */
    {"FocusIn", (XtActionProc) _XmManagerFocusIn},      /* Motif 1.0 */
    {"Arm", (XtActionProc) _XmGadgetArm},       /* Motif 1.0 */
    {"Activate", (XtActionProc) _XmGadgetActivate},     /* Motif 1.0 */
    {"XgInteractorHelp", (XtActionProc) _XgInteractorHelpAction},       /* Motif 1.0 */
    {"BulletinBoardCancel", (XtActionProc) _XmBulletinBoardCancel},
};


/*---------------------------------------------------*/
/* widget resources                                  */
/*---------------------------------------------------*/
static XtResource               resources[] =
{
    /* xgpixel specific resources */

    {XmNscaleTypeMask,
        XmCScaleTypeMask,
        XmRScaleTypeMask,
        sizeof(unsigned char),
        XtOffset(PixelWidget, pixel.scale_type_mask),
        XmRScaleTypeMask,
        (XtPointer) XgRGB
    },
    {XmNcolormap,
        XmCColormap,
        XmRColormap,
        sizeof(XtPointer),
        XtOffset(CoreWidget, core.colormap),
        XtRCallProc,
        (XtPointer) NULL
    },
    {XmNxColor,
        XmCXColor,
        XmRXColor,
        sizeof(XtPointer),
        XtOffset(PixelWidget, pixel.xcolor),
        XmRXColor,
        NULL
    },
    {XmNxColorOriginal,
        XmCXColorOriginal,
        XmRXColor,
        sizeof(XtPointer),
        XtOffset(PixelWidget, pixel.saved),
        XmRXColor,
        NULL
    },
    {XmNscale1LabelString,
        XmCScale1LabelString,
        XmRXmString,
        sizeof(XmString),
        XtOffset(PixelWidget, pixel.scale1_label_string),
        XmRString,
        NULL
    },
    {XmNscale2LabelString,
        XmCScale2LabelString,
        XmRXmString,
        sizeof(XmString),
        XtOffset(PixelWidget, pixel.scale2_label_string),
        XmRString,
        NULL
    },
    {XmNscale3LabelString,
        XmCScale3LabelString,
        XmRXmString,
        sizeof(XmString),
        XtOffset(PixelWidget, pixel.scale3_label_string),
        XmRString,
        NULL
    },
    {XmNcancelRestore,
        XmCCancelRestore,
        XmRBoolean,
        sizeof(Boolean),
        XtOffset(PixelWidget, pixel.cancel_restore),
        XmRImmediate,
        (XtPointer) FALSE
    },
    /* superclass resource default overrides */

    {XmNautoUnmanage,
        XmCAutoUnmanage,
        XmRBoolean,
        sizeof(Boolean),
        XtOffset(PixelWidget, bulletin_board.auto_unmanage),
        XmRImmediate,
        (XtPointer) FALSE
    },
    {XmNdialogType,
        XmCDialogType,
        XmRDialogType,
        sizeof(unsigned char),
        XtOffset(PixelWidget, interactor.dialog_type),
        XmRImmediate,
        (XtPointer) XmINTERACT_WORK_AREA_TYPE
    },

    {XmNaccelerators,
        XmCAccelerators, XmRAcceleratorTable, sizeof(XtAccelerators),
        XtOffset(XmBulletinBoardWidget, core.accelerators),
        XmRString, (XtPointer) defaultAccelerators
    },

};

/* KAB -- I don't think we're done above...yet */
static XmSyntheticResource      syn_resources[] =
{
    /* synthetic resources go here KAB */
    {NULL, 0, 0, NULL, NULL}
};

externaldef(xgpixelclassrec)
    PixelClassRec                   pixelClassRec =
    {
        {                       /* core class record        */
             /* superclass          */ (WidgetClass) & interactorClassRec,
             /* class_name          */ "Pixel",
             /* widget_size         */ sizeof(PixelRec),
             /* class_initialize    */ ClassInitialize,
             /* class part init     */ ClassPartInitialize,
             /* class_inited        */ FALSE,
             /* initialize          */ Initialize,
             /* initialize hook     */ NULL,
             /* realize             */ _XtInherit,
             /* actions             */ actionsList,
             /* num_actions         */ XtNumber(actionsList),
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
             /* tm_table            */ defaultTranslations,
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
        {                       /* pixel class record */
             /* extension */ NULL,
        }
    };

externaldef(xgpixelwidgetclass) WidgetClass
pixelWidgetClass = (WidgetClass) & pixelClassRec;

/*
 * FUNCTION
 */
    static void
                                    ClassInitialize()
{

    XgRegisterScaleTypeConverter();
    return;
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
ClassPartInitialize(xgp)
    PixelWidgetClass                xgp;
#else
ClassPartInitialize(
                    PixelWidgetClass xgp)
#endif
{

    _XmFastSubclassInit(xgp, XmPIXEL_BIT);

    return;
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
Initialize(request, new)
    PixelWidget                     request;
    PixelWidget                     new;
#else
Initialize(
           PixelWidget request,
           PixelWidget new)
#endif
{
    Arg                             args[16];
    int                             numArgs;
    XColor                         *xcolor;

    /*
     * Make sure we have a valid scale type mask
     */
    switch (P_ScaleType(request)) {
    case XgRGB:
    case XgHSV:
    case XgCMY:
        break;
    default:
        _XmWarning(new, "Invalid scale type mask");
        P_ScaleType(new) = XgRGB;
        break;
    }
    /*
     * Create all of the new widgets.
     */
    PixelCreateWidgets(new);

    /*
     * Add a default callback to cancel that restores the color table...
     * If the user has specified to do that...
     */
    if ( P_CancelRestore(new) ) 
	XtAddCallback(I_CancelButton(new), XmNactivateCallback,
                  XgPixelCallBack, XmINTERACT_CANCEL_BUTTON);
    if (request->manager.navigation_type != XmEXCLUSIVE_TAB_GROUP)
        _XmChangeNavigationType((Widget) new, ((XtIsShell(XtParent(request))
                                       ? XmSTICKY_TAB_GROUP : XmTAB_GROUP)));

    /*
     * Make a copy in case the user has used a local variable or something...
     * and also make a copy that we'll save to reset on receipt of a
     * Cancel...
     */
    xcolor = (XColor *) XtMalloc(sizeof(XColor));
    bcopy((char *) P_XColor(new), (char *) xcolor, sizeof(XColor));
    P_XColor(new) = xcolor;
    xcolor = (XColor *) XtMalloc(sizeof(XColor));
    bcopy((char *) P_XColor(new), (char *) xcolor, sizeof(XColor));
    P_XColorSaved(new) = xcolor;

    return;
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
PixelCreateWidgets(xgp)
    PixelWidget                     xgp;
#else
PixelCreateWidgets(PixelWidget xgp)
#endif
{
    Widget                          form;
    Arg                             args[5];
    int                             nargs = 0;
    XColor                         *xcolor = P_XColor(xgp);
    char                            help[256];

    /*
     * We'll use a form to contain all of the pixel widget kids since the
     * interactor only allows one work area child.
     */
    form = XmCreateForm((Widget) xgp, "xgpixel_form", args, nargs);
    /*
     * Add a help callback for the pixel widget work area. We can use
     * I_WorkArea because the Interactors' InsertChild routine has set
     * it...but anal retention is sometimes desireable...
     */
    if (I_WorkArea(xgp) != NULL) {
        XgAddHelpCallback(I_WorkArea(xgp), PIXEL_WIDGET_HELP, NULL);
    }
    XtSetArg(args[nargs], XmNwidth, 100);
    nargs++;
    XtSetArg(args[nargs], XmNtraversalOn, False);
    nargs++;
    if (xcolor != NULL){
      XtSetArg(args[nargs], XmNbackground, xcolor->pixel);
      nargs++;
    }
    P_DisplayArea(xgp) = XmCreateDrawingArea(form,
                                       "xgpixel_display_area", args, nargs);

    XgAddHelpCallback(P_DisplayArea(xgp),
                       "The current color is displayed here.", NULL);

    XtManageChild(P_DisplayArea(xgp));

    PixelCreateScaleWidgets(xgp, form, (int) XgSCALE1);
    PixelCreateScaleWidgets(xgp, form, (int) XgSCALE2);
    PixelCreateScaleWidgets(xgp, form, (int) XgSCALE3);
    PixelAdjustAndAttach(xgp, True);

    XtManageChild(form);

    return;
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
PixelAdjustAndAttach(xgp, attach)
    PixelWidget                     xgp;
    Boolean                         attach;
#else
PixelAdjustAndAttach(PixelWidget xgp, Boolean attach)
#endif
{
    Dimension                       height = 0;
    Dimension                       maxHeight = 0;
    Dimension                       width = 0;
    Dimension                       maxLabelWidth = 0;
    Dimension                       maxTextWidth = 0;

    XtVaGetValues(P_Scale1Label(xgp), XmNwidth, &width,
                  XmNheight, &height, NULL);
    if (height > maxHeight)
        maxHeight = height;
    if (width > maxLabelWidth)
        maxLabelWidth = width;
    XtVaGetValues(P_Scale2Label(xgp), XmNwidth, &width, NULL);
    if (width > maxLabelWidth)
        maxLabelWidth = width;
    XtVaGetValues(P_Scale3Label(xgp), XmNwidth, &width, NULL);
    if (width > maxLabelWidth)
        maxLabelWidth = width;

    XtVaGetValues(P_Scale1Text(xgp), XmNheight, &height, NULL);
    if (height > maxHeight)
        maxHeight = height;

    XtVaGetValues(P_Scale1Text(xgp), XmNwidth, &width,
                  XmNheight, &height, NULL);
    if (height > maxHeight)
        maxHeight = height;
    if (width > maxTextWidth)
        maxTextWidth = width;
    XtVaGetValues(P_Scale2Text(xgp), XmNwidth, &width, NULL);
    if (width > maxTextWidth)
        maxTextWidth = width;
    XtVaGetValues(P_Scale3Text(xgp), XmNwidth, &width, NULL);
    if (width > maxTextWidth)
        maxTextWidth = width;

    XtVaSetValues(P_Scale1Label(xgp),
                  XmNheight, maxHeight,
                  XmNwidth, maxLabelWidth,
                  NULL);
    if (attach) {
        XtVaSetValues(P_Scale1Label(xgp),
                      XmNtopAttachment, XmATTACH_FORM,
                      XmNleftAttachment, XmATTACH_FORM,
                      NULL);
    }
    XtVaSetValues(P_Scale1Text(xgp),
                  XmNheight, maxHeight,
                  XmNwidth, maxTextWidth,
                  NULL);
    if (attach) {
        XtVaSetValues(P_Scale1Text(xgp),
                      XmNtopAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      NULL);
    }
    XtVaSetValues(P_Scale1(xgp),
                  XmNheight, maxHeight,
                  NULL);
    if (attach) {
        XtVaSetValues(P_Scale1(xgp),
                      XmNtopAttachment, XmATTACH_FORM,
                      XmNleftAttachment, XmATTACH_WIDGET,
                      XmNleftWidget, P_Scale1Label(xgp),
                      XmNrightAttachment, XmATTACH_WIDGET,
                      XmNrightWidget, P_Scale1Text(xgp),
                      XmNleftOffset, BB_MarginWidth(xgp),
                      XmNrightOffset, BB_MarginWidth(xgp),
                      NULL);
    }
    XtVaSetValues(P_Scale2Label(xgp),
                  XmNheight, maxHeight,
                  XmNwidth, maxLabelWidth,
                  NULL);
    if (attach) {
        XtVaSetValues(P_Scale2Label(xgp),
                      XmNtopOffset, BB_MarginHeight(xgp),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, P_Scale1Label(xgp),
                      XmNleftAttachment, XmATTACH_FORM,
                      NULL);
    }
    XtVaSetValues(P_Scale2Text(xgp),
                  XmNheight, maxHeight,
                  XmNwidth, maxTextWidth,
                  NULL);
    if (attach) {
        XtVaSetValues(P_Scale2Text(xgp),
                      XmNtopOffset, BB_MarginHeight(xgp),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, P_Scale1Text(xgp),
                      XmNrightAttachment, XmATTACH_FORM,
                      NULL);
    }
    XtVaSetValues(P_Scale2(xgp),
                  XmNheight, maxHeight,
                  NULL);
    if (attach) {
        XtVaSetValues(P_Scale2(xgp),
                      XmNtopOffset, BB_MarginHeight(xgp),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, P_Scale1(xgp),
                      XmNleftAttachment, XmATTACH_WIDGET,
                      XmNleftWidget, P_Scale2Label(xgp),
                      XmNrightAttachment, XmATTACH_WIDGET,
                      XmNrightWidget, P_Scale2Text(xgp),
                      XmNleftOffset, BB_MarginWidth(xgp),
                      XmNrightOffset, BB_MarginWidth(xgp),
                      NULL);
    }
    XtVaSetValues(P_Scale3Label(xgp),
                  XmNheight, maxHeight,
                  XmNwidth, maxLabelWidth,
                  NULL);
    if (attach) {
        XtVaSetValues(P_Scale3Label(xgp),
                      XmNtopOffset, BB_MarginHeight(xgp),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, P_Scale2Label(xgp),
                      XmNleftAttachment, XmATTACH_FORM,
                      NULL);
    }
    XtVaSetValues(P_Scale3Text(xgp),
                  XmNheight, maxHeight,
                  XmNwidth, maxTextWidth,
                  NULL);
    if (attach) {
        XtVaSetValues(P_Scale3Text(xgp),
                      XmNtopOffset, BB_MarginHeight(xgp),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, P_Scale2Text(xgp),
                      XmNrightAttachment, XmATTACH_FORM,
                      NULL);
    }
    XtVaSetValues(P_Scale3(xgp),
                  XmNheight, maxHeight,
                  NULL);
    if (attach) {
        XtVaSetValues(P_Scale3(xgp),
                      XmNtopOffset, BB_MarginHeight(xgp),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, P_Scale2(xgp),
                      XmNleftAttachment, XmATTACH_WIDGET,
                      XmNleftWidget, P_Scale3Label(xgp),
                      XmNrightAttachment, XmATTACH_WIDGET,
                      XmNrightWidget, P_Scale3Text(xgp),
                      XmNleftOffset, BB_MarginWidth(xgp),
                      XmNrightOffset, BB_MarginWidth(xgp),
                      NULL);
    }
    if (attach) {
        XtVaSetValues(P_DisplayArea(xgp),
                      XmNtopOffset, BB_MarginHeight(xgp),
                      XmNtopAttachment, XmATTACH_WIDGET,
                      XmNtopWidget, P_Scale3(xgp),
                      XmNleftAttachment, XmATTACH_FORM,
                      XmNrightAttachment, XmATTACH_FORM,
                      XmNbottomAttachment, XmATTACH_FORM,
                      NULL);
    }
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
PixelCreateScaleWidgets(xgp, form, which)
    PixelWidget                     xgp;
    Widget                          form;
    int                             which;
#else
PixelCreateScaleWidgets(PixelWidget xgp, Widget form, int which)
#endif
{
    Arg                             label_args[10];
    int                             label_nargs = 0;
    Arg                             scale_args[10];
    int                             scale_nargs = 0;
    Arg                             text_args[10];
    int                             text_nargs = 0;
    RGB                             rgb;
    HSV                             hsv;
    CMY                             cmy;
    XColor                         *xcolor = P_XColor(xgp);
    XmString                        empty, xms;
    char                            text_value[10];
    Dimension                       height;

    if (xcolor != NULL){
      rgb.r = xcolor->red;
      rgb.g = xcolor->green;
      rgb.b = xcolor->blue;
    }
    else{
      rgb.r = 0;
      rgb.g = 0;
      rgb.b = 0;
    }

    XtSetArg(scale_args[scale_nargs], XmNwidth, 256);
    scale_nargs++;
    XtSetArg(scale_args[scale_nargs], XmNshowValue, False);
    scale_nargs++;
    XtSetArg(scale_args[scale_nargs], XmNorientation, XmHORIZONTAL);
    scale_nargs++;
    XtSetArg(scale_args[scale_nargs], XmNminimum, 0);
    scale_nargs++;

    XtSetArg(label_args[label_nargs], XmNtraversalOn, False);
    label_nargs++;
    XtSetArg(scale_args[scale_nargs], XmNnavigationType, XmSTICKY_TAB_GROUP);
    scale_nargs++;
    XtSetArg(text_args[text_nargs], XmNnavigationType, XmSTICKY_TAB_GROUP);
    text_nargs++;

    XtSetArg(text_args[text_nargs], XmNcolumns, 5);
    text_nargs++;
    XtSetArg(text_args[text_nargs], XmNmarginHeight, 0);
    text_nargs++;

    switch (P_ScaleType(xgp)) {
    case XgRGB:
        XtSetArg(scale_args[scale_nargs], XmNmaximum, 255);
        scale_nargs++;
        break;
    case XgHSV:
        XtSetArg(scale_args[scale_nargs], XmNmaximum, 1000);
        scale_nargs++;
        XtSetArg(scale_args[scale_nargs], XmNdecimalPoints, 3);
        scale_nargs++;
        hsv = RGBToHSV(rgb);
        break;
    case XgCMY:
        XtSetArg(scale_args[scale_nargs], XmNmaximum, 255);
        scale_nargs++;
        cmy = RGBToCMY(rgb);
        break;
    }

    switch (which) {
    case XgSCALE1:

        switch (P_ScaleType(xgp)) {
        case XgRGB:
            xms = XmStringCreateSimple("Red");
            sprintf(text_value, "%3d", (int) ((float)rgb.r / (65536. / 256.)));
            XtSetArg(text_args[text_nargs], XmNvalue, text_value);
            text_nargs++;
            XtSetArg(scale_args[scale_nargs], XmNvalue, (int) ((float)rgb.r / (65536. / 256.)));
            scale_nargs++;
            break;
        case XgHSV:
            xms = XmStringCreateSimple("Hue");
            sprintf(text_value, "%0.3f", hsv.h);
            XtSetArg(text_args[text_nargs], XmNvalue, text_value);
            text_nargs++;
            XtSetArg(scale_args[scale_nargs], XmNvalue, (int) (hsv.h * 1000.0));
            scale_nargs++;
            break;
        case XgCMY:
            XtSetArg(scale_args[scale_nargs], XmNvalue, (int) ((float)cmy.c / (65536. / 256.)));
            scale_nargs++;
            sprintf(text_value, "%3d", (int) ((float)cmy.c / (65536. / 256.)));
            XtSetArg(text_args[text_nargs], XmNvalue, text_value);
            text_nargs++;
            xms = XmStringCreateSimple("Cyan");
            break;
        }
        XtSetArg(label_args[label_nargs], XmNlabelString, xms);
        label_nargs++;
        P_Scale1Label(xgp) = XmCreateLabelGadget(form,
                           "xgpixel_scale1_label", label_args, label_nargs);

        XtManageChild(P_Scale1Label(xgp));
        XmStringFree(xms);

        XtVaGetValues(P_Scale1Label(xgp), XmNheight, &height, NULL);
        XtSetArg(scale_args[scale_nargs], XmNheight, height);
        scale_nargs++;

        P_Scale1(xgp) = XmCreateScale(form,
                                 "xgpixel_scale1", scale_args, scale_nargs);

        XtManageChild(P_Scale1(xgp));

        XtAddCallback(P_Scale1(xgp), XmNvalueChangedCallback,
                      UpdateScale, (XtPointer) xgp);
        XtAddCallback(P_Scale1(xgp), XmNdragCallback,
                      UpdateScale, (XtPointer) xgp);

        P_Scale1Text(xgp) = XmCreateText(form,
                              "xgpixel_scale1_text", text_args, text_nargs);

        XtManageChild(P_Scale1Text(xgp));

        XtAddCallback(P_Scale1Text(xgp), XmNactivateCallback,
                      UpdateText, (XtPointer) xgp);

        break;
    case XgSCALE2:

        switch (P_ScaleType(xgp)) {
        case XgRGB:
            sprintf(text_value, "%3d", (int) ((float)rgb.g / (65536. / 256.)));
            XtSetArg(text_args[text_nargs], XmNvalue, text_value);
            text_nargs++;
            XtSetArg(scale_args[scale_nargs], XmNvalue, (int) ((float)rgb.g / (65536. / 256.)));
            scale_nargs++;
            xms = XmStringCreateSimple("Green");
            break;
        case XgHSV:
            sprintf(text_value, "%0.3f", hsv.s);
            XtSetArg(text_args[text_nargs], XmNvalue, text_value);
            text_nargs++;
            XtSetArg(scale_args[scale_nargs], XmNvalue, (int) (hsv.s * 1000.0));
            scale_nargs++;
            xms = XmStringCreateSimple("Saturation");
            break;
        case XgCMY:
            sprintf(text_value, "%3d", (int) ((float)cmy.m / (65536. / 256.)));
            XtSetArg(text_args[text_nargs], XmNvalue, text_value);
            text_nargs++;
            XtSetArg(scale_args[scale_nargs], XmNvalue, (int) ((float)cmy.m / (65536. / 256.)));
            scale_nargs++;
            xms = XmStringCreateSimple("Magenta");
            break;
        }
        XtSetArg(label_args[label_nargs], XmNlabelString, xms);
        label_nargs++;
        P_Scale2Label(xgp) = XmCreateLabelGadget(form,
                           "xgpixel_scale2_label", label_args, label_nargs);
        XtManageChild(P_Scale2Label(xgp));

        XtVaGetValues(P_Scale2Label(xgp), XmNheight, &height, NULL);
        XtSetArg(scale_args[scale_nargs], XmNheight, height);
        scale_nargs++;
        P_Scale2(xgp) = XmCreateScale(form,
                                 "xgpixel_scale2", scale_args, scale_nargs);
        XtManageChild(P_Scale2(xgp));

        XtAddCallback(P_Scale2(xgp), XmNvalueChangedCallback,
                      UpdateScale, (XtPointer) xgp);
        XtAddCallback(P_Scale2(xgp), XmNdragCallback,
                      UpdateScale, (XtPointer) xgp);

        P_Scale2Text(xgp) = XmCreateText(form,
                              "xgpixel_scale2_text", text_args, text_nargs);
        XtManageChild(P_Scale2Text(xgp));

        XtAddCallback(P_Scale2Text(xgp), XmNactivateCallback,
                      UpdateText, (XtPointer) xgp);
        break;
    case XgSCALE3:

        switch (P_ScaleType(xgp)) {
        case XgRGB:
            sprintf(text_value, "%3d", (int) ((float)rgb.b / (65536. / 256.)));
            XtSetArg(text_args[text_nargs], XmNvalue, text_value);
            text_nargs++;
            XtSetArg(scale_args[scale_nargs], XmNvalue, (int) ((float)rgb.b / (65536. / 256.)));
            scale_nargs++;
            xms = XmStringCreateSimple("Blue");
            break;
        case XgHSV:
            sprintf(text_value, "%0.3f", hsv.v);
            XtSetArg(text_args[text_nargs], XmNvalue, text_value);
            text_nargs++;
            XtSetArg(scale_args[scale_nargs], XmNvalue, (int) (hsv.v * 1000.0));
            scale_nargs++;
            xms = XmStringCreateSimple("Value");
            break;
        case XgCMY:
            sprintf(text_value, "%3d", (int) ((float)cmy.y / (65536. / 256.)));
            XtSetArg(text_args[text_nargs], XmNvalue, text_value);
            text_nargs++;
            XtSetArg(scale_args[scale_nargs], XmNvalue, (int) ((float)cmy.y / (65536. / 256.)));
            scale_nargs++;
            xms = XmStringCreateSimple("Yellow");
            break;
        }
        XtSetArg(label_args[label_nargs], XmNlabelString, xms);
        label_nargs++;
        P_Scale3Label(xgp) = XmCreateLabelGadget(form,
                           "xgpixel_scale3_label", label_args, label_nargs);
        XtManageChild(P_Scale3Label(xgp));

        XtVaGetValues(P_Scale3Label(xgp), XmNheight, &height, NULL);
        XtSetArg(scale_args[scale_nargs], XmNheight, height);
        scale_nargs++;
        P_Scale3(xgp) = XmCreateScale(form,
                                 "xgpixel_scale3", scale_args, scale_nargs);
        XtManageChild(P_Scale3(xgp));

        XtAddCallback(P_Scale3(xgp), XmNvalueChangedCallback,
                      UpdateScale, (XtPointer) xgp);
        XtAddCallback(P_Scale3(xgp), XmNdragCallback,
                      UpdateScale, (XtPointer) xgp);

        P_Scale3Text(xgp) = XmCreateText(form,
                              "xgpixel_scale3_text", text_args, text_nargs);
        XtManageChild(P_Scale3Text(xgp));

        XtAddCallback(P_Scale3Text(xgp), XmNactivateCallback,
                      UpdateText, (XtPointer) xgp);
        break;
    default:
        return;
    }
    AddHelpCallbacks(which, xgp);
}

static void
#ifdef _NO_PROTO
RemoveHelpCallbacks(xgp)
    PixelWidget                     xgp;
#else
RemoveHelpCallbacks(PixelWidget xgp)
#endif
{
    XtRemoveAllCallbacks(P_Scale1Label(xgp), XmNhelpCallback);
    XtRemoveAllCallbacks(P_Scale2Label(xgp), XmNhelpCallback);
    XtRemoveAllCallbacks(P_Scale3Label(xgp), XmNhelpCallback);

    XtRemoveAllCallbacks(P_Scale1(xgp), XmNhelpCallback);
    XtRemoveAllCallbacks(P_Scale2(xgp), XmNhelpCallback);
    XtRemoveAllCallbacks(P_Scale3(xgp), XmNhelpCallback);

    XtRemoveAllCallbacks(P_Scale1Text(xgp), XmNhelpCallback);
    XtRemoveAllCallbacks(P_Scale2Text(xgp), XmNhelpCallback);
    XtRemoveAllCallbacks(P_Scale3Text(xgp), XmNhelpCallback);
}

static void
#ifdef _NO_PROTO
AddHelpCallbacks(which, xgp)
    int                             which;
    PixelWidget                     xgp;
#else
AddHelpCallbacks(int which, PixelWidget xgp)
#endif
{
    Widget                          widget1;
    Widget                          widget2;
    Widget                          widget3;
    char                            help[1024];
    char                            thelp[1024];
    static char                    *component[3][3] = {
        {"Red", "Hue", "Cyan"},
        {"Green", "Saturation", "Magenta"},
        {"Blue", "Value", "Yellow"},
    };

    if (which == XgSCALE1) {
        widget1 = P_Scale1Label(xgp);
        widget2 = P_Scale1(xgp);
        widget3 = P_Scale1Text(xgp);
    } else if (which == XgSCALE2) {
        widget1 = P_Scale2Label(xgp);
        widget2 = P_Scale2(xgp);
        widget3 = P_Scale2Text(xgp);
    } else {
        widget1 = P_Scale3Label(xgp);
        widget2 = P_Scale3(xgp);
        widget3 = P_Scale3Text(xgp);
    }
    XgAddHelpCallback(widget1, "This label indicates that the",
                       component[which - 1][P_ScaleType(xgp)],
                       "component of the color will be modified by ",
                "manipulating the scale or text area widgets to the right.",
                       GENERIC_LABEL_HELP, NULL);

    XgAddHelpCallback(widget2,
                       "Manipulating this scale widget will modify the",
          component[which - 1][P_ScaleType(xgp)], "component of the color.",
                       GENERIC_SCALE_HELP, NULL);

    XgAddHelpCallback(widget3, "Changing this text field will modify the",
                       component[which - 1][P_ScaleType(xgp)],
                       "component of the color.",
                       (P_ScaleType(xgp) == XgHSV ?
               "You should enter a decimal value in the range 0.0 to 1.0." :
                "You should enter an integer value in the range 0 to 255."),
               "You must hit the return key to activate the change.", NULL);
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
UpdateScale(w, cl, ca)
    Widget                          w;
    XtPointer                       cl, ca;
#else
UpdateScale(Widget w, XtPointer cl, XtPointer ca)
#endif
{
    PixelWidget                     xgp = (PixelWidget) cl;
    int                             which;
    int                             value;
    RGB                             rgb;
    HSV                             hsv;
    CMY                             cmy;
    char                            text_value[10];
    XColor                         *xcolor;

    xcolor = P_XColor(xgp);

    if (P_Scale1(xgp) == w)
        which = XgSCALE1;
    else if (P_Scale2(xgp) == w)
        which = XgSCALE2;
    else
        which = XgSCALE3;

    XtVaGetValues(w, XmNvalue, &value, NULL);

    if (xcolor != NULL){
      rgb.r = xcolor->red;
      rgb.g = xcolor->green;
      rgb.b = xcolor->blue;
    }
    else{
      rgb.r = 0;
      rgb.g = 0;
      rgb.b = 0;
    }
      
    hsv = RGBToHSV(rgb);
    cmy = RGBToCMY(rgb);

    switch (P_ScaleType(xgp)) {
    case XgRGB:
        sprintf(text_value, "%3d", value);
        break;
    case XgHSV:
        sprintf(text_value, "%0.3f", (value / 1000.0));
        break;
    case XgCMY:
        sprintf(text_value, "%3d", value);
        break;
    }
    switch (which) {
    case XgSCALE1:
        switch (P_ScaleType(xgp)) {
        case XgRGB:
            rgb.r = value * 65536 / 256;
            break;
        case XgHSV:
            hsv.h = ((float) value) / 1000.0;
            rgb = HSVToRGB(hsv);
            break;
        case XgCMY:
            cmy.c = value * 65536 / 256;
            rgb = CMYToRGB(cmy);
            break;
        }
        XmTextSetString(P_Scale1Text(xgp), text_value);
        break;
    case XgSCALE2:
        switch (P_ScaleType(xgp)) {
        case XgRGB:
            rgb.g = value * 65536 / 256;
            break;
        case XgHSV:
            hsv.s = ((float) value) / 1000.0;
            rgb = HSVToRGB(hsv);
            break;
        case XgCMY:
            cmy.m = value * 65536 / 256;
            rgb = CMYToRGB(cmy);
            break;
        }
        XmTextSetString(P_Scale2Text(xgp), text_value);
        break;
    case XgSCALE3:
        switch (P_ScaleType(xgp)) {
        case XgRGB:
            rgb.b = value * 65536 / 256;
            break;
        case XgHSV:
            hsv.v = ((float) value) / 1000.0;
            rgb = HSVToRGB(hsv);
            break;
        case XgCMY:
            cmy.y = value * 65536 / 256;
            rgb = CMYToRGB(cmy);
            break;
        }
        XmTextSetString(P_Scale3Text(xgp), text_value);
        break;
    }
    RGBToXColor(rgb, xcolor);
    P_XColor(xgp) = xcolor;
    XStoreColor(XtDisplay(xgp), P_Colormap(xgp), xcolor);
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
UpdateText(w, cl, ca)
    Widget                          w;
    XtPointer                       cl, ca;
#else
UpdateText(Widget w, XtPointer cl, XtPointer ca)
#endif
{
    PixelWidget                     xgp = (PixelWidget) cl;
    int                             which;
    char                           *text_value;
    RGB                             rgb;
    HSV                             hsv;
    CMY                             cmy;
    int                             value;
    char                           *remnant;
    XColor                         *xcolor = P_XColor(xgp);

    if (P_Scale1Text(xgp) == w)
        which = XgSCALE1;
    else if (P_Scale2Text(xgp) == w)
        which = XgSCALE2;
    else
        which = XgSCALE3;

    XtVaGetValues(w, XmNvalue, &text_value, NULL);

    if (xcolor != NULL){
      rgb.r = xcolor->red;
      rgb.g = xcolor->green;
      rgb.b = xcolor->blue;
    }
    else{
      rgb.r = 0;
      rgb.g = 0;
      rgb.b = 0;
    }
    
    hsv = RGBToHSV(rgb);
    cmy = RGBToCMY(rgb);

    switch (P_ScaleType(xgp)) {
    case XgRGB:
        value = strtol(text_value, &remnant, 10);
        if (value <= 0) {
            value = 0;
            if (remnant != NULL)
                XmTextSetString(w, "0");
        }
        if (value > 255) {
            value = 255;
            if (remnant != NULL)
                XmTextSetString(w, "255");
        }
        break;
    case XgHSV:
        value = strtol(text_value, &remnant, 10);
        if (value <= 0) {
            value = 0;
            if (remnant != NULL)
                XmTextSetString(w, "0");
        }
        if (value > 1000) {
            value = 1000;
            if (remnant != NULL)
                XmTextSetString(w, "1000");
        }
        break;
    case XgCMY:
        value = strtol(text_value, &remnant, 10);
        if (value <= 0) {
            value = 0;
            if (remnant != NULL)
                XmTextSetString(w, "0");
        }
        if (value > 255) {
            value = 255;
            if (remnant != NULL)
                XmTextSetString(w, "255");
        }
        break;
    }
    switch (which) {
    case XgSCALE1:
        switch (P_ScaleType(xgp)) {
        case XgRGB:
            rgb.r = value * (65536 / 256);
            break;
        case XgHSV:
            hsv.h = ((float) value) / 1000.0;
            rgb = HSVToRGB(hsv);
            break;
        case XgCMY:
            cmy.c = value * (65536 / 256);
            rgb = CMYToRGB(cmy);
            break;
        }
        XmScaleSetValue(P_Scale1(xgp), value);
        break;
    case XgSCALE2:
        switch (P_ScaleType(xgp)) {
        case XgRGB:
            rgb.g = value * (65536 / 256);
            break;
        case XgHSV:
            hsv.s = ((float) value) / 1000.0;
            rgb = HSVToRGB(hsv);
            break;
        case XgCMY:
            cmy.m = value * (65536 / 256);
            rgb = CMYToRGB(cmy);
            break;
        }
        XmScaleSetValue(P_Scale2(xgp), value);
        break;
    case XgSCALE3:
        switch (P_ScaleType(xgp)) {
        case XgRGB:
            rgb.b = value * (65536 / 256);
            break;
        case XgHSV:
            hsv.v = ((float) value) / 1000.0;
            rgb = HSVToRGB(hsv);
            break;
        case XgCMY:
            cmy.y = value * (65536 / 256);
            rgb = CMYToRGB(cmy);
            break;
        }
        XmScaleSetValue(P_Scale3(xgp), value);
        break;
    }
    RGBToXColor(rgb, xcolor);
    P_XColor(xgp) = xcolor;
    XStoreColor(XtDisplay(xgp), P_Colormap(xgp), xcolor);
}


/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
Destroy(xgp)
    PixelWidget                     xgp;
#else
Destroy(
        PixelWidget xgp)
#endif
{
    return;
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
DeleteChild(w)
    Widget                          w;
#else
DeleteChild(
            Widget w)
#endif
{
    PixelWidget                     xgp;

    if (XtIsRectObj(w)) {
        xgp = (PixelWidget) XtParent(XtParent(w));

        if (w == P_Scale1Label(xgp)) {
            P_Scale1Label(xgp) = NULL;
        } else {
            if (w == P_Scale1(xgp)) {
                P_Scale1(xgp) = NULL;
            } else {
                if (w == P_Scale1Text(xgp)) {
                    P_Scale1Text(xgp) = NULL;
                } else {
                    if (w == P_Scale2Label(xgp)) {
                        P_Scale2Label(xgp) = NULL;
                    } else {
                        if (w == P_Scale2(xgp)) {
                            P_Scale2(xgp) = NULL;
                        } else {
                            if (w == P_Scale2Text(xgp)) {
                                P_Scale2Text(xgp) = NULL;
                            } else {
                                if (w == P_Scale3Label(xgp)) {
                                    P_Scale3Label(xgp) = NULL;
                                } else {
                                    if (w == P_Scale3(xgp)) {
                                        P_Scale3(xgp) = NULL;
                                    } else {
                                        if (w == P_Scale3Text(xgp)) {
                                            P_Scale3Text(xgp) = NULL;
                                        } else {
                                            if (w == P_DisplayArea(xgp)) {
                                                P_DisplayArea(xgp) = NULL;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    (*((InteractorWidgetClass) interactorWidgetClass)
     ->composite_class.delete_child) (w);
    return;
}

/*
 * FUNCTION
 */
static                          Boolean
#ifdef _NO_PROTO
SetValues(current, request, new)
    PixelWidget                     current;
    PixelWidget                     request;
    PixelWidget                     new;
#else
SetValues(
          PixelWidget current,
          PixelWidget request,
          PixelWidget new)
#endif
{
    Arg                             args[10];
    int                             n;
    String                          newString;

    BB_InSetValues(new) = TRUE;

    if (P_Scale1LabelString(current) != P_Scale1LabelString(new)) {

        P_Scale1LabelString(new) = XmStringCopy(P_Scale1LabelString(new));

        n = 0;
        XtSetArg(args[n], XmNlabelString, P_Scale1LabelString(new));
        n++;
        XtSetArg(args[n], XmNlabelType, XmSTRING);
        n++;
        XtSetValues(P_Scale1Label(new), args, n);
    }
    if (P_Scale2LabelString(current) != P_Scale2LabelString(new)) {

        P_Scale2LabelString(new) = XmStringCopy(P_Scale2LabelString(new));

        n = 0;
        XtSetArg(args[n], XmNlabelString, P_Scale2LabelString(new));
        n++;
        XtSetArg(args[n], XmNlabelType, XmSTRING);
        n++;
        XtSetValues(P_Scale2Label(new), args, n);
    }
    if (P_Scale3LabelString(current) != P_Scale3LabelString(new)) {

        P_Scale3LabelString(new) = XmStringCopy(P_Scale3LabelString(new));

        n = 0;
        XtSetArg(args[n], XmNlabelString, P_Scale3LabelString(new));
        n++;
        XtSetArg(args[n], XmNlabelType, XmSTRING);
        n++;
        XtSetValues(P_Scale3Label(new), args, n);
    }
    if (P_XColor(new) != P_XColor(current)) {
        XColor                         *xcolor;

        XtFree(P_XColor(current));

        xcolor = (XColor *) XtMalloc(sizeof(XColor));
        bcopy((char *) P_XColor(new), (char *) xcolor, sizeof(XColor));
        P_XColor(new) = xcolor;

	if (xcolor != NULL)
        XtVaSetValues(P_DisplayArea(new), XmNbackground, xcolor->pixel, NULL);

        XtFree(P_XColorSaved(current));

        xcolor = (XColor *) XtMalloc(sizeof(XColor));
        bcopy((char *) P_XColor(new), (char *) xcolor, sizeof(XColor));
        P_XColorSaved(new) = xcolor;

        SetScaleValues(new, P_ScaleType(current));
    }
    if (P_ScaleType(new) != P_ScaleType(current)) {

        switch (P_ScaleType(new)) {
        case XgRGB:
        case XgHSV:
        case XgCMY:
            break;
        default:
            _XmWarning(new, "Invalid scale type mask");
            P_ScaleType(new) = XgRGB;
            break;
        }
        SetScaleValues(new,P_ScaleType(current));
    }
    BB_InSetValues(new) = FALSE;

    if (XtClass(new) == pixelWidgetClass) {
        _XmBulletinBoardSizeUpdate((XmBulletinBoardWidget) new);
    }
    return (FALSE);
}

static void
#ifdef _NO_PROTO
SetScaleValues(xgp,oldType)
    PixelWidget                     xgp;
    unsigned char                   oldType;
#else
SetScaleValues(PixelWidget xgp, unsigned char oldType)
#endif
{
    /*
     * We are here if a new color has been sent or the scaleTypeMask has been
     * changed via SetValues, so update labels, scales, and texts.
     */
    RGB                             rgb;
    HSV                             hsv;
    CMY                             cmy;
    XColor                         *xcolor = P_XColor(xgp);
    XmString                        xms;
    char                           text[20];

    if (xcolor != NULL){
      rgb.r = xcolor->red;
      rgb.g = xcolor->green;
      rgb.b = xcolor->blue;
    }
    else {
      rgb.r = 0;
      rgb.g = 0;
      rgb.b = 0;
    }

    switch (P_ScaleType(xgp)) {
    case XgRGB:
        xms = XmStringCreateSimple("Red");
        XtVaSetValues(P_Scale1Label(xgp), XmNlabelString, xms, NULL);
        XmStringFree(xms);
        xms = XmStringCreateSimple("Green");
        XtVaSetValues(P_Scale2Label(xgp), XmNlabelString, xms, NULL);
        XmStringFree(xms);
        xms = XmStringCreateSimple("Blue");
        XtVaSetValues(P_Scale3Label(xgp), XmNlabelString, xms, NULL);
        XmStringFree(xms);
        XtVaSetValues(P_Scale1(xgp),
                      XmNvalue, (int) (rgb.r / (65536. / 256.)), NULL);
        sprintf(text, "%3d", (int) (rgb.r / (65536. / 256.)));
        XtVaSetValues(P_Scale1Text(xgp), XmNvalue, text, NULL);
        XtVaSetValues(P_Scale2(xgp),
                      XmNvalue, (int) (rgb.g / (65536. / 256.)), NULL);
        sprintf(text, "%3d", (int) (rgb.g / (65536. / 256.)));
        XtVaSetValues(P_Scale2Text(xgp), XmNvalue, text, NULL);
        XtVaSetValues(P_Scale3(xgp),
                      XmNvalue, (int) (rgb.b / (65536. / 256.)), NULL);
        sprintf(text, "%3d", (int) (rgb.b / (65536. / 256.)));
        XtVaSetValues(P_Scale3Text(xgp), XmNvalue, text, NULL);
	if ( oldType == XgHSV ) {
            XtVaSetValues(P_Scale1(xgp),
                      XmNmaximum,  255,
		      XmNminimum, 0,
		      XmNdecimalPoints, 0,
		      XmNscaleMultiple, 25,
		      NULL);
            XtVaSetValues(P_Scale2(xgp),
                      XmNmaximum,  255,
		      XmNminimum, 0,
		      XmNdecimalPoints, 0,
		      XmNscaleMultiple, 25,
		      NULL);
            XtVaSetValues(P_Scale3(xgp),
                      XmNmaximum,  255,
		      XmNminimum, 0,
		      XmNdecimalPoints, 0,
		      XmNscaleMultiple, 25,
		      NULL);
	}
        break;
    case XgHSV:
        hsv = RGBToHSV(rgb);
        xms = XmStringCreateSimple("Hue");
        XtVaSetValues(P_Scale1Label(xgp), XmNlabelString, xms, NULL);
        XmStringFree(xms);
        xms = XmStringCreateSimple("Saturation");
        XtVaSetValues(P_Scale2Label(xgp), XmNlabelString, xms, NULL);
        XmStringFree(xms);
        xms = XmStringCreateSimple("Value");
        XtVaSetValues(P_Scale3Label(xgp), XmNlabelString, xms, NULL);
        XmStringFree(xms);
        XtVaSetValues(P_Scale1(xgp), XmNvalue, (int) (hsv.h * 1000.0), NULL);
        sprintf(text, "%0.3f", hsv.h);
        XtVaSetValues(P_Scale1Text(xgp), XmNvalue, text, NULL);
        XtVaSetValues(P_Scale2(xgp), XmNvalue, (int) (hsv.s * 1000.0), NULL);
        sprintf(text, "%0.3f", hsv.s);
        XtVaSetValues(P_Scale2Text(xgp), XmNvalue, text, NULL);
        XtVaSetValues(P_Scale3(xgp), XmNvalue, (int) (hsv.v * 1000.0), NULL);
        sprintf(text, "%0.3f", hsv.v);
        XtVaSetValues(P_Scale3Text(xgp), XmNvalue, text, NULL);
	if ( oldType != XgHSV ) {
            XtVaSetValues(P_Scale1(xgp),
                      XmNmaximum,  1000,
		      XmNminimum, 0,
		      XmNscaleMultiple, 100,
		      XmNdecimalPoints, 3,
		      NULL);
            XtVaSetValues(P_Scale2(xgp),
                      XmNmaximum,  1000,
		      XmNminimum, 0,
		      XmNscaleMultiple, 100,
		      XmNdecimalPoints, 3,
		      NULL);
            XtVaSetValues(P_Scale3(xgp),
                      XmNmaximum,  1000,
		      XmNminimum, 0,
		      XmNscaleMultiple, 100,
		      XmNdecimalPoints, 3,
		      NULL);
	}
        break;
    case XgCMY:
        cmy = RGBToCMY(rgb);
        xms = XmStringCreateSimple("Cyan");
        XtVaSetValues(P_Scale1Label(xgp), XmNlabelString, xms, NULL);
        XmStringFree(xms);
        xms = XmStringCreateSimple("Magenta");
        XtVaSetValues(P_Scale2Label(xgp), XmNlabelString, xms, NULL);
        XmStringFree(xms);
        xms = XmStringCreateSimple("Yellow");
        XtVaSetValues(P_Scale3Label(xgp), XmNlabelString, xms, NULL);
        XmStringFree(xms);
        XtVaSetValues(P_Scale1(xgp),
                      XmNvalue, (int) (cmy.c / (65536. / 256.)), NULL);
        sprintf(text, "%3d", (int) (cmy.c / (65536. / 256.)));
        XtVaSetValues(P_Scale1Text(xgp), XmNvalue, text, NULL);
        XtVaSetValues(P_Scale2(xgp),
                      XmNvalue, (int) (cmy.m / (65536. / 256.)), NULL);
        sprintf(text, "%3d", (int) (cmy.m / (65536. / 256.)));
        XtVaSetValues(P_Scale2Text(xgp), XmNvalue, text, NULL);
        XtVaSetValues(P_Scale3(xgp),
                      XmNvalue, (int) (cmy.y / (65536. / 256.)), NULL);
        sprintf(text, "%3d", (int) (cmy.y / (65536. / 256.)));
        XtVaSetValues(P_Scale3Text(xgp), XmNvalue, text, NULL);
	if ( oldType == XgHSV ) {
            XtVaSetValues(P_Scale1(xgp),
                      XmNmaximum,  255,
		      XmNminimum, 0,
		      XmNscaleMultiple, 25,
		      XmNdecimalPoints, 0,
		      NULL);
            XtVaSetValues(P_Scale2(xgp),
                      XmNmaximum,  255,
		      XmNminimum, 0,
		      XmNscaleMultiple, 25,
		      XmNdecimalPoints, 0,
		      NULL);
            XtVaSetValues(P_Scale3(xgp),
                      XmNmaximum,  255,
		      XmNminimum, 0,
		      XmNscaleMultiple, 25,
		      XmNdecimalPoints, 0,
		      NULL);
	}
        break;
    }
    RemoveHelpCallbacks(xgp);
    AddHelpCallbacks(XgSCALE1, xgp);
    AddHelpCallbacks(XgSCALE2, xgp);
    AddHelpCallbacks(XgSCALE3, xgp);
    PixelAdjustAndAttach(xgp, False);
}

/*
 * FUNCTION
 */
static void
#ifdef _NO_PROTO
XgPixelCallBack(wid, which_button, callback)
    Widget                          wid;
    XtPointer                       which_button;
    XmAnyCallbackStruct            *callback;
#else
XgPixelCallBack(
          Widget wid,
          XtPointer which_button,
          XmAnyCallbackStruct * callback)
#endif
{
    InteractorCallbackStruct        callbackStruct;
    PixelWidget                     xgp;
    Boolean                         allowUnmanage = FALSE;

    xgp = (PixelWidget) XtParent(wid);
    callbackStruct.value = (XmString) which_button;
    callbackStruct.event = callback->event;

    switch ((int) which_button) {
    case XmINTERACT_OK_BUTTON:
    case XmINTERACT_APPLY_BUTTON:
    case XmINTERACT_HELP_BUTTON:
        break;
    case XmINTERACT_CANCEL_BUTTON:
        XStoreColor(XtDisplay(xgp), P_Colormap(xgp),
                    P_XColorSaved(xgp));
	bcopy((char *)P_XColorSaved(xgp),(char *)P_XColor(xgp),sizeof(XColor));
	SetScaleValues(xgp,P_ScaleType(xgp));
        break;
    }

    return;
}

/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
XgPixelGetChild(xgp, which)
    Widget                          xgp;        /* Pixel widget  */
    unsigned char                   which;      /* Which child          */
#else
XgPixelGetChild(
                Widget xgp,     /* Pixel widget  */
                unsigned char which)    /* Which child          */
#endif
{
    Widget                          child = NULL;

    switch (which) {
    case XgPIXEL_DISPLAY_AREA:
        {
            child = P_DisplayArea(xgp);
            break;
        }
    case XgPIXEL_SCALE1_LABEL:
        {
            child = P_Scale1Label(xgp);
            break;
        }
    case XgPIXEL_SCALE1:
        {
            child = P_Scale1(xgp);
            break;
        }
    case XgPIXEL_SCALE1_TEXT:
        {
            child = P_Scale1Text(xgp);
            break;
        }
    case XgPIXEL_SCALE2_LABEL:
        {
            child = P_Scale2Label(xgp);
            break;
        }
    case XgPIXEL_SCALE2:
        {
            child = P_Scale2(xgp);
            break;
        }
    case XgPIXEL_SCALE2_TEXT:
        {
            child = P_Scale2Text(xgp);
            break;
        }
    case XgPIXEL_SCALE3_LABEL:
        {
            child = P_Scale3Label(xgp);
            break;
        }
    case XgPIXEL_SCALE3:
        {
            child = P_Scale3(xgp);
            break;
        }
    case XgPIXEL_SCALE3_TEXT:
        {
            child = P_Scale3Text(xgp);
            break;
        }
    default:
        {
            child = XgInteractorGetChild(xgp, which);
            break;
        }
    }
    return (child);
}

/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
XgCreatePixel(p, name, args, n)
    Widget                          p;  /* parent widget   */
    String                          name;       /* widget name     */
    ArgList                         args;       /* arg list        */
    Cardinal                        n;  /* arg count       */
#else
XgCreatePixel(
              Widget p,         /* parent widget   */
              String name,      /* widget name     */
              ArgList args,     /* arg list        */
              Cardinal n)       /* arg count       */
#endif
{
    return (XtCreateWidget(name, pixelWidgetClass, p, args, n));
}

/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
XgCreatePixelDialog(ds_p, name, xgp_args, xgp_n)
    Widget                          ds_p;       /* parent for shell    */
    String                          name;       /* widget name         */
    ArgList                         xgp_args;   /* arglist for xgp      */
    Cardinal                        xgp_n;      /* argcount for xgp     */
#else
XgCreatePixelDialog(
                    Widget ds_p,/* parent for shell    */
                    String name,/* widget name         */
                    ArgList xgp_args,   /* arglist for xgp      */
                    Cardinal xgp_n)     /* argcount for xgp     */
#endif
{
    Widget                          xgp;        /* new xgp widget      */
    Widget                          ds; /* DialogShell         */
    Arg                             ds_args[10];        /* arglist for shell  */
    char                           *ds_name;

    /*
     * Create DialogShell parent.
     */
    ds_name = XtCalloc(strlen(name) + 1, sizeof(char));
    strcpy(ds_name, name);

    XtSetArg(ds_args[0], XmNallowShellResize, True);
    ds = XmCreateDialogShell(ds_p, ds_name, ds_args, 1);

    XtFree(ds_name);

    /*
     * Create Pixel widget.
     */
    xgp = XtCreateWidget(name, pixelWidgetClass, ds,
                         xgp_args, xgp_n);
    XtAddCallback(xgp, XmNdestroyCallback, _XmDestroyParentCallback, NULL);

    return (xgp);
}
