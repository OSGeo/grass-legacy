

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/DrawingA.h>
#include <Xm/ScrolledW.h>
#include <Xm/DialogS.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/PushBGP.h>

#include <HelpP.h>

#include <stdio.h>

#define MAXCHAR 300
#define MAX_SECTIONS 5

#define H_GetFontlist(w,type) type ? w->help.hotword_fontlist : w->help.regular_fontlist
#define H_GetFg(w,type) type ? w->help.hotword_fg : w->help.regular_fg

#undef MAX
#define MAX(x, y) ((x) > (y) ? (x) : (y))

/*---------------------------------------------------*/
/* forward declarations                              */
/* */
/* this is a list of all private procedures in this  */
/* module                                            */
/*---------------------------------------------------*/

#ifdef _NO_PROTO
static void                     ClassPartInitialize();
static void                     Initialize();
static Boolean                  SetValues();
static void                     Destroy();
static void                     HelpCreateWidgets();
static void                     _XgHelpLoadFile();
static void                     MakeHotWord();
static void                     GetHotWord();
static int                      DoHotWord();
static void                     CreateLabel();
static void                     UnmanageCallBack();
static void                     HotWordProcess();
static void                     _XgHelpAdjust();

#else                           /* _NO_PROTO */
static void                     ClassPartInitialize(HelpWidgetClass xgh);
static void 
Initialize(
           HelpWidget request,
           HelpWidget new);
static void 
Destroy(
        HelpWidget xgp);
static Boolean 
SetValues(
          HelpWidget current,
          HelpWidget request,
          HelpWidget new);
static void 
HelpCreateWidgets(
                  HelpWidget new,
                  Display * dpy,
                  int screen);
static void _XgHelpLoadFile(HelpWidget w);
static void MakeHotWord(HelpWidget w, char *str, char *buf, int *str_count,
            Dimension * x_offset, Dimension num_line,int type);
static int DoHotWord(Widget parent, Hot * hot);
static void GetHotWord( 
		       int type, 
		       char *str, 
		       int *str_count, 
		       char *buf, 
		       Hot *hot);
static void CreateLabel( 
			HelpWidget w, 
			int type, 
			char *buf, 
			Hot *hot, 
			Dimension *x_offset, 
			int num_lines);
static void UnmanageCallBack(
			     Widget w,
			     XtPointer cld,
			     XtPointer cad);
static void HotWordProcess(
			   Widget w, 
			   Hot * hot, 
			   XmAnyCallbackStruct *calldata);
static void _XgHelpAdjust(HelpWidget w);
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
    {"Enter", (XtActionProc) _XmManagerEnter},
    {"FocusIn", (XtActionProc) _XmManagerFocusIn},
    {"Arm", (XtActionProc) _XmGadgetArm},
    {"Activate", (XtActionProc) _XmGadgetActivate},
    {"XgInteractorHelp", (XtActionProc) _XgInteractorHelpAction},
    {"BulletinBoardCancel", (XtActionProc) _XmBulletinBoardCancel},
};


/*---------------------------------------------------*/
/* widget resources                                  */
/*---------------------------------------------------*/
static XtResource               resources[] =
{
    {XmNhelpFile,
        XmCHelpFile,
        XtRString,
        sizeof(String),
        XtOffset(HelpWidget, help.help_file),
        XtRString,
    NULL},

    {XmNregularFontList,
        XmCRegularFontList,
        XmRFontList,
        sizeof(XmFontList),
        XtOffset(HelpWidget, help.regular_fontlist),
        XtRString,
    (XtPointer) NULL},

    {XmNhotwordFontList,
        XmCHotwordFontList,
        XmRFontList,
        sizeof(XmFontList),
        XtOffset(HelpWidget, help.hotword_fontlist),
        XtRString,
    (XtPointer) NULL},

    {XmNregularForeground,
        XmCRegularForeground,
        XmRPixel,
        sizeof(Pixel),
        XtOffset(HelpWidget, help.regular_fg),
        XtRString,
    (XtPointer) NULL},

    {XmNhotwordForeground,
        XmCHotwordForeground,
        XmRPixel,
        sizeof(Pixel),
        XtOffset(HelpWidget, help.hotword_fg),
        XtRString,
    (XtPointer) NULL},

    {XmNhelpWidth,
        XmCHelpWidth,
        XmRHorizontalDimension,
        sizeof(Dimension),
        XtOffset(HelpWidget, help.width),
        XtRImmediate,
    (caddr_t) 600},

    {XmNhelpHeight,
        XmCHelpHeight,
        XmRVerticalDimension,
        sizeof(Dimension),
        XtOffset(HelpWidget, help.height),
        XtRImmediate,
    (caddr_t) 350},

    {XmNdismissOnly,
        XmCDismissOnly,
        XmRBoolean,
        sizeof(Boolean),
        XtOffset(HelpWidget, help.dismiss_only),
        XtRImmediate,
        FALSE},

    {XmNautoUnmanage,
        XmCAutoUnmanage,
        XmRBoolean,
        sizeof(Boolean),
        XtOffset(HelpWidget, bulletin_board.auto_unmanage),
        XmRImmediate,
    (XtPointer) FALSE},

    {XmNdialogType,
        XmCDialogType,
        XmRDialogType,
        sizeof(unsigned char),
        XtOffset(HelpWidget, interactor.dialog_type),
        XmRImmediate,
    (XtPointer) XmINTERACT_WORK_AREA_TYPE},

    {XmNaccelerators,
        XmCAccelerators, XmRAcceleratorTable, sizeof(XtAccelerators),
        XtOffset(XmBulletinBoardWidget, core.accelerators),
    XmRString, (XtPointer) defaultAccelerators},
};

static XmSyntheticResource      syn_resources[] =
{
    {NULL, 0, 0, NULL, NULL}
};

externaldef(xghelpclassrec)
    HelpClassRec                    helpClassRec =
    {
        /* core class record */
        {
             /* superclass          */ (WidgetClass) & interactorClassRec,
             /* class_name          */ "Help",
             /* widget_size         */ sizeof(HelpRec),
             /* class_initialize    */ NULL,
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
             /* compress_exposur    */ XtExposeCompressMaximal,
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

        /* composite class record   */
        {
             /* geometry manager    */ (XtGeometryHandler) _XtInherit,
             /* set changed proc    */ _XtInherit,
             /* insert_child        */ _XtInherit,
             /* delete_child        */ _XtInherit,
             /* extension           */ NULL,
        },

        /* constraint class record  */
        {
             /* no additional resources  */ NULL,
             /* num additional resources */ 0,
             /* size of constraint rec   */ 0,
             /* constraint_initialize    */ NULL,
             /* constraint_destroy       */ NULL,
             /* constraint_setvalue      */ NULL,
             /* extension                */ NULL,
        },

        /* manager class record     */
        {
             /* translations                 */ XtInheritTranslations,
             /* get_resources                */ syn_resources,
             /* num_syn_resources            */ XtNumber(syn_resources),
             /* constraint_syn_resources     */ NULL,
             /* num_constraint_syn_resources */ 0,
             /* parent_process               */ XmInheritParentProcess,
             /* extension                    */ NULL,
        },

        /* bulletinBoard class record */
        {
             /* always_install_accelerators */ TRUE,
             /* geo_matrix_create           */ (XmGeoCreateProc) _XtInherit,
             /* focus_moved_proc            */ NULL,
             /* extension                   */ NULL,
        },

        /* interactor class record */
        {
             /* extension */ NULL,
        },

        /* pixel class record */
        {
             /* extension */ NULL,
        }
    };

externaldef(xghelpwidgetclass)
    WidgetClass                     helpWidgetClass = (WidgetClass) & helpClassRec;

    static void
#ifdef _NO_PROTO
                                    ClassPartInitialize(xgh)
    HelpWidgetClass                 xgh;
#else
                                    ClassPartInitialize(
                                                        HelpWidgetClass xgh)
#endif
{

    _XmFastSubclassInit(xgh, XmHELP_BIT);

    return;
}



/****************************************************************
 *  Initialize                                                  *
 ****************************************************************/
static void
#ifdef _NO_PROTO
Initialize(request, new)
    HelpWidget                      request;
    HelpWidget                      new;
#else
Initialize(
           HelpWidget request,
           HelpWidget new)
#endif
{
    Display                        *dpy;
    int                             screen;
    XFontStruct                    *fs;
    int                             i;

    dpy = XtDisplay(request->core.self);
    screen = DefaultScreen(dpy);

    /* check geometry */
    if (request->help.width < 100)
        new->help.width = 100;
    if (request->help.height < 100)
        new->help.height = 100;

    new->help.words = NULL;
    new->help.fontheight = 0;
    new->help.num_words = 0;

    /*
     * creage all of the new widgets
     */
    HelpCreateWidgets(new, dpy, screen);

    if ( new->help.dismiss_only ) {
	XmString xms = XmStringCreateSimple("Done");
	Widget ok;

	XtUnmanageChild(XgInteractorGetChild((Widget)new,XmINTERACT_CANCEL_BUTTON));
	XtUnmanageChild(XgInteractorGetChild((Widget)new,XmINTERACT_APPLY_BUTTON));
	XtUnmanageChild(XgInteractorGetChild((Widget)new,XmINTERACT_HELP_BUTTON));
	XtVaSetValues((Widget)new, XmNokLabelString, xms, NULL);
	ok = XgInteractorGetChild((Widget)new,XmINTERACT_OK_BUTTON);
	XtRemoveAllCallbacks(ok, XmNactivateCallback);
	XtAddCallback(ok, XmNactivateCallback, UnmanageCallBack, (XtPointer)new);
	XmStringFree(xms);
    }

    /*
     * see if we need to write something to the drawing area
     */
    _XgHelpLoadFile(new);
    _XgHelpAdjust(new);

    for (i = 0; i < new->help.num_words; i++)
        if (new->help.words[i].y > 1000)
            printf("stop here\n");

    return;
}

static void
#ifdef _NO_PROTO
UnmanageCallBack(w, cld, cad)
Widget w;
XtPointer cld;
XtPointer cad;
#else
UnmanageCallBack( Widget w, XtPointer cld, XtPointer cad)
#endif
{
    Widget xgh = (Widget)cld;

    XtDestroyWidget(xgh);
    return;
}

/****************************************************************/
static void
#ifdef _NO_PROTO
Destroy(w)
    HelpWidget                      w;
#else
Destroy(HelpWidget w)
#endif
{
    /* clear memory allocated before */
    /* if(w->help.help_file)  free(w->help.help_file); */
    if (w->help.words)
        free(w->help.words);
    free(w);

    return;
}


/****************************************************************/
static                          Boolean
#ifdef _NO_PROTO
SetValues(current, request, new)
    HelpWidget                      current;
    HelpWidget                      request;
    HelpWidget                      new;
#else
SetValues(
          HelpWidget current,
          HelpWidget request,
          HelpWidget new)
#endif
/****************
 * This routine detects differences in two versions
 *   of a widget, when a difference is found the
 *   appropriate action is taken.
 ****************/
{
    /* if change the file name */
    if (H_HelpFile(request) != H_HelpFile(current)) {
        sprintf(new->help.help_file, "%s", request->help.help_file);
        _XgHelpLoadFile(new);
        _XgHelpAdjust(new);
    }
    /* change the width of windows */
    if (request->help.width != current->help.width)
        if (request->help.width > 10)
            new->help.width = request->help.width;

    /* change the height of windows */
    if (request->help.height != current->help.height)
        if (request->help.height > 10)
            new->help.height = request->help.height;

    /* change the dismiss mode */
    if (request->help.dismiss_only != current->help.dismiss_only)
            new->help.dismiss_only = request->help.dismiss_only;
}




/*********************************************************************/
static void
#ifdef _NO_PROTO
HelpCreateWidgets(w, dpy, screen)
    HelpWidget                      w;
    Display                        *dpy;
    int                             screen;
#else
HelpCreateWidgets(HelpWidget w, Display * dpy, int screen)
#endif

{
    Arg                             args[10];
    int                             nargs = 0;

    /*
     * create a scrolled window and a drawing area for display help text
     * purpose
     */
    /* H_ScrolledWin(w) =  */
    H_ScrolledWin(w) =
        XtVaCreateManagedWidget("xghelp_scrwin",
                                xmScrolledWindowWidgetClass,
                                w,
                                XmNscrollingPolicy, XmAUTOMATIC,
                                XmNscrollBarDisplayPolicy, XmAS_NEEDED,
                                NULL);
    H_DrawArea(w) =
        XtVaCreateManagedWidget("xghelp_draw_area",
                                xmDrawingAreaWidgetClass,
                                H_ScrolledWin(w),
                                XmNresizePolicy, XmRESIZE_ANY,
                                NULL);
    XmScrolledWindowSetAreas(H_ScrolledWin(w), NULL, NULL,
                             H_DrawArea(w));
}




Widget
#ifdef _NO_PROTO
XgCreateHelpDialog(parent, name, xgh_args, xgh_n)
    Widget                          parent;
    String                          name;
    ArgList                         xgh_args;
    Cardinal                        xgh_n;
#else
XgCreateHelpDialog(Widget parent,
                   String name,
                   ArgList xgh_args,
                   Cardinal xgh_n)
#endif

{
    Widget                          ds; /* DialogShell           */
    Arg                             ds_args[10];        /* arglist for shell
 */
    ArgList                         _xgh_args;  /* arglist for xgh       */
    Widget                          xgh;        /* new xgh widget        */
    char                           *ds_name;

    /*
     * create DialogShell parent
     */
    ds_name = XtCalloc(strlen(name) + 1, sizeof(char));
    strcpy(ds_name, name);

    XtSetArg(ds_args[0], XmNallowShellResize, True);
    ds = XmCreateDialogShell(parent, ds_name, ds_args, 1);

    XtFree(ds_name);

    /*
     * allocate arglist, copy args, add dialog type arg
     */
    _xgh_args = (ArgList) XtMalloc(sizeof(Arg) * (xgh_n + 1));

    bcopy(xgh_args, _xgh_args, sizeof(Arg) * xgh_n);
    XtSetArg(_xgh_args[xgh_n], XmNdialogType, XmINTERACT_DIALOG_TYPE);
    xgh_n++;

    /*
     * create Interactor, free args, return
     */
    xgh = XtCreateWidget(name, helpWidgetClass,
                         ds, _xgh_args, xgh_n);
    XtAddCallback(xgh, XmNdestroyCallback, _XmDestroyParentCallback, NULL);

    XtFree(_xgh_args);
    return (xgh);
}




/**********************************************************************
 *   _XgHelpLoadFile
 *       open input file and load them into new->regular and new->hot
 **********************************************************************/
#define MARGIN 5                /* the right margin for text display */

static void
#ifdef _NO_PROTO
_XgHelpLoadFile(w)
    HelpWidget                      w;
#else
_XgHelpLoadFile(HelpWidget w)
#endif

{
    FILE                           *in;
    char                            str[MAXCHAR], buf[MAXCHAR];
    int                             i, j;
    Dimension                       x_offset;
    char                            name[MAXCHAR];
    char                           *dir;
    extern char                    *getenv();
    int                             num_lines;

    w->help.words = NULL;
    w->help.num_words = 0;

    if ( !strncmp(w->help.help_file,"man",3 ) ) {
	sprintf(name, "%s/%s", G_gisbase(), w->help.help_file);
    } else {
	if ((dir = getenv("XGRASSHELPDIR")) == NULL) {
	    printf("Error: Environment variable \"XGRASSHELPDIR\" not found\n");
	    exit(-1);
	}
	sprintf(name, "%s/%s", dir, w->help.help_file);
    }
    num_lines = 0;

    /* open input file */
    if ((in = fopen(name, "r")) == NULL) {
        printf("Warning: %s not found\n", w->help.help_file);
        return;
    }
    /* get one line at a time */
    bzero(str, MAXCHAR * sizeof(char));
    while ((fgets(str, MAXCHAR, in)) != NULL) {
        str[strlen(str) - 1] = '\0';
        x_offset = MARGIN;

        /* for each char in this string */
        bzero(buf, MAXCHAR * sizeof(char));
        for (i = 0, j = 0; i < strlen(str); i++, j++) {
            /* no ampersent, put it to buf */
            if (str[i] != '@') {
                buf[j] = str[i];

                /*
                 * hit ampercent, create a xmstring for the char before this
                 * ampercent.  also, make a label for this hotword
                 */
            } else {
                MakeHotWord(w, str, buf, &i, &x_offset, num_lines, REGULAR);
                MakeHotWord(w, str, buf, &i, &x_offset, num_lines, HOTWORD);
                j = -1;
            }
        }

        /* take care of the rest of things in this line */
        MakeHotWord(w, str, buf, &i, &x_offset, num_lines, REGULAR);

        /* increase the line counter */
        num_lines++;

        bzero(str, MAXCHAR * sizeof(char));
    }

    fclose(in);
}



/******************************************************************
 * this function create a hot word label
 ******************************************************************/
static void
#ifdef _NO_PROTO
MakeHotWord(w, str, buf, str_count, x_offset, num_lines, type)
    HelpWidget                      w;
    char                            str[], buf[];
    int                            *str_count;
    Dimension                      *x_offset;
    Dimension                       num_lines;
    int                             type;
#else
MakeHotWord(HelpWidget w, char *str, char *buf, int *str_count,
            Dimension * x_offset, Dimension num_lines,int type)
#endif
{
    int                             num;
    Hot                            *hot;
    int                             hotword_category;

    /* preprocess depending on the type */
    if (type == REGULAR) {
        /* return, if nothing is in the buf */
        if (buf[0] == '\0')
            return;

        /* replace the NULL char by a space, probably is redundant */
        if (buf[strlen(buf) - 1] == '\0')
            buf[strlen(buf) - 1] = ' ';
    } else {
        /* need to create a hot struct, if type is HOTWORD */
        hot = (Hot *) malloc(sizeof(Hot));
    }

    num = w->help.num_words;

    /* allocate memory space */
    if (!num)
        w->help.words = (Text *) calloc(1, sizeof(Text));
    else
        w->help.words = (Text *) realloc(w->help.words,
                                         (num + 1) * sizeof(Text));
    bzero(&(w->help.words[num]), sizeof(Text));

    /* create label and/or callback */
    if (type == HOTWORD) {
        (*str_count)++;         /* skip the @ indicator */

        /* skip characters depending the type of hotword */
        if (strncmp(&(str[*str_count]), "ref", 3) == 0) {
            (*str_count) += 5;  /* skip ref ,parenthesis,and double quote */
            hotword_category = REFERENCE;

        } else if (strncmp(&(str[*str_count]), "glossary", 8) == 0) {
            (*str_count) += 10; /* skip glossary ,paren,and double quote */
            hotword_category = GLOSSARY;

        } else if (strncmp(&(str[*str_count]), "man", 3) == 0) {
            (*str_count) += 5;  /* skip man ,paren, and double quote */
            hotword_category = MANUAL;

        } else {
            printf("illegal hot word\n");
            exit(-1);
        }
        GetHotWord(hotword_category, str, str_count, buf, hot);
	hot->dismissOnly = w->help.dismiss_only;
	hot->buf = XtNewString(buf);
	hot->parent = w;
        CreateLabel(w, hotword_category, buf, hot, x_offset, num_lines);
    } else {
        CreateLabel(w, NULL, buf, NULL, x_offset, num_lines);
    }
}

static void
#ifdef _NO_PROTO
GetHotWord(type, str, str_count, buf, hot)
    int                             type;
    char                           *str;
    int                            *str_count;
    char                           *buf;
    Hot                            *hot;
#else
GetHotWord( int type, char *str, int *str_count, char *buf, Hot *hot)
#endif
{
    char                            where[MAXCHAR];
    int                             i = 0;

    bzero(where, MAXCHAR * sizeof(char));

    /*
     * until we hit the next double quote, copy char from str to buf
     */
    while (str[*str_count] != '"') {
        buf[i] = str[*str_count];
        i++;
        (*str_count)++;
    }

    /* skip the double quote and "," */
    (*str_count) += 2;

    switch (type) {
        /* both glossary and reference have the same syntax */
    case GLOSSARY:
    case REFERENCE:
        /* get the where this reference is */
        i = 0;
        while (str[*str_count] != ')') {
            where[i] = str[*str_count];
            i++;
            (*str_count)++;
        }
        break;

    case MANUAL:
        break;
    }

    hot->type = type;

    /* if type is MANUAL, need to prefix "manual/" before where */
    if (type == MANUAL) {
        hot->where = (char *) XtMalloc(strlen(where) + 1);
        hot->where[0] = '\0';
        strcat(hot->where, where);
    } else {
        hot->where = (char *) XtMalloc(strlen(where) + 1);
        hot->where[0] = '\0';
        strcat(hot->where, where);
    }
}




static void
#ifdef _NO_PROTO
CreateLabel(w, type, buf, hot, x_offset, num_lines)
    HelpWidget                      w;
    int                             type;
    char                           *buf;
    Hot                            *hot;
    Dimension                      *x_offset;
    int                             num_lines;
#else
CreateLabel( HelpWidget w, int type, char *buf, Hot *hot, 
    Dimension *x_offset, int num_lines)
#endif
{
    XmString                        xms;
    int                             num;
    Display                        *dpy;
    int                             screen;
    Dimension                       width, height;
    short                           ht;

    dpy = XtDisplay(w);
    screen = DefaultScreen(dpy);

    /* notice that the memory for this label has been allocated */
    num = w->help.num_words;

    /* create a label widget for this hot word */
    xms = XmStringCreateSimple(buf);
    XmStringExtent(H_GetFontlist(w, type), xms, &width, &height);

    w->help.words[num].text =
        XtVaCreateManagedWidget(type ? "hotword" : "regular",
                             type ? xmPushButtonWidgetClass : xmLabelGadgetClass,
                                w->help.draw_area,
                                XmNlabelString, xms,
                                XmNtraversalOn, type ? True: False,
                                XmNfontList, H_GetFontlist(w, type),
                                XmNforeground, H_GetFg(w, type),
                                NULL);

    /* get the width of this label and update x_offset */
    XtVaGetValues(w->help.words[num].text,
                  XmNwidth, &width,
                  XmNheight, &height, 
		  NULL);
    w->help.words[num].x = *x_offset;
    XtVaGetValues(w->help.words[num].text, 
		  XmNhighlightThickness, &ht, NULL);
    (*x_offset) += (width) + ((type)?2*ht:0);
    XmStringFree(xms);

    if (type) {
        XtAddCallback(w->help.words[num].text, XmNactivateCallback,
                          HotWordProcess, hot);
    }

    /* update fontheight of textdata structure */
    w->help.fontheight = MAX(w->help.fontheight, height);

    /* increase the counter for number of hotwords */
    w->help.num_words++;

    /*
     * set the line number so we can draw this label in the right position
     * later
     */
    w->help.words[num].y = num_lines;

    /* set the type to HOTWORD */
    if (type)
        w->help.words[num].type = HOTWORD;
    else
        w->help.words[num].type = REGULAR;

    /* clear buf */
    bzero(buf, MAXCHAR * sizeof(char));
}

char *
#ifdef _NO_PROTO
FindManPage(mandir, entry)
char *mandir;
char *entry;
#else
FindManPage( char *mandir, char *entry)
#endif
{
    char buf[1024];
    int section = 0;

    for ( section = 1; section <= MAX_SECTIONS; section++) {
        sprintf (buf, "%s/%d/%s", mandir, section, entry);
        if ( access(buf,0) == 0 ) {
	    sprintf(buf,"man/%d/%s", section, entry);
            return XtNewString(buf);
        }
    }
    return NULL;

}

/*********************************************************************
 * call itself recursively
 ********************************************************************/
static void
#ifdef _NO_PROTO
HotWordProcess(w, hot, calldata)
    Widget                          w;
    Hot                            *hot;
    XmAnyCallbackStruct            *calldata;
#else
HotWordProcess(Widget w, Hot * hot, XmAnyCallbackStruct * calldata)
#endif
{
    switch (hot->type) {
        case REFERENCE:
        /*printf(" reference hot word is at \"%s\"\n", hot->where);*/
        DoHotWord(w, hot);
        break;

    case GLOSSARY:
        /*printf(" glossary hot word is at \"%s\"\n", hot->where);*/
        DoHotWord(w, hot);
        break;

    case MANUAL:
	{
	    char *mandir;
	    char *entry = XtNewString(hot->buf);

	    XtFree(hot->where);
	    mandir = XtMalloc(strlen(G_gisbase()) + 5);
	    strcpy(mandir, G_gisbase());
	    strcat(mandir, "/man");
	    hot->where = FindManPage(mandir, entry);
	    if ( hot->where == NULL ) {
		char buffer[256];

		sprintf(buffer,"Can't find \"%s\" manual page.", entry);
		XgWarningDialog(w, buffer);
		return;
	    }
	    DoHotWord(w, hot);
	    break;
	}

    default:
        XgWarningDialog(w,"Error: unrecongizable hot word\n");
        break;
    }
}




/******************************************************************
 * this function adjsut the width and height of drawing area and
 * make the y coordinates of label and xmstring right
 ******************************************************************/
static void
#ifdef _NO_PROTO
_XgHelpAdjust(w)
    HelpWidget                      w;
#else
_XgHelpAdjust(HelpWidget w)
#endif
{
    int                             i;
    Dimension                       w1, w2;
    short                           ht;

    /*
     * now, we have all of the regular text and hot words created, it is time
     * to determined exact position for each of them, notice that
     * data->fontheight is the height of each line
     */
    for (i = 0; i < w->help.num_words; i++) {

	if ( w->help.words[i].type == HOTWORD ) {
	    XtVaGetValues(w->help.words[i].text, 
			  XmNhighlightThickness, &ht, NULL);
	    XtVaSetValues(w->help.words[i].text,
			  XmNx, w->help.words[i].x,
			  XmNy, w->help.words[i].y * w->help.fontheight - ht,
			  NULL);
	} else {
	    XtVaSetValues(w->help.words[i].text,
			  XmNx, w->help.words[i].x,
			  XmNy, w->help.words[i].y * w->help.fontheight,
			  NULL);
	}
    }

    XtVaSetValues(w->help.scr_win,
                  XmNwidth, w->help.width,
                  XmNheight, w->help.height,
                  NULL);
}

typedef struct _XtOffsetRec {
    struct _XtOffsetRec *next;
    XrmQuark       name;
    int            offset;
} XtOffsetRec, *_XtOffsetList;

typedef struct _CallbackRec *CallbackList;
typedef struct _CallbackStruct CallbackStruct;

typedef struct _CallbackRec {
    CallbackList  next;
    Widget          widget;
    XtCallbackProc  callback;
    XtPointer       closure;
} CallbackRec;

struct _CallbackStruct {
    XtCallbackList      external_form;
    int                 array_size;
    CallbackList        internal_form;
};

static CallbackStruct **FetchCallbackStruct(widget, name)
    Widget widget;
    String name;
{
    register _XtOffsetList  offsets;
    register XrmQuark       quark;

    quark = XrmStringToQuark(name);
    for (offsets = (_XtOffsetList)widget->core.widget_class->
                                 core_class.callback_private;
         offsets != NULL;
         offsets = offsets->next) {
        if (quark == offsets->name) {
            return (CallbackStruct**)((char *) widget - offsets->offset - 1);
        }
    }
    return NULL;
}

# ifdef USED_ANYWAY
extern XtCallbackList _XtGetCallbackList();

XtCallbackList FetchXtCallbackList(widget, name)
    Widget  widget;
    String  name;
{
    CallbackStruct **cblist = FetchCallbackStruct(widget, name);

    return _XtGetCallbackList((char *)cblist);
}
# endif
static int
#ifdef _NO_PROTO
DoHotWord(parent, hot)
    Widget                          parent;
    Hot                            *hot;
#else
DoHotWord(Widget parent, Hot * hot)
#endif
{
    Widget                          help;
    XmString xms;
    Arg al[10];
    int ac = 0;

    xms = XmStringCreateSimple(hot->buf);
    XtSetArg(al[ac], XmNdialogTitle, xms); ac++;
    XtSetArg(al[ac], XmNhelpFile, hot->where); ac++;
    XtSetArg(al[ac], XmNdismissOnly, hot->dismissOnly); ac++;
    XtSetArg(al[ac], XmNenableWorkAreaStretch, True); ac++;
    help = XgCreateHelpDialog((Widget) hot->parent, "help_widget", al, ac);
    XmStringFree(xms); 
    XtManageChild(help);
}




/*
 * FUNCTION
 */
Widget
#ifdef _NO_PROTO
XgCreateHelp(p, name, args, n)
    Widget                          p;  /* parent widget         */
    String                          name;       /* widget name           */
    ArgList                         args;       /* arg list      */
    Cardinal                        n;  /* arg count     */
#else
XgCreateHelp(
                   Widget p,    /* parent widget         */
                   String name, /* widget name           */
                   ArgList args,/* arg list      */
                   Cardinal n)  /* arg count     */
#endif
{
    return (XtCreateWidget(name, helpWidgetClass, p, args, n));
}


