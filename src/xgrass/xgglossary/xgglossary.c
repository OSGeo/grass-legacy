static char                     rcsid[] = "$Header: /xgrass/src/xgrass/RCS/xgglossary.c,v 0.0.0.1 1992/05/05 14:59:46 kurt Exp kurt $";
/*
 * $Log: xgglossary.c,v $
 * Revision 0.0.0.1  1992/05/05  14:59:46  kurt
 * auto checkin: Tue May  5 09:59:45 CDT 1992
 *
 * Revision 0.0  1992/03/08  17:58:13  kurt
 * auto checkin: Sun Mar  8 11:58:13 CST 1992
 *
 */

#define MAIN

#include "glossary.h"

static Widget                   match_list;

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"XGRASS Glossary"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

main(argc, argv)
    int                             argc;
    char                          **argv;
{

    /* initialize the toolkit and open the display */
    top = XtAppInitialize(&appContext, "XGrass",
			  initTable, XtNumber(initTable),
			  &argc, argv, NULL, NULL, 0);

    dpy = XtDisplay(top);

    /* create glossary window, which is an instance of interactor */
    CreateGlosaryWindows(top);

    /* create the keyword search dialog */
    CreateKeywordSearchDialog(top);

    /* Xt main loop */
    XtRealizeWidget(top);
    XtAppMainLoop(appContext);
}




CreateGlosaryWindows(parent)
    Widget                          parent;
{
    Widget                          interactor;
    Widget                          row_column;
    Widget                          rc;
    Widget                          letters;
    Widget                          button;
    Widget                          box;
    char                            str[4];
    int                             i;
    XmString                        xms, xms1;
    char                            *items[1];
    XmString                        labels[1];
    void                            GlossaryFindItems();
    void                            DoSearchKeywordDialog();
    void                            Quit();
    Widget okButton;
    Widget exitButton;
    Arg al[5];
    int ac = 0;

    /* create a interactor widget as the child of the topleve shell */
    xms = XmStringCreateSimple("Keyword Search");
    xms1 = XmStringCreateSimple("Exit");
    labels[0] = XmStringCreateSimple("How to Use the Glossary");
    items[0] = _XgStrDup("@xgrass/xglos_how_but");
    interactor = XtVaCreateManagedWidget("interactor", interactorWidgetClass,
		    parent, XmNokLabelString, xms,
		    XmNcancelLabelString, xms1, 
		    XmNhelpItems, items,
		    XmNhelpItemLabels, labels,
		    XmNhelpItemCount, 1, NULL);
    XgAddHelpCallBackFromFile(interactor,"xglos_dialog");
    XmStringFree(xms);
    XmStringFree(xms1);
    XmStringFree(labels[0]);

    XtAddCallback(
                  okButton = XgInteractorGetChild(interactor, XmINTERACT_OK_BUTTON),
                  XmNactivateCallback, DoSearchKeywordDialog, NULL);
    XgAddHelpCallBackFromFile(okButton,"xglos_srch_but");
    XgAddHelpCallBackFromFile(
	exitButton = XgInteractorGetChild(interactor, XmINTERACT_CANCEL_BUTTON),
	"xglos_e_button");
    XtAddCallback(
                  XgInteractorGetChild(interactor, XmINTERACT_CANCEL_BUTTON),
                  XmNactivateCallback, Quit, NULL);

    /* create a row column widget as the child of this interactor */
    row_column = XtVaCreateManagedWidget("row_column",
                                         xmRowColumnWidgetClass,
                                         interactor,
                                         XmNorientation, XmHORIZONTAL,
                                         NULL);

    /*
     * now handle the left hand side, which contain the alphabic letters
     */
    box = XtVaCreateManagedWidget("frame", xmFrameWidgetClass,
                                  row_column, NULL);
    rc = XtVaCreateManagedWidget("row_column", xmRowColumnWidgetClass,
                                 box, NULL);

    xms = XmStringCreateSimple("Select:");
    XtVaCreateManagedWidget("label", xmLabelWidgetClass, rc,
			    XmNtraversalOn,False,
                            XmNlabelString, xms,
                            NULL);
    XmStringFree(xms);
    letters = XtVaCreateManagedWidget("row_column",
                                      xmRowColumnWidgetClass,
                                      rc,
                                      XmNadjustLast, False,
                                      XmNpacking, XmPACK_COLUMN,
                                      XmNorientation, XmHORIZONTAL,
                                      XmNnumColumns, 7,
				      XmNisAligned, True,
				      XmNentryAlignment, XmALIGNMENT_CENTER,
                                      NULL);

    /*
     * on the right hand side is a scrolled list to contain the matched
     * glossary terms
     */
    box = XtVaCreateManagedWidget("frame", xmFrameWidgetClass,
                                  row_column, NULL);
    rc = XtVaCreateManagedWidget("row_column", xmRowColumnWidgetClass,
                                 box, NULL);
    xms = XmStringCreateSimple("Matches:");
    XtVaCreateManagedWidget("label", xmLabelWidgetClass, rc,
		     XmNtraversalOn,False,
                     XmNlabelString, xms, NULL);
    XmStringFree(xms);
    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmSTATIC); ac++;
    XtSetArg(al[ac], XmNwidth, 150); ac++;
    XtSetArg(al[ac], XmNlistSizePolicy, XmCONSTANT); ac++;
    match_list = XmCreateScrolledList(rc, "list", al, ac);
    XgAddHelpCallBackFromFile(match_list,"xglos_mtch_lst");
    XtVaSetValues(match_list, XmNvisibleItemCount, 12,
                  XmNwidth, 200, XmNselectionPolicy, XmSINGLE_SELECT, NULL);
    XtManageChild(match_list);

    /* now, create the alphabic letters on the left hand side */
    for (i = 0; i < 26; i++) {
        str[0] = str[2] = ' ';
        str[1] = 65 + i;        /* because the ASCII code of 'A' is 65 */
        str[3] = '\0';
        xms = XmStringCreateSimple(str);
        button = XtVaCreateManagedWidget("letters",
                                         xmPushButtonWidgetClass,
                                         letters,
                                         XmNlabelString, xms,
                                         XmNwidth, 20,
                                         NULL);
        XmStringFree(xms);
	XgAddHelpCallBackFromFile(button,"xglos_gen_let");
        XtAddCallback(button, XmNactivateCallback,
                      GlossaryFindItems, i);
    }
    xms = XmStringCreateSimple("Other");
    button = XtVaCreateManagedWidget("letters",
				     xmPushButtonWidgetClass,
				     letters,
				     XmNlabelString, xms,
				     XmNwidth, 20,
				     NULL);
    XgAddHelpCallBackFromFile(button,"xglos_o_button");
    XmStringFree(xms);
    XtAddCallback(button, XmNactivateCallback,
		  GlossaryFindItems, 27);
}



void
GlossaryFindItems(w, c, calldata)
    Widget                          w;
    int                             c;
    XmAnyCallbackStruct            *calldata;
{
    char                           *dir;
    char                            name[MAXCHAR];
    char                            str[MAXCHAR];
    FILE                           *in;
    int                             num;
    ListItem                       *list_item;
    XmString                       *xmstr;
    extern char                    *getenv();
    void                            DoGlossary();
    int                             curCount = 0;
    

    XtVaGetValues(match_list, XmNitemCount, &curCount, NULL);
    if ( curCount )
	XmListDeleteAllItems(match_list);

    /* initialize things because list_item is a static variable */
    num = 0;
    list_item = NULL;
    xmstr = NULL;

    /* now, go search the match items and update the scrolled list */

    /* first of all, open the glossary index file */
    if ((dir = getenv("XGRASSHELPDIR")) == NULL) {
        printf("Error: Evnironment variable \"XGRASSHELPDIR\" not found\n");
	XFlush(dpy);
        exit(-1);
    }
    sprintf(name, "%s/GLOSSARY_INDEX", dir);

    if ((in = fopen(name, "r")) == NULL) {
        printf("Warning: %s not found\n", name);
        return;
    }
    /*
     * search this glossary index file until the right catagory is found.
     * When found, update the list items
     */
    while ((fgets(str, MAXCHAR, in)) != NULL) {
        /* find the right catagory */
        if (str[0] == '%' && (str[1] == (65 + c) || 
	     (c == 27 && str[1] == '%'))) {
            /* get all items under this catagory */
            while ((fgets(str, MAXCHAR, in)) != NULL) {
                /* done, if str begin with a % */
                if (str[0] == '%')
                    break;

                /* allocate memory for this new entry */
                if (num) {
                    list_item =
                        (ListItem *) realloc(list_item, (num + 1) * sizeof(ListItem));
                    xmstr =
                        (XmString *) realloc(xmstr, (num + 1) * sizeof(XmString));
                } else {
                    list_item = (ListItem *) XtCalloc(1, sizeof(ListItem));
                    xmstr = (XmString *) XtCalloc(1, sizeof(XmString));
                }

                /*
                 * fill in the fields of list_item[num].  We need to get next
                 * line for the location of this glossary text
                 */
                str[strlen(str) - 1] = '\0';
                sprintf(list_item[num].term, "%s", str);
                fgets(list_item[num].where, MAXCHAR, in);
                list_item[num].where[strlen(list_item[num].where) - 1] = '\0';

                /* create the xmstring for the list entries */
                xmstr[num] = XmStringCreateSimple(list_item[num].term);
                num++;
            }
        }
    }
    if ( num ) {
	XmListAddItems(match_list, xmstr, num, 0);
	XtRemoveAllCallbacks(match_list, XmNsingleSelectionCallback);
	XtAddCallback(match_list, XmNsingleSelectionCallback, DoGlossary, 
		      list_item);
    } else {
	XgWarningDialog(top, "No Matches Found.");
    }

    if (dir)
        free(dir);
}



void
DoGlossary(w, items, calldata)
    Widget                          w;
    ListItem                       *items;
    XmAnyCallbackStruct            *calldata;
{
    Widget                          help;
    Arg al[5];
    int ac = 0;
    XmString title;
    XmListCallbackStruct           *call = (XmListCallbackStruct *) calldata;

    title = XmStringCreateSimple(items[call->item_position - 1].term);
    XtSetArg(al[ac], XmNdialogTitle, title); ac++;
    XtSetArg(al[ac], XmNhelpFile, items[call->item_position - 1].where); ac++;
    XtSetArg(al[ac], XmNdismissOnly, True); ac++;
    help = XgCreateHelpDialog(w, "help_widget", al, ac);
    XtManageChild(help);
    XmStringFree(title);
}



void
Quit(w, client, calldata)
    Widget                          w;
    caddr_t                         client;
    XmAnyCallbackStruct            *calldata;
{
    Widget                          tmp;

    exit(0);
}


GetWarningDialog(parent, string)
    Widget                          parent;
    char                           *string;
{
    Widget                          w;
    Widget                          shell;
    void                            QuitOk();
    void                            QuitCancel();

    w = XmCreateWarningDialog(parent, "warning", NULL, 0);
    XtVaSetValues(w,
                  XmNmessageString, XmStringCreateSimple(string),
                  XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
                  NULL);
    XtAddCallback(XmMessageBoxGetChild(w, XmDIALOG_OK_BUTTON),
                  XmNactivateCallback, QuitOk, NULL);
    XtAddCallback(XmMessageBoxGetChild(w, XmDIALOG_CANCEL_BUTTON),
                  XmNactivateCallback, QuitCancel, NULL);

    XtUnmanageChild(XmMessageBoxGetChild(w, XmDIALOG_HELP_BUTTON));
    shell = XtParent(w);
    if (XmIsMotifWMRunning(shell)) {
        unsigned int                    decor_flags;
        decor_flags = MWM_DECOR_BORDER;

        XtVaSetValues(shell,
                      XmNmwmDecorations, decor_flags, NULL);
    }
    XtManageChild(w);
}



void
QuitOk(w, client, calldata)
    Widget                          w;
    caddr_t                         client;
    XmAnyCallbackStruct            *calldata;
{
    XtDestroyWidget(w);
    XtDestroyWidget(top);
    XFlush(dpy);
    exit(0);
}


void
QuitCancel(w, client, calldata)
    Widget                          w;
    caddr_t                         client;
    XmAnyCallbackStruct            *calldata;
{
    return;
}



void
GlossaryDone(w, client, calldata)
    Widget                          w;
    caddr_t                         client;
    XmAnyCallbackStruct            *calldata;
{
    XtDestroyWidget(client);
}
