static char                     rcsid[] = "$Header: /xgrass/src/xgrass/RCS/keyword.c,v 0.0.0.1 1992/05/05 14:59:44 kurt Exp kurt $";
/*
 * $Log: keyword.c,v $
 * Revision 0.0.0.1  1992/05/05  14:59:44  kurt
 * auto checkin: Tue May  5 09:59:44 CDT 1992
 *
 * Revision 0.0  1992/03/08  17:58:12  kurt
 * auto checkin: Sun Mar  8 11:58:12 CST 1992
 * Revision 1.2  92/02/26  09:31:32  jinyuan ***
 * empty log message ***
 * 
 * Revision 1.1  92/02/26  09:19:25  jinyuan Initial revision
 */

#include <stdio.h>
#include <xgrass_lib.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/Frame.h>
#include <Interact.h>

#include "glossary.h"

#define SearchBegin             1
#define SearchContain           2
#define SearchRelated           3

typedef struct _ToggleData {
    char                           *name;
    Bool                            default_set;
    int                             calldata;
}                               ToggleData;

static Widget                   search_dialog;
static Widget                   keyword_input;
static ToggleData               search_toggle_data[] = {
    "Find words beginning with keyword", True, SearchBegin,
    "Find words containing keyword", False, SearchContain,
/*    "Find related words", False, SearchRelated,*/
    NULL, NULL, NULL,
};
static int                      hotword_search_status = SearchBegin;

CreateKeywordSearchDialog(parent)
    Widget                          parent;
{
    Widget                          interactor;
    Widget                          child;
    Widget                          form;
    Widget                          rc;
    Widget                          left, right;
    Widget                          left_frame, right_frame;
    Widget                          frame;
    Widget                          match_list;
    XmString                        xms;
    void                            ExecuteKeywordSearch();
    void                            CancelSearchKeywordDialog();
    Arg al[5];
    int ac = 0;

    /* a popup shell */
    search_dialog = XtCreatePopupShell("dialog",
                                 topLevelShellWidgetClass, parent, NULL, 0);

    /* an interactor as the child of this popup shell */
    xms = XmStringCreateSimple("Keyword Search");
    interactor = XtVaCreateManagedWidget("interactor",
                                         interactorWidgetClass,
                                         search_dialog,
					 XmNdialogTitle, xms,
                                         NULL);
    XgAddHelpCallBackFromFile(interactor,"xglos_key_diag");
    child = XgInteractorGetChild(interactor,XmINTERACT_OK_BUTTON);
    XgAddHelpCallBackFromFile(child,"xlos_exe_but");
    XmStringFree(xms);

    /* a main form */
    form = XtVaCreateManagedWidget("form",
                                  xmFormWidgetClass, interactor, NULL);

    left_frame = XtVaCreateManagedWidget("frame", xmFrameWidgetClass,
                                 form,
				 XmNtopAttachment, XmATTACH_FORM, 
				 XmNleftAttachment, XmATTACH_FORM, 
				 XmNbottomAttachment, XmATTACH_FORM, 
				 NULL);
    left = XtVaCreateManagedWidget("row_column", xmRowColumnWidgetClass,
                                 left_frame,
				 NULL);
    /* first, we need a text widget for the keyword input */
    xms = XmStringCreateSimple("Keyword:");
    XtVaCreateManagedWidget("label", xmLabelWidgetClass, left,
		     XmNtraversalOn,False,
                     XmNlabelString, xms, NULL);
    XmStringFree(xms);
    keyword_input =
        XtVaCreateManagedWidget("text", xmTextWidgetClass, left,
                                NULL);
    XgAddHelpCallBackFromFile(keyword_input,"xglos_key_inp");
    frame = XtVaCreateManagedWidget("frame", xmFrameWidgetClass,
                                    left, XmNshadowThickness, 4, NULL);
    /* some radio boxes within this frame */
    CreateDialogRadioBox(frame, search_toggle_data);
    right_frame = XtVaCreateManagedWidget("frame",
                                    xmFrameWidgetClass, form, 
				    XmNtopAttachment, XmATTACH_FORM,
				    XmNbottomAttachment, XmATTACH_FORM,
				    XmNleftAttachment, XmATTACH_WIDGET,
				    XmNleftWidget, left_frame,
				    XmNrightAttachment, XmATTACH_FORM,
				    NULL);
    right = XtVaCreateManagedWidget("row_column",
                                    xmRowColumnWidgetClass, right_frame, 
				    NULL);
    /* need a label first on the right first */
    xms = XmStringCreateSimple("Matches:");
    XtVaCreateManagedWidget("label", xmLabelWidgetClass, right,
			    XmNtraversalOn,False,
                            XmNlabelString, xms,
                            NULL);
    XmStringFree(xms);

    XtSetArg(al[ac], XmNscrollBarDisplayPolicy, XmSTATIC); ac++;
    XtSetArg(al[ac], XmNwidth, 300); ac++;
    XtSetArg(al[ac], XmNlistSizePolicy, XmCONSTANT); ac++;
    XtSetArg(al[ac], XmNvisibleItemCount, 5); ac++; 
    XtSetArg(al[ac], XmNselectionPolicy, XmSINGLE_SELECT); ac++;
    match_list = XmCreateScrolledList(right, "list", al, ac);
    XgAddHelpCallBackFromFile(match_list,"xglos_mat_list");
    XtManageChild(match_list);

    /* change the label of OK to Execute and assign a callback with it */
    xms = XmStringCreateSimple("Execute");
    XtVaSetValues(XgInteractorGetChild(interactor, XmINTERACT_OK_BUTTON),
                  XmNlabelString,xms,
                  NULL);
    XmStringFree(xms);
    xms = XmStringCreateSimple("Cancel");
    XtVaSetValues(XgInteractorGetChild(interactor, XmINTERACT_CANCEL_BUTTON),
                  XmNlabelString, xms,
                  NULL);
    XmStringFree(xms);
    XtAddCallback(XgInteractorGetChild(interactor, XmINTERACT_OK_BUTTON),
                  XmNactivateCallback, ExecuteKeywordSearch, match_list);
    XtAddCallback(XgInteractorGetChild(interactor, XmINTERACT_CANCEL_BUTTON),
                XmNactivateCallback, CancelSearchKeywordDialog, match_list);
}



void
DoSearchKeywordDialog(w, client, calldata)
    Widget                          w;
    caddr_t                         client;
    XmAnyCallbackStruct            *calldata;
{
    XtPopup(search_dialog, XtGrabNone);
    XRaiseWindow(XtDisplay(search_dialog), XtWindow(search_dialog));
}



void
CancelSearchKeywordDialog(w, client, calldata)
    Widget                          w;
    caddr_t                         client;
    XmAnyCallbackStruct            *calldata;
{
    XtPopdown(search_dialog);
}




CreateDialogRadioBox(parent, data)
    Widget                          parent;
    ToggleData                     *data;
{
    int                             i;
    Widget                          radio;
    XmString                        xms;
    Widget                          w;
    void                            ToggleHotwordSearch();
    int                             n;
    Arg                             args[10];


    /* create a radio box */
    radio = XmCreateRadioBox(parent, "radio", NULL, 0);
    XtVaSetValues(radio, XmNentryClass, xmToggleButtonWidgetClass, NULL);
    XtManageChild(radio);

    /* create toggles of this radio box */
    for (i = 0; data[i].name; i++) {
        n = 0;
        XtSetArg(args[n], XmNlabelType, XmSTRING);
        n++;
        xms = XmStringCreateSimple(data[i].name);
        XtSetArg(args[n], XmNlabelString, xms);
        n++;
        XtSetArg(args[n], XmNfillOnSelect, True);
        n++;
        XtSetArg(args[n], XmNindicatorOn, True);
        n++;
        XtSetArg(args[n], XmNindicatorType, XmONE_OF_MANY);
        n++;
        XtSetArg(args[n], XmNspacing, 0);
        n++;
        XtSetArg(args[n], XmNmarginHeight, 0);
        n++;
        XtSetArg(args[n], XmNmarginWidth, 0);
        n++;
        XtSetArg(args[n], XmNalignment, XmALIGNMENT_BEGINNING);
        n++;
        if (data[i].default_set) {
            XtSetArg(args[n], XmNset, True);
            n++;
        } else {
            XtSetArg(args[n], XmNset, False);
            n++;
        }
        w = XmCreateToggleButton(radio, "toggle", args, n);
        XtManageChild(w);
        XtAddCallback(w, XmNvalueChangedCallback,
                      ToggleHotwordSearch, data[i].calldata);
        XmStringFree(xms);
    }
}



void
ToggleHotwordSearch(w, client, calldata)
    Widget                          w;
    int                             client;
    XmAnyCallbackStruct            *calldata;
{
    hotword_search_status = client;
}



void
ExecuteKeywordSearch(w, list, calldata)
    Widget                          w;
    Widget                          list;
    XmAnyCallbackStruct            *calldata;
{
    void                            DoGlossary();
    FILE                           *in;
    char                           *word;
    char                           *dir;
    int                             num;
    char                            name[MAXCHAR];
    char                            str[MAXCHAR];
    ListItem                       *list_item;
    XmString                       *xmstr;
    extern char                    *getenv();
    int                             curCount = 0;

    XtVaGetValues(list, XmNitemCount, &curCount, NULL);
    if ( curCount )
	XmListDeleteAllItems(list);

    num = 0;
    list_item = NULL;
    xmstr = NULL;

    /* get str from keyword text widget */
    word = XmTextGetString(keyword_input);
    if (word == NULL)
        return;

    /* search begin */
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
    while ((fgets(str, MAXCHAR, in)) != NULL) {
        /* skip the catagory line */
        if (str[0] != '%') {
            /* get the glossary items and compare with the word  */
            /* fgets(str, MAXCHAR, in); */
            if (MatchSearchPattern(str, word)) {
                /* match the pattern, generate a list entry */
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
            } else {
                /* if not match, we still need to read one more line */
                fgets(str, MAXCHAR, in);
            }
        }
    }

    /* search finished, now need to update the match list */
    if ( num ) {
	XmListAddItems(list, xmstr, num, 0);
	XtRemoveAllCallbacks(list, XmNsingleSelectionCallback);
	XtAddCallback(list, XmNsingleSelectionCallback, DoGlossary,
		      list_item);
    } else {
	XgWarningDialog(search_dialog, "No Matches Found.");
    }
    if ( word ) free(word);
    if ( dir ) free(dir);
}



MatchSearchPattern(str, word)
    char                           *str, *word;
{
    int                             i;

    if (hotword_search_status == SearchBegin) {
        if (strncmp(str, word, strlen(word)) == 0)
            return (True);
    } else if (hotword_search_status == SearchContain) {
        for (i = 0; i < strlen(str); i++)
            if (strncmp(&(str[i]), word, strlen(word)) == 0)
                return (True);
    }
    return (False);
}
