static char rcsid[] = "@(#)XGRASS $Id: xgmapsets.c,v 0.0.0.1 1992/05/05 14:59:54 kurt Exp kurt $";
/*
 * File: xgmapsets.c
 *
 * Desc: Top level program for changing mapset search path
 *
 * Auth: Eric W. Sink
 *
 * Date: 1 Feb 1992
 *
 * Modification History:
 *
 *
 */
#include "xgrass_lib.h"
#include <Xm/TextF.h>
#include "Interact.h"
#include "Region.h"
#ifndef mips
#include "unistd.h"
#include "stdlib.h"
#include "sys/types.h"
#include "sys/stat.h"
#endif
#include "gis.h"

Display        *display;
Widget          mainshell;
Widget          xgi;
Widget          theForm;
Widget          theLabel;
Widget          buttonsContainer,leftList,rightList,leftLabel,rightLabel;
Widget          leftContainer,rightContainer;
Widget          addButton,removeButton;
Widget          leftFrame,rightFrame;

XtAppContext    appContext;
char **availMapsets;
int availMapsetCount;

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"XGRASS Mapsets"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

static void
#ifdef _NO_PROTO
XgmapsetsOk(w,data)
Widget w;
XtPointer data;
#else
XgmapsetsOk(Widget w, XtPointer data)
#endif
{
  XmStringTable items;
  int itemCount;
  int err = 0;
  int i;
  char *s;
  char command[1024];

  strcpy(command,"g.mapsets -p mapset=");

  XtVaGetValues(rightList,XmNitems,&items,XmNitemCount,&itemCount,NULL);
  for (i=0;i<itemCount;i++) {
    XmStringGetLtoR(items[i],XmSTRING_DEFAULT_CHARSET,&s);
    if (i != 0)
      strcat(command,",");
    strcat(command,s);
  }
  XgSystem(xgi,command,True,&err,1);

  XgSetCommandString(display,XgGetMenuWindow(display),command);
  XFlush(display);
  exit(0);
}

static void
#ifdef _NO_PROTO
XgmapsetsAdd(w,data)
Widget w;
XtPointer data;
#else
XgmapsetsAdd(Widget w,XtPointer data)
#endif
{
  /* Get the selected item from the left list, and add it to the right list */
  XmString theItem;
  XmStringTable theTable;
  XtVaGetValues(leftList,XmNselectedItems,&theTable,NULL);
  theItem = theTable[0];
  if (XmListItemExists(rightList,theItem)) {
    XmListDeleteItem(rightList,theItem);
  }
  XmListAddItemUnselected(rightList,theItem,0);
  XmListDeselectAllItems(leftList);
  XtSetSensitive(addButton,False);
}

static void
#ifdef _NO_PROTO
XgmapsetsRemove(w,data)
Widget w;
XtPointer data;
#else
XgmapsetsRemove(Widget w,XtPointer data)
#endif
{
  int *positions;
  int poscount;

  XmListGetSelectedPos(rightList,&positions,&poscount);
  XmListDeleteItemsPos(rightList,1,positions[0]);
  XtFree(positions);
  XmListDeselectAllItems(rightList);
  XtSetSensitive(removeButton,False);
}


static void
#ifdef _NO_PROTO
XgmapsetsLeftListCallback(w,data,cbs)
Widget w;
XtPointer data;
XtPointer cbs;
#else
XgmapsetsLeftListCallback(Widget w,XtPointer data,XtPointer cbs)
#endif
{
  int hasSelection;
  XtVaGetValues(leftList,XmNselectedItemCount,&hasSelection,NULL);
  if (hasSelection) {
    XtSetSensitive(addButton,True);
  }
  else {
    XtSetSensitive(addButton,False);
  }
}

static void
#ifdef _NO_PROTO
XgmapsetsRightListCallback(w,data,cbs)
Widget w;
XtPointer data;
XtPointer cbs;
#else
XgmapsetsRightListCallback(Widget w,XtPointer data,XtPointer cbs)
#endif
{
  int hasSelection;
  XtVaGetValues(rightList,XmNselectedItemCount,&hasSelection,NULL);
  if (hasSelection) {
    XtSetSensitive(removeButton,True);
  }
  else {
    XtSetSensitive(removeButton,False);
  }
}


static void
#ifdef _NO_PROTO
XgmapsetsCancel(w,data)
Widget w;
XtPointer data;
#else
XgmapsetsCancel(Widget w, XtPointer data)
#endif
{
  XFlush(display);
  exit(1);
}

static int
#ifdef _NO_PROTO
CreateLayout(w)
    Widget          w;
#else
CreateLayout(Widget w)
#endif
{
    Arg             al[16];
    int             ac = 0;
    
    XtSetArg(al[ac], XmNenableWorkAreaStretch, True);
    ac++;
    XtSetArg(al[ac], XmNokLabelString, XmStringCreateSimple("Execute"));
    ac++;
    xgi = XgCreateInteractor(w, "Set Mapset Search Path", al, ac);
    XtManageChild(xgi);

    XtAddCallback(xgi, XmNokCallback, XgmapsetsOk, xgi);
    XtAddCallback(xgi, XmNcancelCallback, XgmapsetsCancel, xgi);

    theForm = XtVaCreateManagedWidget("xgmapsets_form", xmFormWidgetClass, xgi,
                                      NULL);
    theLabel = XtVaCreateManagedWidget("xgmapsets_label", xmLabelWidgetClass,theForm,
				       XmNtraversalOn, False,
				       XmNlabelString,XgCreateXmStringFromFile("xgmapsets_mesg"),
                                       XmNtopAttachment, XmATTACH_FORM,
                                       XmNrightAttachment, XmATTACH_FORM,
                                       XmNleftAttachment, XmATTACH_FORM,
                                       NULL);
    leftFrame = XtVaCreateManagedWidget("xgmapsets_left_frame", xmFrameWidgetClass, theForm,
                                       XmNtopAttachment, XmATTACH_WIDGET,
				       XmNtopWidget,theLabel,
                                       XmNleftAttachment, XmATTACH_FORM,
                                       XmNbottomAttachment, XmATTACH_FORM,
                                       NULL);

    rightFrame = XtVaCreateManagedWidget("xgmapsets_right_frame", xmFrameWidgetClass, theForm,
                                       XmNtopAttachment, XmATTACH_WIDGET,
				       XmNtopWidget,theLabel,
                                       XmNrightAttachment, XmATTACH_FORM,
                                       XmNbottomAttachment, XmATTACH_FORM,
                                       NULL);

    buttonsContainer = XtVaCreateManagedWidget("xgmapsets_buttons_container", xmRowColumnWidgetClass, theForm,
                         XmNleftAttachment,XmATTACH_WIDGET,
			 XmNleftWidget,leftFrame,
			 XmNrightAttachment,XmATTACH_WIDGET,
			 XmNrightWidget,rightFrame,
			 XmNtopAttachment,XmATTACH_WIDGET,
                         XmNtopWidget,theLabel,
                                        NULL);

    addButton = XtVaCreateManagedWidget("xgm_add_button",xmPushButtonWidgetClass, buttonsContainer,
	   XmNlabelString,XmStringCreateSimple("Add"),
           NULL);
    XgAddHelpCallBackFromFile(addButton,"xgm_add_button");
    XtAddCallback(addButton,XmNactivateCallback,XgmapsetsAdd,xgi);

    removeButton = XtVaCreateManagedWidget("xgm_rm_button",xmPushButtonWidgetClass, buttonsContainer,
	   XmNlabelString,XmStringCreateSimple("Remove"),
           NULL);
    XgAddHelpCallBackFromFile(removeButton,"xgm_rm_button");
    XtAddCallback(removeButton,XmNactivateCallback,XgmapsetsRemove,xgi);

    leftContainer = XtVaCreateManagedWidget("xgmapsets_left_container", xmRowColumnWidgetClass, leftFrame,
           XmNorientation,XmVERTICAL,
	   NULL);

    leftLabel = XtVaCreateManagedWidget("xgmapsets_left_label", xmLabelGadgetClass, leftContainer,
	   XmNlabelString,XmStringCreateSimple("Available mapsets"),
	   XmNtraversalOn, False,
           NULL);

    ac = 0;
    XtSetArg(al[ac], XmNselectionPolicy, XmSINGLE_SELECT);
    ac++;
    XtSetArg(al[ac], XmNvisibleItemCount, 8);
    ac++;
    leftList = XmCreateScrolledList(leftContainer, "xgmapsets_left_list", al, ac);

    XtAddCallback(leftList, XmNsingleSelectionCallback, XgmapsetsLeftListCallback, xgi);
    XtManageChild(leftList);
    XgAddHelpCallBackFromFile(leftList,"xgm_left_list");

    rightContainer = XtVaCreateManagedWidget("xgmapsets_right_container", xmRowColumnWidgetClass, rightFrame,
           XmNorientation,XmVERTICAL,
	   NULL);

    rightLabel = XtVaCreateManagedWidget("xgmapsets_right_label", xmLabelGadgetClass, rightContainer,
	   XmNlabelString,XmStringCreateSimple("Your search path"),
	   XmNtraversalOn, False,
           NULL);

    ac = 0;
    XtSetArg(al[ac], XmNselectionPolicy, XmSINGLE_SELECT);
    ac++;
    XtSetArg(al[ac], XmNvisibleItemCount, 8);
    ac++;
    rightList = XmCreateScrolledList(rightContainer, "xgmapsets_right_list", al, ac);

    XtAddCallback(rightList, XmNsingleSelectionCallback, XgmapsetsRightListCallback, xgi);
    XtManageChild(rightList);
    XgAddHelpCallBackFromFile(rightList,"xgm_right_list");

    availMapsets = _XgDirectoryListing(G_location_path(),&availMapsetCount,False,1,True);
    _XgPutStringArrayInList(leftList,availMapsets,availMapsetCount);

    {
      int n;
      char *name;
      for (n = 0; name = G__mapset_name(n); n++)
	{
	  XmListAddItemUnselected(rightList,XmStringCreateSimple(name),0);
	}
      
      }


    XmListDeselectAllItems(leftList);
    XmListDeselectAllItems(rightList);
    XtSetSensitive(addButton,False);
    XtSetSensitive(removeButton,False);
}

int
#ifdef _NO_PROTO
main(argc, argv)
    unsigned int    argc;
    char          **argv;
#else
main(unsigned int argc, char **argv)
#endif
{
    Widget          shell;
    Arg             al[16];
    int             ac;

    /* initialize the toolkit  */
    /* and open the display (and a few other things...)  */
    G_gisinit(argv[0]);
    mainshell = shell = XtAppInitialize(&appContext, "XGrass",
					initTable, XtNumber(initTable),
					&argc, argv, NULL, NULL, 0);

    display = XtDisplay(shell);

    CreateLayout(mainshell);

    XtRealizeWidget(shell);
    XtAppMainLoop(appContext);

    return 0;
}

