#include <stdio.h>
#include <string.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/DialogS.h>
#include <Xm/DrawingA.h>
#include <Xm/DrawnB.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/ArrowB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <Xm/SelectioB.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>


static int CurrentLinkSetSize = 0;
int get_link_set(); 
int *CurrentLinkSet = NULL;
void highlite_links();

Widget          *AttTexts;
Widget          *AttButtons;
Widget          *AttLabels;
Widget          *AttContainers;
static Widget acceptB, follow_linkB, helpB, doneB, cancelB;
static Widget   theDialog;
static Widget   ButtonPanel;
Widget          CatLabel;
XtAppContext    appContext;
char          **values;
static int     nalloc=0;
void Create_Att_Table_Children(), Destroy_Att_Table_Children(), 
     reallocate_cols(), Create_Edit_Buttons();
void            CallCatLabel(), CallPage(); 
void            CallOk(), CallCancel(), CallHelp(), CallReset(), CallAccept();
void           GetAttText(), show_links();
static Widget         AttTable;
static int            AttTableNumCols;
static char           AttTableName[500];
/* for future use
static ReferenceEdge highlighted_edge;
Widget *AttTableTexts;
*/

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"d.vect.db Table Editor"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

Widget 
Create_Edit_Dialog(parent, name)
    Widget          parent;
    char *name;
{
    Widget          attLB;
    Widget          CatLabelDescr ;
    Widget ScrolledWindow;
    Widget ScrollBar;
    Arg             al[30];
    int             ac, att_height;

    theDialog = XmCreateFormDialog(parent, name, NULL, 0);
    XmRemoveTabGroup(theDialog);

    ac = 0;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    strcpy(AttTableName, name);
    XtSetArg(al[ac], XmNlabelString,
             XmStringCreate(AttTableName, XmSTRING_DEFAULT_CHARSET));ac++;
    XtSetArg(al[ac], XmNx, 20);ac++;
    attLB = XmCreateLabel(theDialog, "attLB", al, ac);
    XtManageChild(attLB);
    
    AttTableNumCols = 36; /* TEMP */
    if(AttTableNumCols > nalloc) reallocate_cols();

    ac = 0;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
    ButtonPanel = XmCreateRowColumn(theDialog, "ButtonPanel", al, ac);
    Create_Edit_Buttons(ButtonPanel);
    XtManageChild(ButtonPanel);

    ac = 0;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
    XtSetArg(al[ac], XmNtopWidget, attLB); ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
    XtSetArg(al[ac], XmNbottomWidget, ButtonPanel); ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNscrollingPolicy, XmAUTOMATIC); ac++;
    XtSetArg(al[ac], XmNvisualPolicy, XmCONSTANT); ac++;
    ScrolledWindow = XmCreateScrolledWindow(theDialog, "Table", al, ac);

    ac = 0;
    XtSetArg(al[ac], XmNorientation, XmVERTICAL); ac++;
    AttTable = XmCreateRowColumn(ScrolledWindow, "AttTable", al, ac); 

    Create_Att_Table_Children();

    ac = 0;
    XtSetArg(al[ac], XmNheight, &att_height);ac++;
    XtGetValues(AttTexts[0], al, ac);
    ac = 0;
    XtSetArg(al[ac], XmNheight, att_height * 15);ac++;
    XtSetArg(al[ac], XmNworkWindow, AttTable);ac++;
    XtSetValues(ScrolledWindow, al, ac);
    XmAddTabGroup(ScrolledWindow);
    XtManageChild(AttTable);
    XtManageChild(ScrolledWindow);

    return (theDialog);
}

void Destroy_Att_Table_Children()
{
    int i;

    for (i = 0; i < AttTableNumCols; i++)
    {
	XtUnmanageChild(AttLabels[i]);

	XtUnmanageChild(AttTexts[i]);

        /* CREATE LINK BUTTON IF EXISTS */
	if(link_exists(i))
	   XtUnmanageChild(AttButtons[i]);
     }
}

void
Create_Att_Table_Children()
{
    char            name[120];
    Arg             al[30];
    int             ac, i;

    for (i = 0; i < AttTableNumCols; i++)
    {
	if(AttContainers[i]) 
	{
	   XtManageChild(AttContainers[i]);
	   continue;
        }
        sprintf(name, "container%d", i + 1);
	ac=0;
	/*
        XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
        XtSetArg(al[ac], XmNpacking, XmPACK_COLUMN); ac++;
	AttContainers[i] = XmCreateRowColumn(AttTable, name, al, ac);
	*/
	AttContainers[i] = XmCreateForm(AttTable, name, al, ac);
	if(link_exists(i))
           AttButtons[i] = XmCreatePushButton(AttContainers[i], name, NULL, 0);

	/* CREATE ATTR DESCRIPTION */
        sprintf(name, "lab%d", i + 1);
        ac = 0;
	if(i==5)
	   sprintf(name,"olechka-zain'ka");
        XtSetArg(al[ac], XmNlabelString,
             XmStringCreate(name, XmSTRING_DEFAULT_CHARSET));ac++;
        XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
        AttLabels[i] = XmCreateLabel(AttContainers[i], name, al, ac);
	XtManageChild(AttLabels[i]);

	/* CREATE ATTR TEXT EDIT FIELD */
        ac = 0;
        sprintf(name, "%3d", i + 1);
        XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
        XtSetArg(al[ac], XmNleftWidget, AttLabels[i]); ac++;
	if(link_exists(i))
	{
	    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_WIDGET); ac++;
	    XtSetArg(al[ac], XmNrightWidget, AttButtons[i]); ac++;
        }
	else
	{
	    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	}
	AttTexts[i] = XtCreateWidget(name, xmTextWidgetClass, AttContainers[i], al, ac);
        if(!values[i]) values[i] = XtMalloc(20);
	/******** WIDTH */
        sprintf(values[i], "0");
	XmTextSetString(AttTexts[i], values[i]);
        XtAddCallback(AttTexts[i], XmNlosingFocusCallback, GetAttText, i);
        XtAddCallback(AttTexts[i], XmNactivateCallback, GetAttText, i);
        XmAddTabGroup(AttTexts[i]);
	XtManageChild(AttTexts[i]);

        /* CREATE LINK BUTTON IF EXISTS */
	if(link_exists(i))
	{
           sprintf(name, "link%d", i + 1);
	   ac = 0;
           XtSetArg(al[ac], XmNlabelString,
             XmStringCreate("Show Links", XmSTRING_DEFAULT_CHARSET)); ac++;
           XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
           XtSetValues(AttButtons[i], al, ac);
           XtAddCallback(AttButtons[i], XmNactivateCallback, show_links, i);
	   XmAddTabGroup(AttButtons[i]);
	   XtManageChild(AttButtons[i]);
        }
        XtManageChild(AttContainers[i]);
    }
}

void 
show_links(w, client_data, data)
    Widget          w;
    int             client_data;
    caddr_t         data;
{
    int i = client_data;
    /* first unhighlight former highlight list */
    if(CurrentLinkSet)
        highlite_links(CurrentLinkSet, CurrentLinkSetSize, 0);

    CurrentLinkSetSize = get_link_set(&CurrentLinkSet, i);
    /* now highlight new link set */
    highlite_links(CurrentLinkSet, CurrentLinkSetSize, 1);

    printf("follow link: %d\n", i);
}

void 
highlite_links(link_set, num, highlight)
    int *link_set, num, highlight;
{
    int i;
    Arg al[10];
    Pixel background, foreground;
    int ac;

    ac = 0;

    if(!num) return;
    XtSetArg(al[ac], XmNforeground, &background); ac++;
    XtSetArg(al[ac], XmNbackground, &foreground); ac++;
    XtGetValues(AttButtons[link_set[0]], al, ac);

    XtSetArg(al[ac], XmNforeground, foreground); ac++;
    XtSetArg(al[ac], XmNbackground, background); ac++;
    if(!highlight)
        XtSetArg(al[ac], XmNcursorPositionVisible, TRUE);
    else
        XtSetArg(al[ac], XmNcursorPositionVisible, FALSE);
    ac++;
    for(i=0; i<num; i++)
    {
       XtSetValues(AttButtons[link_set[i]], al, ac);
       XtSetValues(AttTexts[link_set[i]], al, ac);
       XtSetValues(AttLabels[link_set[i]], al, ac);
       XtSetValues(AttContainers[link_set[i]], al, ac);
    }
}

void 
GetAttText(w, client_data, call_data)
    Widget          w;
    int             client_data;
    caddr_t         call_data;
{
    int i = client_data;
    sprintf(values[i], "%s", XmTextGetString(w));
    printf("%d att: %s\n", i, values[i]);
}

void 
CallCatLabel(w, client_data, call_data)
    Widget          w;
    caddr_t         client_data;
    caddr_t         call_data;
{
    char            temp[256];

    sprintf(temp, "%s", XmTextGetString(w));
    printf("cat label: %s\n", temp);
}

void 
CallOk(w, parent, call_data)
    Widget          w, parent;
    caddr_t         call_data;
{
    int             i, err = 0;
    char            command[512];
    for (i = 0; i < AttTableNumCols; i++)
    {
            sprintf(values[i], "%s", XmTextGetString(AttTexts[i]));
	    printf("col %d: %s\n", i, values[i]);
    }
    /*printf("att label: %s\n", XmTextGetString(CatLabel));*/
    XtUnmanageChild(parent);
}

void 
CallCancel(w, parent, call_data)
    Widget          w, parent;
    caddr_t         call_data;
{
    XtUnmanageChild(parent);
}

void 
CallHelp(w, client_data, call_data)
    Widget          w;
    caddr_t         client_data, call_data;
{
    printf("Help is not yet available!\n");
}

void 
CallAccept(w, tb, call_data)
    Widget          w, tb;
    caddr_t         call_data;
{
    int             i, count = 0;
    char           *dists, *tok;
    char            temp[660], buf[11];

    for (i = 0; i < AttTableNumCols; i++)
    {
            sprintf(values[i], "%s", XmTextGetString(AttTexts[i]));
	    printf("col %d: %s\n", i, values[i]);
    }
}

void 
CallReset(w, tb, call_data)
    Widget          w, tb;
    caddr_t         call_data;
{
    int             i;
    Time time;

    for (i = 0; i < AttTableNumCols; i++)
    {
        sprintf(values[i], "");
	XmTextSetString(AttTexts[i], values[i]);
    }
}

static int link_exists(i)
   int i;
{
   return (i==2 || i==4 || i==5 || i==8 || i==25 || i==30 || i==34);
}

int set1[] = {2,4,5};
int num1 = 3;
int set2[] = {5,8,25, 30};
int num2 = 4;
int set3[] = {25,30,34};
int num3 = 3;

int get_link_set(set, i)
    int **set, i;
{
    if (i==2 || i==4 || i==5)
    {
	*set = set1;
	return num1;
    }
    if(i==5 || i==8 || i==25)
    {
	*set = set2;
	return num2;
    }
    if(i==25 || i==30 || i==34)
    {
	*set = set3;
	return num3;
    }
}

void reallocate_cols()
{
    int i;

    if(!nalloc)
    {
        AttTexts = (Widget *) G_malloc(AttTableNumCols * sizeof(Widget));
        AttLabels = (Widget *) G_malloc(AttTableNumCols * sizeof(Widget));
        AttButtons = (Widget *) G_malloc(AttTableNumCols * sizeof(Widget));
        AttContainers = (Widget *) G_malloc(AttTableNumCols * sizeof(Widget));
        values = (char **) G_malloc(AttTableNumCols * sizeof(char *));
    }
    else
    {
        AttTexts = (Widget *) G_realloc((char *) AttTexts,
					AttTableNumCols * sizeof(Widget));
        AttLabels = (Widget *) G_realloc((char *) AttLabels,
			 	        AttTableNumCols * sizeof(Widget));
        AttButtons = (Widget *) G_realloc((char *) AttButtons,
					AttTableNumCols * sizeof(Widget));
        AttContainers = (Widget *) G_realloc((char *) AttContainers,
					AttTableNumCols * sizeof(Widget));
        values = (char **) G_realloc((char *) values, 
					AttTableNumCols * sizeof(char *));
    }

    for(i=0; i<AttTableNumCols; i++)
    {
       AttTexts[i] = NULL;
       AttLabels[i] = NULL;
       AttButtons[i] = NULL;
       AttContainers[i] = NULL;
       values[i] = NULL;
    }
}

void
Create_Edit_Buttons(parent)
    Widget parent; 
{
    Widget acceptB, follow_linkB, helpB, doneB, cancelB;
    Arg al[10];
    int ac;

    acceptB = XmCreatePushButton(parent, "Accept", NULL, 0);
    XtAddCallback(acceptB, XmNactivateCallback, CallAccept, AttTable);
    XmAddTabGroup(acceptB);
    XtManageChild(acceptB);

    doneB = XmCreatePushButton(parent, "Done", NULL, 0);
    XtAddCallback(doneB, XmNactivateCallback, CallOk, theDialog);
    XmAddTabGroup(doneB);
    XtManageChild(doneB);

    cancelB = XmCreatePushButton(parent, "Cancel", NULL, 0);
    XtAddCallback(cancelB, XmNactivateCallback, CallCancel, theDialog);
    XmAddTabGroup(cancelB);
    XtManageChild(cancelB);

    helpB = XmCreatePushButton(parent, "Help", NULL, 0);
    XtAddCallback(helpB, XmNactivateCallback, CallHelp, theDialog);
    XmAddTabGroup(helpB);
    XtManageChild(helpB);

    follow_linkB = XmCreatePushButton(parent, "Follow Link to Another Table", NULL, 0);
    /*
    XtAddCallback(follow_linkB, XmNactivateCallback, CallHelp, theDialog);
    */
    XmAddTabGroup(follow_linkB);
    XtManageChild(follow_linkB);

}
