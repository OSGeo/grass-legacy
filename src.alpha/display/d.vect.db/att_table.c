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
#include "gis.h"
#include "global.h"

static int CurrentLinkSetSize = 0;
void get_link_set(); 
int *CurrentLinkSet = NULL;
void highlite_links();

void Create_Att_Table_Children(), Destroy_Att_Table_Children(), 
     reallocate_cols(), Create_Edit_Buttons();
void            CallCatLabel(), CallFollow_link(); 
void            CallOk(), CallCancel(), CallHelp(), CallReset(), CallAccept();
void           GetAttText(), show_links();

static XrmOptionDescRec initTable[] = {
{"-title",	"*title",	XrmoptionSepArg, (caddr_t)"d.vect.db Table Editor"},
{"-font",	"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
{"-fn",		"*fontList",	XrmoptionSepArg, (caddr_t)"fixed"},
};

void
Create_Edit_Dialog(parent, Table)
    Widget          parent;
    struct Widget_Info *Table;
{
    Widget          attLB, att_list;
    Widget          CatLabelDescr ;
    Widget   ButtonPanel;
    Widget ScrolledWindow;
    char   buf[100];
    Arg             al[30];
    int             ac, att_height;

    /* first of all if the table already exists but not managed, just manage it */
    if(Table->AttTable)
    {
	   XtManageChild(Table->AttTable);
	   return;
    }
    sprintf(buf, "table %s index %s", Table->AttTableName, Table->AttKeys[0]);
    /* TEMP later allow multiple column indexes */
    Table->AttTable = XmCreateFormDialog(parent, buf, NULL, 0);
    XmRemoveTabGroup(Table->AttTable);

    ac = 0;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNlabelString,
             XmStringCreate(buf, XmSTRING_DEFAULT_CHARSET));ac++;
    XtSetArg(al[ac], XmNx, 20);ac++;
    attLB = XmCreateLabel(Table->AttTable, "attLB", al, ac);
    XtManageChild(attLB);
    
    if(Table->AttTableNumCols > Table->nalloc) reallocate_cols(Table);

    ac = 0;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
    ButtonPanel = XmCreateRowColumn(Table->AttTable, "ButtonPanel", al, ac);
    Create_Edit_Buttons(ButtonPanel, Table);
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
    ScrolledWindow = XmCreateScrolledWindow(Table->AttTable, "Table", al, ac);

    ac = 0;
    XtSetArg(al[ac], XmNorientation, XmVERTICAL); ac++;
    att_list = XmCreateRowColumn(ScrolledWindow, "AttTable", al, ac); 

    Create_Att_Table_Children(att_list, Table);

    ac = 0;
    XtSetArg(al[ac], XmNheight, &att_height);ac++;
    XtGetValues(Table->Cols[0].Text, al, ac);
    ac = 0;
    /*XtSetArg(al[ac], XmNheight, att_height * 5);ac++;*/
    XtSetArg(al[ac], XmNworkWindow, att_list);ac++;
    XtSetValues(ScrolledWindow, al, ac);
    XmAddTabGroup(ScrolledWindow);
    XtManageChild(att_list);
    XtManageChild(ScrolledWindow);

    Table->CurrentLinkSet = NULL;
    Table->CurrentLinkSetSize = 0;
    Table->ChosenRefNumber = -1;
}

void
Create_Att_Table_Children(parent, Table)
    Widget parent;
    struct Widget_Info *Table;
{
    char            name[120];
    Arg             al[30];
    int             ac, i;

    for (i = 0; i < Table->AttTableNumCols; i++)
    {
	Table->Cols[i].col_num = i;
	Table->Cols[i].parent_table = Table;
	if(Table->Cols[i].Container) 
	{
	   XtManageChild(Table->Cols[i].Container);
	   continue;
        }
        sprintf(name, "container%d", i + 1);
	ac=0;
	/*
        XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
        XtSetArg(al[ac], XmNpacking, XmPACK_COLUMN); ac++;
	Cols[i].Container = XmCreateRowColumn(parent, name, al, ac);
	*/
	Table->Cols[i].Container = XmCreateForm(parent, name, al, ac);
	Table->Cols[i].parent_table = Table;
	if(link_exists(i))
           Table->Cols[i].Button = XmCreatePushButton(Table->Cols[i].Container, name, NULL, 0);

	/* CREATE ATTR DESCRIPTION */
        sprintf(name, "lab%d", i + 1);
        ac = 0;
	if(i==5)
	   sprintf(name,"olechka-zain'ka");
        XtSetArg(al[ac], XmNlabelString,
             XmStringCreate(name, XmSTRING_DEFAULT_CHARSET));ac++;
        XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
        Table->Cols[i].Label = XmCreateLabel(Table->Cols[i].Container, name, al, ac);
	XtManageChild(Table->Cols[i].Label);

	/* CREATE ATTR TEXT EDIT FIELD */
        ac = 0;
        sprintf(name, "%3d", i + 1);
        XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
        XtSetArg(al[ac], XmNleftWidget, Table->Cols[i].Label); ac++;
	if(link_exists(i))
	{
	    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_WIDGET); ac++;
	    XtSetArg(al[ac], XmNrightWidget, Table->Cols[i].Button); ac++;
        }
	else
	{
	    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	}
        XtSetArg(al[ac], XmNcursorPositionVisible, FALSE);ac++;
	Table->Cols[i].Text = XtCreateWidget(name, xmTextWidgetClass, Table->Cols[i].Container, al, ac);
        if(!Table->AttValues[i]) 
	{
	   Table->AttValues[i] = XtMalloc(20);
	/******** WIDTH */
           sprintf(Table->AttValues[i], "0");
        }
	XmTextSetString(Table->Cols[i].Text, Table->AttValues[i]);
        XtAddCallback(Table->Cols[i].Text, XmNlosingFocusCallback, GetAttText, &(Table->Cols[i]));
        XtAddCallback(Table->Cols[i].Text, XmNactivateCallback, GetAttText, &(Table->Cols[i]));
        XmAddTabGroup(Table->Cols[i].Text);
	XtManageChild(Table->Cols[i].Text);

        /* CREATE LINK BUTTON IF EXISTS */
	if(link_exists(i))
	{
           sprintf(name, "link%d", i + 1);
	   ac = 0;
           XtSetArg(al[ac], XmNlabelString,
             XmStringCreate("Show Links", XmSTRING_DEFAULT_CHARSET)); ac++;
           XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
           XtSetValues(Table->Cols[i].Button, al, ac);
           XtAddCallback(Table->Cols[i].Button, XmNactivateCallback, show_links, &(Table->Cols[i]));
	   XmAddTabGroup(Table->Cols[i].Button);
	   XtManageChild(Table->Cols[i].Button);
        }
        XtManageChild(Table->Cols[i].Container);
    }
}

void 
show_links(w, Col, data)
    Widget          w;
    struct Column_Info *Col;
    caddr_t         data;
{
    choose_ref_for_col(Col);
}

void 
highlite_links(Table, highlight)
    struct Widget_Info *Table;
    int highlight;
{
    int i;
    Arg al[10];
    Pixel background, foreground;
    int ac;
    int *link_set = Table->CurrentLinkSet;

    ac = 0;

    if(!Table->CurrentLinkSetSize) return;
    XtSetArg(al[ac], XmNforeground, &background); ac++;
    XtSetArg(al[ac], XmNbackground, &foreground); ac++;
    XtGetValues(Table->Cols[link_set[0]].Button, al, ac);

    XtSetArg(al[ac], XmNforeground, foreground); ac++;
    XtSetArg(al[ac], XmNbackground, background); ac++;
    /*
    if(!highlight)
        XtSetArg(al[ac], XmNcursorPositionVisible, TRUE);
    else
        XtSetArg(al[ac], XmNcursorPositionVisible, FALSE);
    ac++;
    */
    for(i=0; i<Table->CurrentLinkSetSize; i++)
    {
       XtSetValues(Table->Cols[link_set[i]].Text, al, ac);
       XtSetValues(Table->Cols[link_set[i]].Container, al, ac);
       XtSetValues(Table->Cols[link_set[i]].Label, al, ac);
       XtSetValues(Table->Cols[link_set[i]].Button, al, ac);
    }
}

void 
GetAttText(w, Col, call_data)
    Widget          w;
    struct Column_Info *Col;
    caddr_t         call_data;
{
    int i = Col->col_num;
    struct Widget_Info *Table = (struct Widget_Info *) Col->parent_table;
    sprintf(Table->AttValues[i], "%s", XmTextGetString(w));
    printf("%d att: %s\n", i, Table->AttValues[i]);
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
CallOk(w, Table, call_data)
    Widget          w;
    struct Widget_Info *Table;
    caddr_t         call_data;
{
    int             i, err = 0;
    char            command[512];
    for (i = 0; i < Table->AttTableNumCols; i++)
    {
            sprintf(Table->AttValues[i], "%s", XmTextGetString(Table->Cols[i].Text));
	    printf("col %d: %s\n", i, Table->AttValues[i]);
    }
    /*printf("att label: %s\n", XmTextGetString(CatLabel));*/
    XtUnmanageChild(Table->AttTable);
}

void 
CallCancel(w, Table, call_data)
    Widget          w;
    struct Widget_Info *Table;
    caddr_t         call_data;
{
    XtUnmanageChild(Table->AttTable);
}

void 
CallHelp(w, Table, call_data)
    Widget          w;
    struct Widget_Info *Table;
    caddr_t         call_data;
{
    printf("Help is not yet available!\n");
}

void 
CallFollow_link(w, Table, call_data)
    Widget          w;
    struct Widget_Info *Table;
    caddr_t         call_data;
{
  if(Table->ChosenRefNumber<0)
     choose_ref_for_table(Table);
     /* choose_ref_for_table() will call Create_Edit_Dialog() */
  else
  {
     if(C.AttTableManaged)
     {
	 printf("Already managed\n");
	 XtUnmanageChild(C.AttTable);
	 XtManageChild(C.AttTable);
     }
     else
     {
         C.AttTableNumCols = 10;
         C.AttValues = (char **) G_malloc(10 * sizeof(char *));
         C.AttValues[0] = "1";
         C.AttValues[1] = "2";
         C.AttValues[2] = "3";
         C.AttValues[3] = "4";
         C.AttValues[4] = "5";
         C.AttValues[5] = "1";
         C.AttValues[6] = "2";
         C.AttValues[7] = "3";
         C.AttValues[8] = "4";
         C.AttValues[9] = "5";
         C.AttKeys = (char **) G_malloc(1 * sizeof(char *));
         C.AttKeys[0] = "4";
         C.nalloc = 0;
         C.AttTableName = G_store("Table C");
         Create_Edit_Dialog(w, &C);
         XtManageChild(C.AttTable);
         C.AttTableManaged = 1;
     }
  }
}

void 
CallAccept(w, Table, call_data)
    Widget          w;
    struct Widget_Info *Table;
    caddr_t         call_data;
{
    int             i, count = 0;
    char           *dists, *tok;
    char            temp[660], buf[11];

    for (i = 0; i < Table->AttTableNumCols; i++)
    {
            sprintf(Table->AttValues[i], "%s", XmTextGetString(Table->Cols[i].Text));
	    printf("col %d: %s\n", i, Table->AttValues[i]);
    }
}

void 
CallReset(w, Table, call_data)
    Widget          w;
    struct Widget_Info *Table;
    caddr_t         call_data;
{
    int             i;
    Time time;

    for (i = 0; i < Table->AttTableNumCols; i++)
    {
        sprintf(Table->AttValues[i], "");
	XmTextSetString(Table->Cols[i].Text, Table->AttValues[i]);
    }
}

static int link_exists(i, Table)
   struct Widget_Info *Table;
   int i;
{
   return (i==2 || i==4 || i==1 || i==8 || i==6 || i==7);
}

int set1[] = {2,4};
int num1 = 2;
int set2[] = {1};
int num2 = 1;
int set3[] = {6,7,8};
int num3 = 3;

void
get_link_set(i, Table)
   struct Widget_Info *Table;
    int i;
{
    if (i==2 || i==4 )
    {
	Table->CurrentLinkSet = set1;
	Table->CurrentLinkSetSize = num1;
    }
    if(i==1)
    {
	Table->CurrentLinkSet = set2;
	Table->CurrentLinkSetSize = num2;
    }
    if(i==6 || i==7 || i==8)
    {
	Table->CurrentLinkSet = set3;
	Table->CurrentLinkSetSize = num3;
    }
}

void reallocate_cols(Table)
   struct Widget_Info *Table;
{
    int i;

    if(!Table->nalloc)
        Table->Cols = (struct Column_Info *) 
	       G_malloc(Table->AttTableNumCols * sizeof(struct Column_Info));
    else
        Table->Cols = (struct Column_Info *) 
	       G_realloc((char *) Table->Cols, 
			 Table->AttTableNumCols * sizeof(struct Column_Info));

    for(i=0; i<Table->AttTableNumCols; i++)
    {
       Table->Cols[i].Text = NULL;
       Table->Cols[i].Label = NULL;
       Table->Cols[i].Button = NULL;
       Table->Cols[i].Container = NULL;
       Table->Cols[i].col_num = 0;
    }
}

void
Create_Edit_Buttons(parent, Table)
    Widget parent; 
    struct Widget_Info *Table;
{
    Widget acceptB, helpB, doneB, cancelB;
    Arg al[10];
    int ac;

    acceptB = XmCreatePushButton(parent, "Accept", NULL, 0);
    XtAddCallback(acceptB, XmNactivateCallback, CallAccept, Table);
    XmAddTabGroup(acceptB);
    XtManageChild(acceptB);

    doneB = XmCreatePushButton(parent, "Done", NULL, 0);
    XtAddCallback(doneB, XmNactivateCallback, CallOk, Table);
    XmAddTabGroup(doneB);
    XtManageChild(doneB);

    cancelB = XmCreatePushButton(parent, "Cancel", NULL, 0);
    XtAddCallback(cancelB, XmNactivateCallback, CallCancel, Table);
    XmAddTabGroup(cancelB);
    XtManageChild(cancelB);

    helpB = XmCreatePushButton(parent, "Help", NULL, 0);
    XtAddCallback(helpB, XmNactivateCallback, CallHelp, Table);
    XmAddTabGroup(helpB);
    XtManageChild(helpB);

    FollowLinkB = XmCreatePushButton(parent, "Follow Link to Another Table", NULL, 0);
    XtAddCallback(FollowLinkB, XmNactivateCallback, CallFollow_link, Table);
    XmAddTabGroup(FollowLinkB);
    XtManageChild(FollowLinkB);

}



