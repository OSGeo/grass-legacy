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

#define NCOLS 15
Widget          *AttTexts;
Widget          *AttButtons;
Widget          *AttLabels;
static Widget acceptB, upB, downB, follow_linkB, helpB, doneB, cancelB;
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
static int            AttTableMinCol=0;
static Widget         AttTableParent;
static Widget         AttTable;
static int            AttTableNumCols;
static char           AttTableName[500];

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
    Arg             al[30];
    int             ac;

    theDialog = XmCreateFormDialog(parent, name, NULL, 0);
    /* XmRemoveTabGroup(theDialog);*/

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
    AttTableMinCol = 16;
    AttTableParent = theDialog;

    ac = 0;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNorientation, XmHORIZONTAL); ac++;
    ButtonPanel = XmCreateRowColumn(theDialog, "ButtonPanel", al, ac);
    Create_Edit_Buttons(ButtonPanel);
    XtManageChild(ButtonPanel);

    ac=0;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
    XtSetArg(al[ac], XmNtopWidget, attLB); ac++;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
    XtSetArg(al[ac], XmNbottomWidget, ButtonPanel); ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;	
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    AttTable = XmCreateForm(theDialog, "AttTable", al,ac); 
    /* now create the children of AttTable */
    Create_Att_Table_Children();
    XtManageChild(AttTable);

/*
    ac = 0;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
    XtSetArg(al[ac], XmNbottomWidget, done_button); ac++;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
    XtSetArg(al[ac], XmNtopWidget, AttTable); ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNlabelString,
             XmStringCreate("name of the object", XmSTRING_DEFAULT_CHARSET));ac++;
    CatLabelDescr = XmCreateLabel(theDialog, "cat lab descr", al, ac);
    XtManageChild(CatLabelDescr);

    ac = 0;
    XtSetArg(al[ac], XmNbottomAttachment, XmATTACH_WIDGET); ac++;
    XtSetArg(al[ac], XmNbottomWidget, done_button); ac++;
    XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
    XtSetArg(al[ac], XmNtopWidget, AttTable); ac++;
    XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
    XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
    XtSetArg(al[ac], XmNleftWidget, CatLabelDescr); ac++; 
    
    CatLabel = XtCreateWidget(theDialog, xmTextWidgetClass, theDialog, al, ac);
    XmTextSetString(CatLabel, "cat label");
    XtAddCallback(CatLabel, XmNactivateCallback, CallCatLabel, NULL);
    XtAddCallback(CatLabel, XmNlosingFocusCallback, CallCatLabel, NULL);
    XmAddTabGroup(CatLabel);
    XtManageChild(CatLabel);
*/

    return (theDialog);
}

void Destroy_Att_Table_Children()
{
    int i;

    for (i = AttTableMinCol; i < AttTableMinCol + NCOLS; i++)
    {
	if(i>=AttTableNumCols) break;
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

    for (i = AttTableMinCol; i < AttTableMinCol + NCOLS; i++)
    {
	if(i>=AttTableNumCols) break;
	if(AttLabels[i] && AttTexts[i] && 
	  (!link_exists(i) || AttButtons[i]))
	{
	   XtManageChild(AttLabels[i]);
	   XtManageChild(AttTexts[i]);
	   if(link_exists(i))
		   XtManageChild(AttButtons[i]);
	   continue;
        }
	/* CREATE ATTR DESCRIPTION */
        sprintf(name, "lab%d", i + 1);
	if(i==5)
	   sprintf(name,"olechka-zain'ka");
        AttLabels[i] = XmCreateLabel(AttTable, name, NULL, 0);
	XtManageChild(AttLabels[i]);

	/* CREATE ATTR TEXT EDIT FIELD */
        sprintf(name, "%3d", i + 1);
	AttTexts[i] = XtCreateWidget(name, xmTextWidgetClass, AttTable, NULL, 0);
           XmAddTabGroup(AttTexts[i]);
	XtManageChild(AttTexts[i]);

        /* CREATE LINK BUTTON IF EXISTS */
	if(link_exists(i))
	{
           sprintf(name, "link%d", i + 1);
           AttButtons[i] = XmCreatePushButton(AttTable, name, NULL, 0);
	   XtManageChild(AttButtons[i]);
        }

        /* SET ARGUMENTS ATTRIBUTE DESCRIPTION */
        ac = 0;
	if(i==AttTableMinCol)
	{
	   XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
        }
        else
	{
	   XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
           XtSetArg(al[ac], XmNtopWidget, AttTexts[i-1]); ac++;
        }
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_FORM); ac++;
        sprintf(name, "lab%d", i + 1);
	if(i==5)
	   sprintf(name,"olechka-zain'ka ochen' horoshaya devochka  ");
        XtSetArg(al[ac], XmNlabelString,
             XmStringCreate(name, XmSTRING_DEFAULT_CHARSET));ac++;

	XtSetValues(AttLabels[i], al, ac);

	/* SET ARGUMENTS FOR TEXT EDIT FIELD */
        ac = 0;
	if(i==AttTableMinCol)
	{
	   XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
        }
        else
	{
	   XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
           XtSetArg(al[ac], XmNtopWidget, AttTexts[i-1]); ac++;
        }
	if(!link_exists(i))
	{
	     XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	}
        else
	{
	     XtSetArg(al[ac], XmNrightAttachment, XmATTACH_WIDGET); ac++;
	     XtSetArg(al[ac], XmNrightWidget, AttButtons[i]); ac++;
        }
	XtSetArg(al[ac], XmNleftAttachment, XmATTACH_WIDGET); ac++;
	XtSetArg(al[ac], XmNleftWidget, AttLabels[i]); ac++;
        values[i] = XtMalloc(20);
	/******** WIDTH */
        sprintf(values[i], "0");
	XmTextSetString(AttTexts[i], values[i]);
        XtAddCallback(AttTexts[i], XmNlosingFocusCallback, GetAttText, i);
        XtAddCallback(AttTexts[i], XmNactivateCallback, GetAttText, i);
	XtSetValues(AttTexts[i], al, ac);

        /* CREATE BUTTONS FOR FOREIGN KEYS */
	/* now if the attribute is a foreign or primary key, make a button */
	if(link_exists(i))
	/* make a button */
	{
	   ac = 0;
           sprintf(name, "link%d", i + 1);
	   if(i==AttTableMinCol)
	   {
	     XtSetArg(al[ac], XmNtopAttachment, XmATTACH_FORM); ac++;
           }
           else
	   {
	     XtSetArg(al[ac], XmNtopAttachment, XmATTACH_WIDGET); ac++;
             XtSetArg(al[ac], XmNtopWidget, AttTexts[i-1]); ac++;
           }
           XtSetArg(al[ac], XmNlabelString,
             XmStringCreate("Show Links", XmSTRING_DEFAULT_CHARSET)); ac++;
	   XtSetArg(al[ac], XmNrightAttachment, XmATTACH_FORM); ac++;
	   XtSetValues(AttButtons[i], al, ac);
           XtAddCallback(AttButtons[i], XmNactivateCallback, show_links, i);
	}

    }
    XtManageChildren(AttTexts, XtNumber(AttTexts));
    XtManageChildren(AttLabels, XtNumber(AttLabels));
    XtManageChild(AttTable);
}

void 
show_links(w, client_data, data)
    Widget          w;
    int             client_data;
    caddr_t         data;
{
    int i = client_data;
    printf("follow link: %d\n", i);
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
    for (i = AttTableMinCol; i < AttTableMinCol + NCOLS; i++)
    {
	if(i>=AttTableNumCols) break;
            sprintf(values[i], "%s", XmTextGetString(AttTexts[i]));
	    printf("col %d: %s\n", i, values[i]);
    }
    /*printf("att label: %s\n", XmTextGetString(CatLabel));*/
    XtUnmanageChild(parent);
}

#define NEXT 1
#define PREV -1

void
CallPage(w, direction, call_data)
    Widget w;
    int direction;
    caddr_t call_data;
{
    int i;
    if(direction==PREV &&  AttTableMinCol == 0) return;
    if(direction==NEXT &&  (AttTableMinCol+NCOLS) >= AttTableNumCols) 
       return;

    printf("destroying table and drawing new one \n");
    /* first remember all the current entries */
    for (i = AttTableMinCol; i < AttTableMinCol + NCOLS; i++)
    {
        if(i>=AttTableNumCols) break;
	sprintf(values[i], "%s", XmTextGetString(AttTexts[i]));
        printf("col %d: %s\n", i, values[i]);
    }

    /* destroy old table and create the new one */
    XtUnmanageChild(AttTable);
    Destroy_Att_Table_Children();
    AttTableMinCol = AttTableMinCol + direction * NCOLS;
    /* now create the children of AttTable */
    Create_Att_Table_Children();
    XtManageChild(AttTable);

    if(AttTableMinCol == 0) 
       XtUnmanageChild(upB);
    else
       XtManageChild(upB);

    if((AttTableMinCol+NCOLS) < AttTableNumCols) 
       XtManageChild(downB);
    else
       XtUnmanageChild(downB);

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

    for (i = AttTableMinCol; i < AttTableMinCol + NCOLS; i++)
    {
	if(i>=AttTableNumCols) break;
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

    for (i = AttTableMinCol; i < AttTableMinCol + NCOLS; i++)
    {
	if(i>=AttTableNumCols) break;
        sprintf(values[i], "");
	XmTextSetString(AttTexts[i], values[i]);
    }
}

static int link_exists(i)
   int i;
{
   return (i==2 || i==4 || i==5 || i==8 || i==25 || i==30 || i==34);
}

void reallocate_cols()
{
    int i;

    if(!nalloc)
    {
        AttTexts = (Widget *) G_malloc(AttTableNumCols * sizeof(Widget));
        AttLabels = (Widget *) G_malloc(AttTableNumCols * sizeof(Widget));
        AttButtons = (Widget *) G_malloc(AttTableNumCols * sizeof(Widget));
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
        values = (char **) G_realloc((char *) values, 
					AttTableNumCols * sizeof(char *));
    }

    for(i=0; i<AttTableNumCols; i++)
    {
       AttTexts[i] = NULL;
       AttLabels[i] = NULL;
       AttButtons[i] = NULL;
    }
}

void
Create_Edit_Buttons(parent)
    Widget parent; 
{
    Arg al[10];
    int ac;

    acceptB = XmCreatePushButton(parent, "Accept", NULL, 0);
    XtAddCallback(acceptB, XmNactivateCallback, CallAccept, AttTable);
    XmAddTabGroup(acceptB);
    XtManageChild(acceptB);

    ac = 0;
    XtSetArg(al[ac], XmNarrowDirection, XmARROW_UP); ac++;
    upB = XmCreateArrowButton(parent, "Prev Page", al, ac);
    XmAddTabGroup(upB);
    XtAddCallback(upB, XmNactivateCallback, CallPage, PREV);
    if(AttTableMinCol != 0) 
       XtManageChild(upB);

    ac = 0;
    XtSetArg(al[ac], XmNarrowDirection, XmARROW_DOWN); ac++;
    downB = XmCreateArrowButton(parent, "Next Page", al, ac);
    XmAddTabGroup(downB);
    XtAddCallback(downB, XmNactivateCallback, CallPage, NEXT);
    if((AttTableMinCol+NCOLS) < AttTableNumCols) 
       XtManageChild(downB);

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
