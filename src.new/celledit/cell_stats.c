/***********************************************************************

File     	:	cell_stats.c
Function 	:	BuildCellStats(w)
Args	 	:	    Widget w; -- Our Widget
			    char *name; -- cell file name

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	13 May 1990
Last Revised	:
Abstract 	:	Build the cell stats part of the interface.
Returns  	:	None.

***********************************************************************/
#include "cell_editor.h"
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>

void BuildCellStats();
void UpdateCellStats();
void QueryEditor();

static Widget cellForm;
static Widget cellLoc;
static Widget cellFName;
static Widget value;
static Widget rowCol;


void BuildCellStats(w, name)
    Widget w;
    char *name;
    {
    Arg args[1];
    char locStr[80];
    char nameStr[80];
    char rowStr[80];
    char valStr[80];
    Widget parent = XtParent(XtParent(w)); /* topForm */
    Dimension nameW, locW;
    Dimension oldW, oldH;
    Dimension formW, formH, formBW;
    Dimension newW, newH;
    Dimension retW, retH;
    int vert;
    Cardinal num;
    XtGeometryResult status;

    /* trap motion events */
    XtAddEventHandler(w, PointerMotionMask, FALSE, QueryEditor, NULL);

    /* get original dimensions */
    XtVaGetValues(parent, XtNwidth, (XtArgVal)&oldW, 
			  XtNheight, (XtArgVal)&oldH, NULL);
    
    /* create the form */
    cellForm = 
	XtVaCreateWidget("cellForm", formWidgetClass, parent,
		XtNfromVert, (XtArgVal)XtParent(w), NULL);

    /* create the filename label */
    sprintf(nameStr, "File Name: %s", name);
    cellFName = 
	XtVaCreateManagedWidget("cellFName", labelWidgetClass, cellForm,
		XtNlabel, (XtArgVal)nameStr, NULL);

    /* get its width */
    XtVaGetValues(cellFName, XtNwidth, (XtArgVal)&nameW, NULL);
    printf("Cell Name Width = %d\n", (int)nameW);

    /* create the location label */
    sprintf(locStr, "Location: %s", G_location());
    cellLoc =  
	XtVaCreateManagedWidget("cellLoc", labelWidgetClass, cellForm,
		XtNlabel, (XtArgVal)locStr, 
		XtNfromHoriz, (XtArgVal)cellFName, 
		NULL);

    /* get its width */
    XtVaGetValues(cellFName, XtNwidth, (XtArgVal)&locW, NULL);

    /* create the value widget with the same width as cellFName */
    sprintf(valStr, "Data Value: 0");
    value = 
	XtVaCreateManagedWidget("value", labelWidgetClass, cellForm,
        	XtNlabel, (XtArgVal)valStr,
		XtNfromVert, (XtArgVal)cellFName,
		XtNwidth, (XtArgVal)nameW, NULL);

    /* create the rowCol widget withthe same width as cellLoc */
    sprintf(rowStr, "Row,Col: 0,0");
    rowCol = 
	XtVaCreateManagedWidget("rowCol", labelWidgetClass, cellForm,
    		XtNlabel, rowStr,
		XtNfromVert, (XtArgVal)cellLoc, 
		XtNfromHoriz, (XtArgVal)value,
	        XtNwidth, (XtArgVal)locW, NULL);

    /* now that our children exist */
    XSync(XtDisplay(parent), FALSE);
    XtRealizeWidget(cellForm);
    XtManageChild(cellForm);
    XSync(XtDisplay(parent), FALSE);

    XtVaGetValues(cellForm, XtNwidth, (XtArgVal)&formW,
			    XtNheight, (XtArgVal)&formH,
			    XtNborderWidth, (XtArgVal)&formBW, 
			    XtNvertDistance, &vert,
			    NULL);

    printf("new part is %dX%d %d\n", formW, formH, (int)formBW);

    formW += (Dimension)(2*(int)formBW);
    formH += (Dimension)((2*(int)formBW) + vert);

    printf("new part is %dX%d\n", formW, formH);
    if (formW <= oldW )
	newW = oldW;
    else
	newW = oldW + (formW - oldW);

    newH = formH + oldH;
    status = XtMakeResizeRequest(parent, newW, newH, &retW, &retH);
    printf("Asked for %dx%d, got %dx%d\n", newW, newH, retW, retH);
    switch(status)
	{
	case XtGeometryYes:
	    printf("Yes\n");
	    break;
	case XtGeometryAlmost:
	    printf("Asked for %dx%d, got %dx%d\n", newW, newH, retW, retH);
	    break;
	case XtGeometryNo:
	    printf("No\n");
	    break;
	}
    XtMapWidget(cellForm);
    (void)UpdateCellStats(NULL, 0, 0);
    }

/***********************************************************************

File     	:	cell_stats.c
Function 	:	QueryEditor(w, close, ev, ctd)
Args	 	:	    w Widget; -- Our Widget
  	    		    XtPointer close; -- extraction source
			    XEvent *ev; -- the event
			    Boolean ctd; -- continue to dispatch

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:
Abstract 	:	Extracts and returns a substring from a raw string.
Returns  	:	None.

***********************************************************************/
void QueryEditor(w, close, ev, ctd )
    Widget w;
    XtPointer close; /* UNUSED */
    XEvent *ev;
    Boolean ctd; /* UNUSED */
    {
    int x, y;
    int width, height;

    if(!loaded)
	return;

    if (ev->type != MotionNotify)
	return;

    GetImageExtents(&width, &height);
    x = ((XMotionEvent *)ev)->x;
    y = ((XMotionEvent *)ev)->y;

    printf("x,y - %d,%d\n", x, y);
    printf("width,height - %d,%d\n", width, height);

    if ( (x >= width) || (y >= height) )
	return; /* not on image */
    printf("Trying to update stats\n");
    (void)UpdateCellStats(NULL, x, y);
    }


void UpdateCellStats(fileName, x, y)
    char *fileName;
    int x, y;
    {
    char nameStr[80];
    char valStr[20];
    char rowColStr[20];
    int row;
    int col;
    int val;
    int width;
    Arg args[3];

    QueryCellFile(editor, x, y, &row, &col, &val);
    sprintf(valStr, "Data: %ld", val);
    sprintf(rowColStr, "Row, Col: %d, %d", row, col);
    fflush(stdout);
    if (fileName != NULL)
	{
	sprintf(nameStr, "Cell File: %s", fileName);
	SetLabelStr(cellFName, nameStr);
	}
    SetLabelStr(value, valStr);
    SetLabelStr(rowCol, rowColStr);
    }

