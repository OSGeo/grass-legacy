#include "xgrass_lib.h"
#include "Interact.h"
#include "Caption.h"
#include "gis.h"
#include <ctype.h>

static int done = 0;
static int retval = 0;

typedef struct _ask_format_data {
    struct Cell_head *cellhd;
    Widget rows, cols, bytes;
    long filesize;
} AskFormatData;

void 
XgAskFormatEventLoop(appContext, widget)
XtAppContext    appContext;
Widget widget;
{
    Display *display;

    done = 0;
    retval = 0;
    while (done == 0 || XtPending()) {
	XEvent          event;
	XtAppNextEvent(appContext, &event);
	XtDispatchEvent(&event);
    }
    XtUnmanageChild(widget);
    XSync(XtDisplay(widget), False);
    while (XtPending()) {
        XEvent          event;
        XtAppNextEvent(appContext, &event);
        XtDispatchEvent(&event);
    }

}

void
AskFormatCancelCallBack(w, cld)
Widget w;
XtPointer cld;
{
    done = 1;
    retval = 0;
    return;
}

void
AskFormatOkCallBack(w, cld)
Widget w;
XtPointer cld;
{
  Widget xgi = w;
  AskFormatData *data = (AskFormatData *)cld;
  char *rowsText, *colsText, *bytesText;
  XmString xms;
  Widget child;
  char buffer[8096];
  struct Cell_head *cellhd;
  int rows, cols, bytes;

  rowsText = XmTextFieldGetString(data->rows);
  colsText = XmTextFieldGetString(data->cols);
  bytesText = XmTextFieldGetString(data->bytes);
  cellhd = data->cellhd;
  rows = atoi(rowsText);
  cols = atoi(colsText);
  bytes = atoi(bytesText);
  if ( rows <= 0 || cols <= 0 || bytes <= 0 ) {
      if ( rows <= 0 ) {
	  sprintf(buffer,"%d",cellhd->rows);
	  XmTextFieldSetString(data->rows, buffer);
      } else if ( cols <= 0 ) {
	  sprintf(buffer,"%d",cellhd->cols);
	  XmTextFieldSetString(data->cols, buffer);
      } else {
	  sprintf(buffer,"%d",cellhd->format);
	  XmTextFieldSetString(data->bytes, buffer);
      }
      XgWarningDialog(XtParent(xgi), "Enter positive values only!"); 
      return;
  }
  cellhd->rows = rows;
  cellhd->cols = cols;
  cellhd->format = bytes;

  if ( cellhd->compressed == 0 && 
       cellhd->rows * cellhd->cols * cellhd->format != data->filesize ) {

    if ( cellhd->format == 0 || data->filesize%cellhd->format != 0 ) {
	int i;
	char tbuf[80];

	buffer[0] = 0;
        for (i = 1; i <= sizeof (CELL); i++) {
	    if ( data->filesize%i) continue;
	    sprintf(tbuf,"%d byte%s per cell\n", i, i==1?"":"s");
	    strcat(buffer,tbuf);
	    _XgAskFormatFactors(buffer, data->filesize, i);
	}
    } else {
	buffer[0] = 0;
	_XgAskFormatFactors(buffer, data->filesize, cellhd->format);
    }
    xms = XmStringCreateLtoR(buffer, XmSTRING_DEFAULT_CHARSET);
    child = XgInteractorGetChild(xgi,XmINTERACT_PROMPT_LABEL);
    XtVaSetValues(child,
	XmNlabelString, xms, NULL);
    XtManageChild(child);
    return;
  }
  done = 1;
  retval = 1;
  return;
}

Widget
DoAskFormatDialog(shell, cellhd, filesize) 
    Widget shell;
    struct Cell_head *cellhd;
    long filesize;
{
    Widget xgi;
    Widget child;
    Widget form;
    Widget rowCaption;
    Widget rowText;
    Widget colCaption;
    Widget colText;
    Widget bytesCaption;
    Widget bytesText;
    Arg al[10];
    int ac = 0;
    char rows[10];
    char cols[10];
    char bytes[10];
    Boolean constant = False;
    AskFormatData *data = (AskFormatData *)XtMalloc(sizeof(AskFormatData));

    XtSetArg(al[ac], XmNautoUnmanage, False); ac++;
    XtSetArg(al[ac], XmNenableWorkAreaStretch, True); ac++;
    xgi = XgCreateInteractorDialog(shell,"Raster Header Format Dialog",al,ac);
    child = XgInteractorGetChild(xgi, XmINTERACT_HELP_BUTTON);
    XtUnmanageChild(child);

    form = XtVaCreateManagedWidget("form", xmFormWidgetClass, xgi, NULL);

    rowCaption = XtVaCreateManagedWidget("row_caption", 
	xbaeCaptionWidgetClass, form,
	XmNleftAttachment, XmATTACH_FORM,
	XmNrightAttachment, XmATTACH_FORM,
	XmNtopAttachment, XmATTACH_FORM,
	XmNlabelPosition, XbaePositionLeft,
	XmNlabelString, XmStringCreateSimple("Number of Rows"),
	XmNlabelAlignment, XbaeAlignmentCenter,
	XmNtraversalOn, False,
	NULL);

    sprintf(rows,"%d", cellhd->rows);
    if (cellhd->compressed) constant = True;
    rowText = XtVaCreateManagedWidget("row_text", xmTextFieldWidgetClass, 
	rowCaption,
	XmNcolumns, 15,
	XmNvalue, rows,
	XmNeditable, constant,
	NULL);

    colCaption = XtVaCreateManagedWidget("column_caption", 
	xbaeCaptionWidgetClass, form,
	XmNleftAttachment, XmATTACH_FORM,
	XmNrightAttachment, XmATTACH_FORM,
	XmNtopAttachment, XmATTACH_WIDGET,
	XmNtopWidget, rowCaption,
	XmNlabelPosition, XbaePositionLeft,
	XmNlabelString, XmStringCreateSimple("Number of Columns"),
	XmNlabelAlignment, XbaeAlignmentCenter,
	XmNtraversalOn, False,
	NULL);

    sprintf(cols,"%d", cellhd->cols);
    colText = XtVaCreateManagedWidget("col_text", xmTextFieldWidgetClass, 
	colCaption,
	XmNcolumns, 15,
	XmNvalue, cols,
	NULL);

    bytesCaption = XtVaCreateManagedWidget("bytes_caption", 
	xbaeCaptionWidgetClass, form,
	XmNleftAttachment, XmATTACH_FORM,
	XmNrightAttachment, XmATTACH_FORM,
	XmNtopAttachment, XmATTACH_WIDGET,
	XmNtopWidget, colCaption,
	XmNbottomAttachment, XmATTACH_FORM,
	XmNlabelPosition, XbaePositionLeft,
	XmNlabelString, XmStringCreateSimple("Number of Bytes per Cell"),
	XmNlabelAlignment, XbaeAlignmentCenter,
	XmNtraversalOn, False,
	NULL);

    sprintf(bytes,"%d", cellhd->format);
    bytesText = XtVaCreateManagedWidget("bytes_text", xmTextFieldWidgetClass, 
	bytesCaption,
	XmNcolumns, 15,
	XmNvalue, bytes,
	NULL);

    data->cellhd = cellhd;
    data->rows = rowText;
    data->cols = colText;
    data->bytes = bytesText;
    data->filesize = filesize;
    XtAddCallback(xgi, XmNokCallback, AskFormatOkCallBack, data);
    XtAddCallback(xgi, XmNcancelCallback, AskFormatCancelCallBack, NULL);

    XtManageChild(xgi);
    return xgi;
}

int
XgDoAskFormat(shell, cellhd, filesize) 
    Widget shell;
    struct Cell_head *cellhd;
    long filesize;
{
	XtAppContext    appContext;
	Widget widget;

	widget = DoAskFormatDialog(shell, cellhd, filesize, &done);
	appContext = XtWidgetToApplicationContext(shell);
	XgAskFormatEventLoop(appContext, widget);
	return retval;
}

_XgAskFormatFactors(buffer, n, div)
    char *buffer;
    long n;
    int div;
{
    register long m;
    register long x;
    char tbuf[256];
    char buf[30];
    int len, totlen;
    char fmt[30];

    sprintf(tbuf, 
     "rows * cols * bytes per cell must be the\nsame as the filesize (%ld)\n",
     n);
    strcat(buffer,tbuf);

    sprintf (buf, "%ld", n);
    len = strlen (buf);
    n /= div;

    sprintf (fmt, "%%%dld * %%-%dld", len, len);
    totlen = 0;
    for (m = 1;; m++)
    {
        if (n%m == 0)
        {
            x = n/m;
            if (x < m) break;
            sprintf (buf, fmt, m, x) ;
            len = strlen (buf) + 3;
            if (totlen + len > 75)
            {
                sprintf (tbuf, "\n");
                strcat(buffer, tbuf);
                totlen = 0;
            }
            sprintf (tbuf, "%s   ", buf);
	    strcat(buffer, tbuf);
            totlen += len;
        }
    }
    if (totlen) {
        sprintf (tbuf, "\n");
	strcat(buffer, tbuf);
    }

}
