/* * Last edited: Sep 23 15:15 1991 (mallet) */
/*
 * $Id: Dialog.c,v 1.3.1.3 1991/09/30 10:58:53 mallet Exp $
 * 
 * Copyright 1991 Lionel Mallet
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appears in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Lionel MALLET not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  Lionel MALLET makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * Lionel MALLET DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS, IN NO EVENT SHALL Lionel MALLET BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION 
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  This software is opened and free. Furthermore, everybody is kindly
 * invited to participate to improve it for the benefit of all.
 * Improvements can be new features, bugs fixes and porting issues
 * resolution.
 *
 * Author:  Lionel Mallet, SIMULOG
 */

/*
 * $XConsortium: Dialog.c,v 1.3 90/06/09 20:20:23 dmatic Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Davor Matic, MIT X Consortium
 */



#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xos.h>
#ifndef USE_ATHENA
#include <Xm/SelectioB.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#else USE_ATHENA
#include <X11/Shell.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Command.h>
#endif

#if ! defined(XlibSpecificationRelease)
#ifndef _XPOINTER_DEF
typedef char *XPointer;
#define _XPOINTER_DEF
#endif
#endif
    
#include "Dialog.h"

#define min(x, y)                     (((x) < (y)) ? (x) : (y))
#define max(x, y)                     (((x) > (y)) ? (x) : (y))

static int selected;

static void SetSelected(w, client_data, call_data)
     Widget w;
     XPointer client_data, call_data;
{
  int i;
    
  selected = (int)client_data;
}

#ifdef USE_ATHENA
static void SetOkay(w)
     Widget w;
{
  SetSelected(w, Okay | Yes );
}

static XtActionsRec actions_table[] = {
  {"set-okay", SetOkay},
};
#endif

Dialog CreateDialog(top_widget, name, options)
     Widget top_widget;
     String name;
     int options;
{
    int i;
    Dialog popup;
    Arg wargs[2];

    if (popup = (Dialog) XtMalloc(sizeof(_Dialog))) {
#ifdef USE_ATHENA
	XtAddActions(actions_table, XtNumber(actions_table));
#endif
	
	popup->top_widget = top_widget;
#ifndef USE_ATHENA
	popup->dialog_widget = XmCreatePromptDialog(top_widget, name, NULL, 0);
	popup->shell_widget = XtParent(popup->dialog_widget);

	XtUnmanageChild(XmSelectionBoxGetChild(popup->dialog_widget,
					       XmDIALOG_HELP_BUTTON));
	XtUnmanageChild(XmSelectionBoxGetChild(popup->dialog_widget,
					       XmDIALOG_OK_BUTTON));
	XtUnmanageChild(XmSelectionBoxGetChild(popup->dialog_widget,
					       XmDIALOG_APPLY_BUTTON));
	XtUnmanageChild(XmSelectionBoxGetChild(popup->dialog_widget,
					       XmDIALOG_CANCEL_BUTTON));
	
	for (i = 0; i < XtNumber(dialog_buttons); i++)
	    if (options & dialog_buttons[i].flag)
	      switch (dialog_buttons[i].flag)
		{
		case Okay:
		case Yes:
		case Abort:
		  XtSetArg(wargs[0], XmNlabelString, 
			   XmStringCreateLtoR(dialog_buttons[i].name,
					      XmSTRING_DEFAULT_CHARSET));
		  XtSetValues(XmSelectionBoxGetChild(popup->dialog_widget,
						       XmDIALOG_OK_BUTTON),
			      wargs, 1);
		  XtManageChild(XmSelectionBoxGetChild(popup->dialog_widget,
						       XmDIALOG_OK_BUTTON));
                  XtRemoveAllCallbacks(XmSelectionBoxGetChild(
                                                       popup->dialog_widget,
                                                       XmDIALOG_OK_BUTTON),
                                       XmNactivateCallback);
		  XtAddCallback(XmSelectionBoxGetChild(popup->dialog_widget,
						       XmDIALOG_OK_BUTTON),
				XmNactivateCallback, SetSelected, 
				dialog_buttons[i].flag);
		  break;
		case No:
		  XtSetArg(wargs[0], XmNlabelString, 
			   XmStringCreateLtoR(dialog_buttons[i].name,
					      XmSTRING_DEFAULT_CHARSET));
		  XtSetValues(XmSelectionBoxGetChild(popup->dialog_widget,
						       XmDIALOG_APPLY_BUTTON),
			      wargs, 1);
		  XtManageChild(XmSelectionBoxGetChild(popup->dialog_widget,
						       XmDIALOG_APPLY_BUTTON));
                  XtRemoveAllCallbacks(XmSelectionBoxGetChild(
                                                       popup->dialog_widget,
                                                       XmDIALOG_APPLY_BUTTON),
                                       XmNactivateCallback);
		  XtAddCallback(XmSelectionBoxGetChild(popup->dialog_widget,
						       XmDIALOG_APPLY_BUTTON),
				XmNactivateCallback, SetSelected, 
				dialog_buttons[i].flag);
		  break;
		case Cancel:
		case Retry:
		  XtSetArg(wargs[0], XmNlabelString, 
			   XmStringCreateLtoR(dialog_buttons[i].name,
					      XmSTRING_DEFAULT_CHARSET));
		  XtSetValues(XmSelectionBoxGetChild(popup->dialog_widget,
						       XmDIALOG_CANCEL_BUTTON),
			      wargs, 1);
		  XtManageChild(XmSelectionBoxGetChild(popup->dialog_widget,
						      XmDIALOG_CANCEL_BUTTON));
                  XtRemoveAllCallbacks(XmSelectionBoxGetChild(
                                                       popup->dialog_widget,
                                                       XmDIALOG_CANCEL_BUTTON),
                                       XmNactivateCallback);
		  XtAddCallback(XmSelectionBoxGetChild(popup->dialog_widget,
						       XmDIALOG_CANCEL_BUTTON),
				XmNactivateCallback, SetSelected, 
				dialog_buttons[i].flag);
		  break;
		}
#else USE_ATHENA
	popup->shell_widget = XtCreatePopupShell(name, 
						 transientShellWidgetClass, 
						 top_widget, NULL, 0);
	popup->dialog_widget = XtCreateManagedWidget("dialog", 
						     dialogWidgetClass,
						     popup->shell_widget, 
						     NULL, 0);
	for (i = 0; i < XtNumber(dialog_buttons); i++)
	    if (options & dialog_buttons[i].flag)
		XawDialogAddButton(popup->dialog_widget, 
				   dialog_buttons[i].name, 
				   SetSelected, dialog_buttons[i].flag);
#endif USE_ATHENA

	popup->options = options;
	return popup;
    }
    else
	return NULL;
}

void PopdownDialog(popup, answer)
    Dialog popup;
    String *answer;
{
    if (answer)
#ifndef USE_ATHENA
	*answer = XmTextGetString(XmSelectionBoxGetChild(popup->dialog_widget,
							 XmDIALOG_TEXT));
    
    XtUnmanageChild(popup->dialog_widget);
#else USE_ATHENA
	*answer = XawDialogGetValueString(popup->dialog_widget);
    
    XtPopdown(popup->shell_widget);
#endif USE_ATHENA
}

extern void unsetKillfromWM();

int PopupDialog(popup, message, suggestion, answer, grab)
    Dialog popup;
    String message, suggestion, *answer;
    XtGrabKind grab;
{
  Position popup_x, popup_y, top_x, top_y;
  Dimension popup_width, popup_height, top_width, top_height, border_width;
  int n;
  Arg wargs[4];

  n = 0;
#ifndef USE_ATHENA
  XtSetArg(wargs[n], XmNselectionLabelString, 
	   XmStringCreateLtoR(message, XmSTRING_DEFAULT_CHARSET)); n++;
  if (suggestion)
    {
      XtSetArg(wargs[n], XmNtextString, 
	       XmStringCreateLtoR(suggestion, XmSTRING_DEFAULT_CHARSET)); n++;
      XtManageChild(XmSelectionBoxGetChild(popup->dialog_widget, 
					   XmDIALOG_TEXT));
      XtRemoveAllCallbacks(XmSelectionBoxGetChild(popup->dialog_widget,
                                                  XmDIALOG_TEXT),
                           XmNactivateCallback);
      XtAddCallback(XmSelectionBoxGetChild(popup->dialog_widget,XmDIALOG_TEXT),
		    XmNactivateCallback, SetSelected, Okay | Yes);
    }
  else XtUnmanageChild(XmSelectionBoxGetChild(popup->dialog_widget, 
					      XmDIALOG_TEXT));
  XtSetValues(popup->dialog_widget, wargs, n);
#else USE_ATHENA
  XtSetArg(wargs[n], XtNlabel, message); n++;
  XtSetArg(wargs[n], XtNvalue, suggestion); n++;
  XtSetValues(popup->dialog_widget, wargs, n);

  XtRealizeWidget(popup->shell_widget);
#endif USE_ATHENA
  
  n = 0;
  XtSetArg(wargs[n], XtNx, &top_x); n++;
  XtSetArg(wargs[n], XtNy, &top_y); n++;
  XtSetArg(wargs[n], XtNwidth, &top_width); n++;
  XtSetArg(wargs[n], XtNheight, &top_height); n++;
  XtGetValues(popup->top_widget, wargs, n);

  n = 0;
  XtSetArg(wargs[n], XtNwidth, &popup_width); n++;
  XtSetArg(wargs[n], XtNheight, &popup_height); n++;
  XtSetArg(wargs[n], XtNborderWidth, &border_width); n++;
  XtGetValues(popup->shell_widget, wargs, n);

  popup_x = max(0, 
		min(top_x + ((Position)top_width - (Position)popup_width) / 2, 
		    (Position)DisplayWidth(XtDisplay(popup->shell_widget), 
			       DefaultScreen(XtDisplay(popup->shell_widget))) -
		    (Position)popup_width - 2 * (Position)border_width));
  popup_y = max(0, 
		min(top_y+((Position)top_height - (Position)popup_height) / 2,
		    (Position)DisplayHeight(XtDisplay(popup->shell_widget), 
			       DefaultScreen(XtDisplay(popup->shell_widget))) -
		    (Position)popup_height - 2 * (Position)border_width));
  n = 0;
  XtSetArg(wargs[n], XtNx, popup_x); n++;
  XtSetArg(wargs[n], XtNy, popup_y); n++;
  XtSetValues(popup->shell_widget, wargs, n);

  selected = Empty;

#ifndef USE_ATHENA
  XtManageChild(popup->dialog_widget);
#else USE_ATHENA
  XtPopup(popup->shell_widget, grab);
#endif USE_ATHENA
  unsetKillfromWM(popup->shell_widget);

#ifndef sparc
  XtSetKeyboardFocus(popup->top_widget, popup->shell_widget);
#endif sparc
  
  while ((selected & popup->options) == Empty) {
      XEvent event;
      XtNextEvent(&event);
      XtDispatchEvent(&event);
  }

  PopdownDialog(popup, answer);

  return (selected & popup->options);
}



