#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Form.h>


Window form_win;
extern Display* the_display;
extern Widget fo;

void change_enter(widget, event, params, nparams)
	Widget widget;
	XEvent *event;
	char **params;
	int *nparams;
{
	Arg arglist[1];

	XtSetArg(arglist[0], XtNbackground, pixel("cyan"));
	XtSetValues(widget, arglist, 1);
}	

void change_leave(widget, event, params, nparams)
	Widget widget;
	XEvent *event;
	char **params;
	int *nparams;
{
	 Arg arglist[1];

	XtSetArg(arglist[0], XtNbackground, pixel("white"));
	XtSetValues(widget, arglist, 1);
}
