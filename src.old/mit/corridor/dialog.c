#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/AsciiText.h>
#include <X11/StringDefs.h>
#include <X11/Command.h>
#include <X11/Shell.h>
#include <X11/Box.h>
#include <X11/Shell.h>
#include <X11/cursorfont.h>
#include <X11/Simple.h>
#include <X11/Dialog.h>
#include <X11/Core.h>
#include <X11/Form.h>
#include <stdio.h>


/* XtTranslations form_translations; */
extern cmd_translations;

extern Widget toplevel;
static Widget dialog_shell;

static char Form_translations[] =
	"<Key>k:cr()";	

static void cr()
{
fprintf(stderr, "popped down\n");
	exit(0);
}

static XtActionsRec actionlist[] =
        {
                {"cr", cr},
        };

/* ARGSUSED */
void Quit(widget,closure,callData)
    Widget widget;
    caddr_t closure;            /* Widget */
    caddr_t callData;
{
    fprintf(stderr,"command callback.\n");
    XtPopdown(dialog_shell);
}

void Destroyed(widget, closure, callData)
    Widget widget;              /* unused */
    caddr_t closure;            /* unused */
    caddr_t callData;           /* unused */
{
    fprintf( stderr, "everything now destroyed.  Bye!\n" );
    exit(0);
}

void dialog(w)
	Widget w;
{
	Widget  dialog_widget, conf_widget, canc_widget ;
	Arg args[20];
	int n;
	XFontStruct *font;
	Cursor  the_cursor;
	char *str;
	static XtCallbackRec callback[2];

	/* toplevel = XtInitialize("main", "XDiag", NULL, NULL, &argc, argv);*/
	font = XLoadQueryFont(XtDisplay(toplevel),"fg-16.snf");

	the_cursor = XCreateFontCursor(XtDisplay(toplevel), 86);

	/*form_translations = XtParseTranslationTable(Form_translations);
	XtAddActions(actionlist, 1);*/

	n = 0;
	XtSetArg(args[n], XtNwidth, 350); n++;
	XtSetArg(args[n], XtNheight, 90); n++;
	XtSetArg(args[n], XtNcursor, the_cursor); n++;
	XtSetArg(args[n], XtNborderWidth, 7); n++;
	

	dialog_shell = XtCreatePopupShell("dialog", topLevelShellWidgetClass, 
				toplevel,
				args, n);


	/* box = XtCreateManagedWidget("box", formWidgetClass, toplevel, args, n); */
	
	/* XtAddCallback( box, XtNdestroyCallback, Destroyed, NULL);*/	

	
	n = 0;
	XtSetArg(args[n], XtNlabel, "Enter cell name or select from menu"); n++;
	XtSetArg(args[n], XtNvalue, ""); n++;
	XtSetArg(args[n], XtNcursor, the_cursor); n++;
	/* XtSetArg(args[n], XtNheight,90); n++;
	XtSetArg(args[n], XtNwidth, 350); n++; */
	XtSetArg(args[n], XtNborderWidth, 5); n++;
	/* XtSetArg(args[n], XtNtranslations, form_translations); */

	dialog_widget = XtCreateManagedWidget("diag", dialogWidgetClass, 
			dialog_shell,args,n); 

	/* XtOverrideTranslations( dialog_widget, form_translations); */

	n = 0;
	callback[0].callback = Quit;
	callback[0].closure = (caddr_t) toplevel;
	XtSetArg( args[n], XtNlabel, "confirm"); n++;
	XtSetArg( args[n], XtNcallback, callback); n++;
	conf_widget =XtCreateManagedWidget( "command", commandWidgetClass,  dialog_widget,
			args, n );

	XtOverrideTranslations(conf_widget, cmd_translations);

	 n = 0;
        callback[0].callback = Quit;
        callback[0].closure = (caddr_t) toplevel;
        XtSetArg( args[n], XtNcallback, callback); n++;
	XtSetArg( args[n], XtNlabel, "cancel"); n++;
	canc_widget = XtCreateManagedWidget( "command2", commandWidgetClass, dialog_widget,
			args, n);	

	XtOverrideTranslations(canc_widget, cmd_translations);
	 XtPopup(dialog_shell, XtGrabNone);  
	/*
	str = XtDialogGetValueString(dialog_widget);
	printf("\n\n\n  answer : %s\n",str); 
	XtMainLoop();	
	*/
	/* return(dialog_widget); */

	
}
