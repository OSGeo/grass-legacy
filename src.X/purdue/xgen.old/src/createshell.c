#include "xgen.h"
#include <X11/StringDefs.h>

/***************************************************************
 * This routine takes a shell description and creates it.
 **************************************************************/
void
Create_Shell(s)
	Shell *s;
{
	void ShellPopdownCB();

/***************************************************************
 * switch on the shell type and call the appropriate routine
 **************************************************************/
	switch(s->type) {
		case STATICMENU:
			CreateMenu(s,False);
			break;
		case DYNAMICMENU:
			CreateMenu(s,True);
			break;
		case MESSAGEBOARD:
			CreateMessageBoard(s);
			break;
		case COMMANDBOARD:
			CreateCommandBoard(s);
			break;
	}
/***************************************************************
 * add a destroy callback for the shell.
 * add a popdown callback for the shell.
 * if a shell is killed or popped down thru window manager 
 * interaction or something, we need to know.
 **************************************************************/
	XtAddCallback(s->widget,XtNdestroyCallback,ShellDestroyedCB,(caddr_t)s);
	XtAddCallback(s->widget,XtNpopdownCallback,ShellPopdownCB,(caddr_t)s);
}
