#include "xgen.h"
#include <X11/StringDefs.h>

/***************************************************************
 * This routine takes a shell description and creates it.
 **************************************************************/
void
Create_Shell(s)
	Shell *s;
{
	Resource * resource;

/***************************************************************
 * switch on the shell type and call the appropriate routine
 **************************************************************/
	switch(s->type) {
		case MENU:
			if ( NULL != (resource = IndexResource(s,SHELL,"updatefrom")))
			    CreateMenu(s,True);
			else
				CreateMenu(s,False);
			break;
		case COMMANDBOARD:
			if ( NULL != (resource = IndexResource(s,SHELL,"pane"))) {
				if ( resource->val.bval ) 
			        CreateMessageBoard(s);
				else
			        CreateCommandBoard(s);
			} else
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
