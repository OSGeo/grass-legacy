/**********************************************************************
   client.c      - handle client message events, parse decor and funcs
 *********************************************************************/
/*******************************************************************************
Xgen was developed by Kurt Buehler, while at the Center for Advanced Decision
Support for Water and Environmental Systems (CADSWES), University of Colorado
at Boulder and at the Indiana Water Resources Research Center (IWRRC),
Purdue University for the U.S. Army Construction Engineering Research
Laboratory in support of the Geographical Resources Analysis Support
System (GRASS) software. The example scripts were developed by Ms. Christine
Poulsen of USA-CERL, much thanks goes to her for her work.

Permission to use, copy, modify and distribute without charge this software,
documentation, etc. is granted, provided that this comment is retained,
and that the names of Kurt Buehler, Christine Poulsen, CADSWES, IWRRC,
the University of Colorado at Boulder, Purdue University, or USA-CERL are not
used in advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

The author disclaims all warranties with regard to this software, including
all implied warranties of merchantability and fitness, in no event shall
the author be liable for any special, indirect or consequential damages or
any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this
software.
*******************************************************************************/

#include "xgen.h"

void
XgenClientMessage(w, cld, msg)
Widget w;
XtPointer cld;
XClientMessageEvent *msg;
{
    Shell *shell = (Shell *)cld;
    int   message = msg->data.l[0];
    Atom  WM_DELETE_WINDOW;

    if (msg->type != ClientMessage)
        return;

    WM_DELETE_WINDOW = XmInternAtom(msg->display, "WM_DELETE_WINDOW", False);

    if (message == WM_DELETE_WINDOW) {
        if ( w == xgenGD.applShell ) {
	    int exit_code = 0;

	    XgenExit(exit_code);
	} else if ( shell == NULL ) { /* XgEditor sets cld = NULL */
	    XtPopdown(w);
	} else {
	    PopdownShell(shell);
	}
    }
}

static struct DecorationTable {
    char *name;
    unsigned int flag;
} decorTable[] = {
    "all",      MWM_DECOR_ALL,
    "border",   MWM_DECOR_BORDER,
    "resizeh",  MWM_DECOR_RESIZEH,
    "title",    MWM_DECOR_TITLE,
    "menu",     MWM_DECOR_MENU,
    "minimize", MWM_DECOR_MINIMIZE,
    "maximize", MWM_DECOR_MAXIMIZE,
    NULL, 0,
};

unsigned int
ParseDecorations(s)
char *s;
{
    char **token, **Tokenize();
    int num;
    unsigned int flags = 0;
    int i,j;

    token = Tokenize(s,",");
    num = NumberOfTokens(token);
    for ( i = 0; i < num; i++ ) {
	for ( j = 0; decorTable[j].name; j++ ) {
	    if ( !strcmp(token[i],decorTable[j].name) ) {
		flags |= decorTable[j].flag;
	    }
	}
    }
    return flags;
}

static struct FunctionTable {
    char *name;
    unsigned int flag;
} funcTable[] = {
    "all",      MWM_FUNC_ALL,
    "resize",   MWM_FUNC_RESIZE,
    "move",     MWM_FUNC_MOVE,
    "close",    MWM_FUNC_CLOSE,
    "minimize", MWM_FUNC_MINIMIZE,
    "maximize", MWM_FUNC_MAXIMIZE,
    NULL, 0,
};

unsigned int
ParseFunctions(s)
char *s;
{
    char **token, **Tokenize();
    int num;
    unsigned int flags = 0;
    int i,j;

    token = Tokenize(s,",");
    num = NumberOfTokens(token);
    for ( i = 0; i < num; i++ ) {
	for ( j = 0; funcTable[j].name; j++ ) {
	    if ( !strcmp(token[i],funcTable[j].name) ) {
		flags |= funcTable[j].flag;
	    }
	}
    }
    return flags;
}
