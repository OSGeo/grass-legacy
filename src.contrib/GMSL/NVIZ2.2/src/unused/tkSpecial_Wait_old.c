/* 
 * tkSpecial_Wait.c --
 *
 * This is just a hack off the original tkwait command so that
 * our scripting works correctly.  Basically, during waiting we
 * read lines from the scriptfile until waiting ends, then the
 * usual script reading mechanism takes over.
 */

#include "tkConfig.h"
#include "tkInt.h"
#include <errno.h>

/*
 * Forward declarations for procedures defined later in this file:
 */

static char *		WaitVariableProc _ANSI_ARGS_((ClientData clientData,
			    Tcl_Interp *interp, char *name1, char *name2,
			    int flags));
static void		WaitVisibilityProc _ANSI_ARGS_((ClientData clientData,
			    XEvent *eventPtr));
static void		WaitWindowProc _ANSI_ARGS_((ClientData clientData,
			    XEvent *eventPtr));

/*
 *----------------------------------------------------------------------
 *
 * Tk_Tkspecial_waitCmd --
 *
 *	This procedure is invoked to process the "tkspecial_wait" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *----------------------------------------------------------------------
 */

	/* ARGSUSED */
int
Tk_Tkspecial_waitCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    Tk_Window tkwin = (Tk_Window) clientData;
    int c, length;
    int done;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " variable|visible|window name\"", (char *) NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);
    if ((c == 'v') && (strncmp(argv[1], "variable", length) == 0)
	    && (length >= 2)) {
	Tcl_TraceVar(interp, argv[2],
		TCL_GLOBAL_ONLY|TCL_TRACE_WRITES|TCL_TRACE_UNSETS,
		WaitVariableProc, (ClientData) &done);
	done = 0;
	while (!done) {
	    if (!Tk_DoOneEvent(TK_DONT_WAIT))
	      Tcl_GlobalEval(interp, "PlayNextLine");
	}
	Tcl_UntraceVar(interp, argv[2],
		TCL_GLOBAL_ONLY|TCL_TRACE_WRITES|TCL_TRACE_UNSETS,
		WaitVariableProc, (ClientData) &done);
    } else if ((c == 'v') && (strncmp(argv[1], "visibility", length) == 0)
	    && (length >= 2)) {
	Tk_Window window;

	window = Tk_NameToWindow(interp, argv[2], tkwin);
	if (window == NULL) {
	    return TCL_ERROR;
	}
	Tk_CreateEventHandler(window, VisibilityChangeMask,
	    WaitVisibilityProc, (ClientData) &done);
	done = 0;
	while (!done) {
	    if (!Tk_DoOneEvent(TK_DONT_WAIT))
	      Tcl_GlobalEval(interp, "PlayNextLine");
	}
	Tk_DeleteEventHandler(window, VisibilityChangeMask,
	    WaitVisibilityProc, (ClientData) &done);
    } else if ((c == 'w') && (strncmp(argv[1], "window", length) == 0)) {
	Tk_Window window;

	window = Tk_NameToWindow(interp, argv[2], tkwin);
	if (window == NULL) {
	    return TCL_ERROR;
	}
	Tk_CreateEventHandler(window, StructureNotifyMask,
	    WaitWindowProc, (ClientData) &done);
	done = 0;
	while (!done) {
	    if (!Tk_DoOneEvent(TK_DONT_WAIT))
	      Tcl_GlobalEval(interp, "PlayNextLine ; after 1000");
	}
	/*
	 * Note:  there's no need to delete the event handler.  It was
	 * deleted automatically when the window was destroyed.
	 */
    } else {
	Tcl_AppendResult(interp, "bad option \"", argv[1],
		"\": must be variable, visibility, or window", (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Clear out the interpreter's result, since it may have been set
     * by event handlers.
     */

    Tcl_ResetResult(interp);
    return TCL_OK;
}

	/* ARGSUSED */
static char *
WaitVariableProc(clientData, interp, name1, name2, flags)
    ClientData clientData;	/* Pointer to integer to set to 1. */
    Tcl_Interp *interp;		/* Interpreter containing variable. */
    char *name1;		/* Name of variable. */
    char *name2;		/* Second part of variable name. */
    int flags;			/* Information about what happened. */
{
    int *donePtr = (int *) clientData;

    *donePtr = 1;
    return (char *) NULL;
}

	/*ARGSUSED*/
static void
WaitVisibilityProc(clientData, eventPtr)
    ClientData clientData;	/* Pointer to integer to set to 1. */
    XEvent *eventPtr;		/* Information about event (not used). */
{
    int *donePtr = (int *) clientData;
    *donePtr = 1;
}

static void
WaitWindowProc(clientData, eventPtr)
    ClientData clientData;	/* Pointer to integer to set to 1. */
    XEvent *eventPtr;		/* Information about event. */
{
    int *donePtr = (int *) clientData;

    if (eventPtr->type == DestroyNotify) {
	*donePtr = 1;
    }
}

