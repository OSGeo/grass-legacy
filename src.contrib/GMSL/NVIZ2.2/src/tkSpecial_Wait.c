/* 
 * tkSpecial_Wait.c --
 *
 * This is just a hack off the original tkwait command so that
 * our scripting works correctly.  Basically, during waiting we
 * read lines from the scriptfile until waiting ends, then the
 * usual script reading mechanism takes over.
 */

/* 
 * tkCmds.c --
 *
 *	This file contains a collection of Tk-related Tcl commands
 *	that didn't fit in any particular file of the toolkit.
 *
 * Copyright (c) 1990-1994 The Regents of the University of California.
 * Copyright (c) 1994-1995 Sun Microsystems, Inc.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 */

static char sccsid[] = "@(#) tkCmds.c 1.100 95/06/24 17:43:37";

#include "tkPort.h"
#include "tkInt.h"
#include <errno.h>

/*
 * Forward declarations for procedures defined later in this file:
 */

static Tk_Window	GetDisplayOf _ANSI_ARGS_((Tcl_Interp *interp,
			    Tk_Window tkwin, char **argv));
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
 * Tk_TkwaitCmd --
 *
 *	This procedure is invoked to process the "tkwait" Tcl command.
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
Tk_Tkspecial_waitCmd (
    ClientData clientData,	/* Main window associated with
				 * interpreter. */
    Tcl_Interp *interp,		/* Current interpreter. */
    int argc,			/* Number of arguments. */
    char **argv		/* Argument strings. */
)
{
    Tk_Window tkwin = (Tk_Window) clientData;
    int c, done;
    size_t length;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
		argv[0], " variable|visible|window name\"", (char *) NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    length = strlen(argv[1]);
    if ((c == 'v') && (strncmp(argv[1], "variable", length) == 0)
	    && (length >= 2)) {
	if (Tcl_TraceVar(interp, argv[2],
		TCL_GLOBAL_ONLY|TCL_TRACE_WRITES|TCL_TRACE_UNSETS,
		WaitVariableProc, (ClientData) &done) != TCL_OK) {
	    return TCL_ERROR;
	}
	done = 0;

	/* Basically, to avoid holding up scripts we give Tcl/Tk a
	 * chance to catch up on pending events every once in a while.
	 * In the meantime we keep on playing the script.
	 */
	while (!done) {
	    if (!Tcl_DoOneEvent(TCL_DONT_WAIT))
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

	/* Basically, to avoid holding up scripts we give Tcl/Tk a
	 * chance to catch up on pending events every once in a while.
	 * In the meantime we keep on playing the script.
	 */
	while (!done) {
	  if (!Tcl_DoOneEvent(TCL_DONT_WAIT))
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

	/* Basically, to avoid holding up scripts we give Tcl/Tk a
	 * chance to catch up on pending events every once in a while.
	 * In the meantime we keep on playing the script.
	 */
	while (!done) {
	  if (!Tcl_DoOneEvent(TCL_DONT_WAIT))
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
WaitVariableProc (
    ClientData clientData,	/* Pointer to integer to set to 1. */
    Tcl_Interp *interp,		/* Interpreter containing variable. */
    char *name1,		/* Name of variable. */
    char *name2,		/* Second part of variable name. */
    int flags			/* Information about what happened. */
)
{
    int *donePtr = (int *) clientData;

    *donePtr = 1;
    return (char *) NULL;
}

	/*ARGSUSED*/
static void 
WaitVisibilityProc (
    ClientData clientData,	/* Pointer to integer to set to 1. */
    XEvent *eventPtr		/* Information about event (not used). */
)
{
    int *donePtr = (int *) clientData;
    *donePtr = 1;
}

static void 
WaitWindowProc (
    ClientData clientData,	/* Pointer to integer to set to 1. */
    XEvent *eventPtr		/* Information about event. */
)
{
    int *donePtr = (int *) clientData;

    if (eventPtr->type == DestroyNotify) {
	*donePtr = 1;
    }
}
