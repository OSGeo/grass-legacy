/* 
 * main.c --
 *
 *	This file contains the main program for "wish", a windowing
 *	shell based on Tk and Tcl.  It also provides a template that
 *	can be used as the basis for main programs for other Tk
 *	applications.
 *
 * Copyright (c) 1990-1993 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#include <stdio.h>
#include <string.h>
#include <tk.h>

#include "gis.h"

/*
 * Command-line options:
 */

char *fileName = NULL;

Tk_ArgvInfo argTable[] = {
    {"-file", TK_ARGV_STRING, (char *) NULL, (char *) &fileName,
	"File from which to read commands"},
    {(char *) NULL, TK_ARGV_END, (char *) NULL, (char *) NULL,
	(char *) NULL}
};

static int Tk_AppInit(Tcl_Interp *interp);

/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	Main program for Wish.
 *
 * Results:
 *	None. This procedure never returns (it exits the process when
 *	it's done
 *
 * Side effects:
 *	This procedure initializes the wish world and then starts
 *	interpreting commands;  almost anything could happen, depending
 *	on the script being interpreted.
 *
 *----------------------------------------------------------------------
 */

void main(int argc, char *argv[])
{
    Tcl_Interp *interp;

    G_gisinit(argv[0]); 

    /*
     * Parse command-line arguments.
     */

    interp = Tcl_CreateInterp();
    if (Tk_ParseArgv(interp, (Tk_Window) NULL, &argc, argv, argTable, 0)
	    != TCL_OK) {
	fprintf(stderr, "%s\n", interp->result);
	exit(1);
    }
    Tcl_DeleteInterp(interp);

    /*
     * Initialize and run Tk.fied, put it into the DISPLAY
     */

    Tk_Main(argc, argv, Tk_AppInit);
    exit(0);
}

int Tk_AppInit(Tcl_Interp *interp)
{
    int code;
    char *rcfilename;
    char *tcltkgrassbase;
    char *script = "/script/tcltkgrass.tcl";

    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    if (Tk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    /*
     * Specify a user-specific startup file to invoke if the application
     * is run interactively.  Typically the startup file is "~/.apprc"
     * where "app" is the name of the application.  If this line is deleted
     * then no user-specific startup file will be run under any conditions.
     */

    tcltkgrassbase = Tcl_GetVar(interp, "env(TCLTKGRASSBASE)", TCL_GLOBAL_ONLY);
    rcfilename = (char *) malloc(strlen(tcltkgrassbase) + strlen(script) + 1);
    strcpy(rcfilename, tcltkgrassbase);
    strcat(rcfilename, script);
    Tcl_SetVar(interp, "tcl_rcFileName", rcfilename, TCL_GLOBAL_ONLY);

    /*
     * Invoke the script specified on the command line, if any.
     */

    if (fileName != NULL) {
        code = Tcl_VarEval(interp, "source ", fileName, (char *) NULL);
        if (code != TCL_OK) {
            fprintf(stderr, "%s\n", Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY));
            Tcl_Eval(interp, "exit 1");
            exit(1);
        } else {
            Tcl_Eval(interp, "exit");
            exit(0);
        }
    }

    return TCL_OK;
}
