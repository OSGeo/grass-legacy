/* 
 * TCLTKGRASS was revised 1998-1999 by Jacques Bouchard, France
 *               for
 *
 *      GRASS Development Team
 *      Baylor University, Texas
 *      http://www.baylor.edu/~grass/
 *
 * New versions you find there or at 
 * http://www.geog.uni-hannover.de/grass/
 *
 * The software is based on TCLTKGRASS developed 1994 by
 *
 * L.A.S.
 * Logiciels et Applications Scientifiques Inc.
 * Montreal, Canada
 * tel.: (514) 858-1104
 * fax: (514) 389-9373
 * email: gc@copernic.lasinc.qc.ca
 *
 *
 *
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
/* #include <tcl.h> */
#include <tk.h>
#include "gis.h"

/*
 * Command-line options:
 */

int synchronize = 0;
char *fileName = NULL;
char *name = NULL;
char *display = NULL;
char *geometry = NULL;

static Tk_ArgvInfo argTable[] = {
    {"-file", TK_ARGV_STRING, (char *) NULL, (char *) &fileName,
	"File from which to read commands"},
    {"-geometry", TK_ARGV_STRING, (char *) NULL, (char *) &geometry,
	"Initial geometry for window"},
    {"-display", TK_ARGV_STRING, (char *) NULL, (char *) &display,
	"Display to use"},
    {"-name", TK_ARGV_STRING, (char *) NULL, (char *) &name,
	"Name to use for application"},
    {"-sync", TK_ARGV_CONSTANT, (char *) 1, (char *) &synchronize,
	"Use synchronous mode for display server"},
    {(char *) NULL, TK_ARGV_END, (char *) NULL, (char *) NULL,
	(char *) NULL},
};

int Tk_AppInit(Tcl_Interp *interp);

int viewCmd(ClientData clientData,
	    Tcl_Interp *interp,
	    int argc, char *argv[]);

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

int main(int argc, char *argv[])
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
    Tk_Window main;

    if (Tcl_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    if (Tk_Init(interp) == TCL_ERROR) {
	return TCL_ERROR;
    }

    /*
     * Call Tcl_CreateCommand for application-specific commands, if
     * they weren't already created by the init procedures called above.
     */

    main = Tk_MainWindow(interp);
    Tcl_CreateCommand(interp, "View", viewCmd, (ClientData) main, NULL);

    /*
     * Specify a user-specific startup file to invoke if the application
     * is run interactively.  Typically the startup file is "~/.apprc"
     * where "app" is the name of the application.  If this line is deleted
     * then no user-specific startup file will be run under any conditions.
     */
    tcltkgrassbase = Tcl_GetVar(interp, "env(TCLTKGRASSBASE)", TCL_GLOBAL_ONLY);
    if (!tcltkgrassbase) {
      fprintf(stderr, "TCLTKGRASSBASE not set\n");
      exit (-1);
    }
    rcfilename = (char *) malloc( (strlen(tcltkgrassbase) + strlen(script) + 1) );
    if (!rcfilename) {
      fprintf(stderr, "memory error\n");
      exit (-1);
    }
    strcpy(rcfilename, tcltkgrassbase);
    strcat(rcfilename, script);
    Tcl_SetVar(interp, "tcl_rcFileName", rcfilename, TCL_GLOBAL_ONLY);

    /*
     * If a display was specified, put it into the DISPLAY
     * environment variable so that it will be available for
     * any sub-processes created by us.
     */

    if (display != NULL) {
        Tcl_SetVar2(interp, "env", "DISPLAY", display, TCL_GLOBAL_ONLY);
    }

    if (synchronize) {
        XSynchronize(Tk_Display(main), True);
    }

    /*
     * Set the "geometry" variable from the geometry
     * specified on the command line.
     * Set the geometry of the main window, if requested.
     */

    if (geometry != NULL) {
        Tcl_SetVar(interp, "geometry", geometry, TCL_GLOBAL_ONLY);
        code = Tcl_VarEval(interp, "wm geometry . ", geometry, (char *) NULL);
        if (code != TCL_OK) {
            fprintf(stderr, "%s\n", interp->result);
        }
    }

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
