/*
 * This is basically tkAppInit.c from the tk4.0 distribution except
 * that we define Tcl_AppInit in tkAppInit.c.
 */

#include "tk.h"

/*
 * The following variable is a special hack that is needed in order for
 * Sun shared libraries to be used for Tcl.
 */

extern int matherr();
/*int *tclDummyMathPtr = (int *) matherr;*/


extern int NVIZ_AppInit(Tcl_Interp*);


/*
 *----------------------------------------------------------------------
 *
 * main --
 *
 *	This is the main program for the application.
 *
 * Results:
 *	None: Tk_Main never returns here, so this procedure never
 *	returns either.
 *
 * Side effects:
 *	Whatever the application does.
 *
 *----------------------------------------------------------------------
 */

int 
main (
    int argc,			/* Number of command-line arguments. */
    char **argv		/* Values of command-line arguments. */
)
{
    Tk_Main(argc, argv, NVIZ_AppInit);
    return 0;			/* Needed only to prevent compiler warning. */
}
