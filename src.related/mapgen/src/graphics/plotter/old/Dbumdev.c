#ifndef lint
static char *SCCSID = "@(#)Dbumdev.c	OEMG v.3.1";
#endif
/* Dbumdev - a dummy device which is given control
** if applications selects an invalid device
*/

#include "grerror.h"
#include "plotter.h"

extern int error;

	XYS *
Dbumdev(cmd) {
	if (cmd == D_INIT)
		fputs("invalid/uninstalled device selected\n", stderr);
	error = E_BADDEV;
	return ((XYS *)0);
}
