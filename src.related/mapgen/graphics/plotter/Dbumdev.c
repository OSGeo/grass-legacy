#ifndef lint
static char *SCCSID = "@(#)Dbumdev.c	USGS v.4.1";
#endif
/* Dbumdev - a dummy device which is given control
** if applications selects an invalid device
*/
#define PLOTTER
#include "plotter.h"
#include "graphics.h"

extern int error;

	XYS *
Dbumdev(cmd) {
	if (cmd == D_INIT)
		fputs("invalid/uninstalled device selected\n", stderr);
	error = E_BADDEV;
	return ((XYS *)0);
}
