#ifndef lint
static char *SCCSID = "@(#)plotreq.c	USGS v.4.1";
#endif
/* requests data from plotter and decodes the input meta-graphic stream.  */
#include "graphics.h"

extern BASE base;
	ANSWR *
plotreq(arg) int arg; {
	int cmd, type, plotin(), i, c;
	char *p;
	long strtov();

	plotout(P_REQ);		/* send request and 	*/
	plotout(arg);		/*  and link tag	*/
	pltflush();

	while ((cmd = plotin()) != P_ACK) { /* read until link acknowledge */
		type = cmd & (~_LBMASK);
		switch (type) {	/* decode response stream */
		case _COORD:
		case _COORD+_REL:
			cvtoxy(cmd, plotin, &base.answr.x, &base.answr.y);
			break;
		case _NOARG:
			break;
		case _STR:
			p = base.answr.str;
			for (i = 0; c = plotin(); i++)
				if (i < MAX_USTR)
					*p++ = c;
			*p = '\0';
			break;
		default: /* either LONG or BYTE */
			base.answr.code = strtov(cmd >> 5, plotin);
		}
		base.answr.cmd = cmd;
	}
	return(&base.answr);
}
