#ifndef lint
static char *SCCSID = "@(#)Dhp7475.c	OEMG v.3.2";
#endif
/* hp7475A - desktop plotter */
/* the following defines must be included for all drivers */
/* modified for multiple paper sizes */
#include <varargs.h>
#include "plotter.h"
#define file stdout
#define ifile stdin
# include <termio.h>
/*	select appropriate baud rate for your system */
# define BAUD B9600
#define OFFy 380
#define NUM_PEN 6

static char s[] = "\0";		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */
static OFFx;

	static int
oldpen;
	static char
*spen;
	static struct {
int	xlim,  ylim;
} lims[3] = {
        10365, 7962,            /* A paper */
	16640, 10365,		/* B paper */
	16640, 10365,		/* direct connect */
};
	XYS *
Dhp7475(va_alist) va_dcl {
	va_list vap;
	int cmd, i, pen=1, px1,py1,px2,py2;
	long	x, y, t;
	XYS *ret = &cursor;

	va_start(vap); cmd = va_arg(vap, int);
	switch(cmd) {
	case D_SCALE:
		if (Dglobal.scale <= 0.)
			Dglobal.scale = 1.;
		goto scaleit;
	case D_INIT:
		if ( Dglobal.model_no == 2 && !(isatty(fileno(file)) || isatty(fileno(file)))){
			fprintf(stderr,"Error: Paper size not specified\n");
			exit();
			}
		if (Dglobal.model_no == 0) OFFx = 360;
		else OFFx = 0;
		if (isatty(i = fileno(file))) { /* set baud rate */
			struct termio term;
			if (ioctl(i, TCGETA, &term) == -1)
				return NULL;
			term.c_iflag |= IXON+IXANY;
			term.c_cflag = (term.c_cflag & ~CBAUD) + BAUD;
			if (ioctl(i, TCSETA, &term)== -1)
				return NULL;
		}
		fputs("IN;\033.I81;;17:\033.N;19:SP1;PU;\n",file);
                fprintf(file,"IP;\n");
		oldpen = 1;
                if (Dglobal.model_no == 2) {
			struct termio term;
                        if (ioctl(i, TCGETA, &term) == -1)
                                return NULL;
                        term.c_iflag |= IXON+IXANY;
                        term.c_cflag = (term.c_cflag & ~CBAUD) + BAUD;
                        if (ioctl(i, TCSETA, &term)== -1)
                                return NULL;
                        fputs("OP;\n",file);
                        scanf("%d,%d,%d,%d",&px1,&py1,&px2,&py2);
                        lims[Dglobal.model_no].xlim = px2 - px1 +300;
                        lims[Dglobal.model_no].ylim = py2 - py1 +300;
                }
scaleit:
		if (Dglobal.reverse) {
			cursor.x = lims[Dglobal.model_no].ylim / Dglobal.scale;
			cursor.y = lims[Dglobal.model_no].xlim / Dglobal.scale;
		} else {
			cursor.x = lims[Dglobal.model_no].xlim / Dglobal.scale;
			cursor.y = lims[Dglobal.model_no].ylim / Dglobal.scale;
		}
		break;
	case D_DONE:
		fputs("SP;\n",file);
		fclose(file);
		break;
	case D_MOVE:
		spen = "PU";
	case D_LINE:
		x = (va_arg(vap, long)) * Dglobal.scale + OFFx;
		y = (va_arg(vap, long)) * Dglobal.scale + OFFy;
		if (Dglobal.reverse) {
			t = x;
			x = y;
			y = lims[Dglobal.model_no].ylim - t;
		}
		fprintf(file,"%s%d,%d;\n", spen, x, y);
		spen = "PD";
		break;
	case D_PEN:
		pen = (va_arg(vap, long)) & 7;
		while (pen > NUM_PEN) pen -= NUM_PEN;  /* only six pens */
		if (pen == 0) pen = 1;
		if (pen != oldpen) {
			oldpen = pen;
			fprintf(file,"SP%d;\n",pen);
		}
	}
	va_end(vap);
	return ret;
}
