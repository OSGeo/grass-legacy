/*This is a pseudo generic HPGL driver. The scale factors (FX FY) and
* the limits (lims[]) wher designed for the HP Draftmaster plotter
*          M.L.Holko                 4/4/91
*/
#ifndef lint
static char *SCCSID = "@(#)Ddmp60.c	OEMG v.3.2";
#endif
/* the following defines must be included for all drivers */
/* modified for multiple paper sizes */
#include <varargs.h>
#include <termio.h>
#include <math.h>
#include "plotter.h"
#define file stdout
#define ifile stdin
# define BAUD B9600
#define FX .99899
#define FY .99576


static char s[] = "\0";		/* required structure for return */
static XYS cursor = {0, 0, s};	/* data.  "s" may be larger, if needed */
/*	select appropriate baud rate for your system */
	static int
oldpen;
	static char
*spen;
	static struct {
int	xlim,  ylim;
} lims[6] = {
        9800, 6700,             /* A paper */
        15700, 9700,           /* B paper */
        20900, 14700,           /* C paper */
        33000, 20000,           /* D paper */
        43000, 33000,           /* E paper */
        43000, 33000,           /* direct connect */
};
	XYS *
Ddmp60(va_alist) va_dcl {
	va_list vap;
	int cmd, i, pen=1, px1,py1,px2,py2;
	double Dx, Dy, Dt;
	long	x, y, t;
	XYS *ret = &cursor;

	va_start(vap); cmd = va_arg(vap, int);
	switch(cmd) {
	case D_SCALE:
		if (Dglobal.scale <= 0.)
			Dglobal.scale = 1.;
		goto scaleit;
	case D_INIT:
                if ( Dglobal.model_no == 5 && !(isatty(fileno(file)) || isatty(fileno(file)))){
                        fprintf(stderr,"Error: Paper size not specified\n");
                        exit();
                        }
		if (isatty(i = fileno(file))) { /* set baud rate */
			struct termio term;
			if (ioctl(i, TCGETA, &term) == -1)
				return NULL;
			term.c_iflag |= IXON+IXANY;
			term.c_cflag = (term.c_cflag & ~CBAUD) + BAUD;
			if (ioctl(i, TCSETA, &term)== -1)
				return NULL;
		}
		fputs("\033.I81;;17:\033.N;19:\nIN;\nSP1;PU;\n",file);
		fprintf(file,"IP;\n");
		oldpen = 1;
		if (Dglobal.model_no == 5) {
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
		goto moveit;
	case D_LINE:
		spen = "PD";
moveit:
		Dx = (va_arg(vap, long)) * Dglobal.scale;
		Dy = (va_arg(vap, long)) * Dglobal.scale;

		if (Dglobal.reverse) {
			Dt = Dx;
			Dx = -Dy + (lims[Dglobal.model_no].ylim / 2.0);
			Dy = Dt - (lims[Dglobal.model_no].xlim / 2.0);
			}
		else {
			Dx = Dx - (lims[Dglobal.model_no].xlim / 2.0);
			Dy = Dy - (lims[Dglobal.model_no].ylim / 2.0);
			}
		x = floor(Dx * FX);
		y = floor(Dy * FY);
		fprintf(file,"%s%d,%d;\n", spen, -x, -y);
		break;
	case D_PEN:
		pen = (va_arg(vap, long));
		/*pen %= 8;   only 8 pens */
		while (pen > 8) pen = pen - 8;
		if (pen == 0) pen = 1;
		if (pen != oldpen) {
			oldpen = pen;
			fprintf(file,"SP%d;PU;\n",pen);
		}
	}
	va_end(vap);
	return ret;
}
