/* entries from X11 which are to be converted to output */
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <math.h>
#include <varargs.h>

#include <plotgen.h>
#include <mapgen.h>
#define EXTDEV
#include <graphics.h>

#include "xzoom.h"

	struct map_def
m_def;
	double
cts;
	struct PLTDEF
p_def;
	static struct PLTDEF
null_p_def;

# define MAX_DEPTH 20
	long
x_board[MAX_DEPTH], y_board[MAX_DEPTH],
x_base[MAX_DEPTH], y_base[MAX_DEPTH];
	int
zoom_depth;
	char
overlay[200];

	static char
legendov[30];

	static double
scale, bscale;

# define MAX_LINE 150
# define MAXC 30
	void
setplot() { } /* don't need its services */
	static void
setscale() {
	double fx, fy;

	fx = ((double)xTablet) / x_board[zoom_depth];
	fy = ((double)yTablet) / y_board[zoom_depth];
	if (fx > fy) fx = fy;
	bscale = 1. / fx;
	scale = 1. / cts;
	sprintf(annot_L1, "size: %.1f %.1f, off: %.1f %.1f",
		x_board[zoom_depth]*scale,
		y_board[zoom_depth]*scale,
		x_base[zoom_depth]*scale,
		y_base[zoom_depth]*scale);
	sprintf(annot_L2, "Zm lev: %d, scale: 1:%g", zoom_depth,
		xpix_per_cm * bscale * scale);
	docomment((char*)0,(char*)0,0);
}
	static char
emess_line[200];
	void
emess(va_alist) va_dcl {
	va_list args;
	char *fmt;

	va_start(args);
	fmt = va_arg(args, char *);
	vsprintf(emess_line,fmt,args);
	va_end(args);
}
	static
loaddef(fid) FILE *fid; {
	char line[MAX_LINE], *s, *argv[MAXC], *getline();
	int scalein();

	while (s = getline(line, MAX_LINE, fid)) {
		if (*s == '#') ++s;
		if (! words(s, MAXC, argv, scalein))
			return(1);
	}
	*annot_L1 = *annot_L2 = '\0';
	docomment("failed to PLOTGEN scale",emess_line,0);
	sensitivity(0);
	return(0);
}
	int
loader(master) char *master; {
	FILE *fid;
	long tag;

	Zscaled = 0;
	zoom_depth = 0;
	if (!(fid = fopen(master, "r"))) {
		docomment("master opening error",(char *)0,1);
		sensitivity(0);
		return(0);
	}
	if (fread((char *)&tag, sizeof(long), 1, fid) != 1) {
		docomment("MAPGEN load error",(char *)0,1);
		sensitivity(0);
		return(0);
	}
	rewind(fid);
	p_def = null_p_def;
	if (tag == MAGIC) {	/* MAPGEN */
		if (fread((char *)&m_def, sizeof(m_def), 1, fid) != 1) {
			docomment("mapdef load length error",(char *)0,1);
			sensitivity(0);
			return(0);
		}
		x_board[zoom_depth] = m_def.B.x_max;
		y_board[zoom_depth] = m_def.B.y_max;
		cts = m_def.cts_cm;
	} else if (loaddef(fid)) {	/* PLOTGEN */
		x_board[zoom_depth] = p_def.x.board;
		y_board[zoom_depth] = p_def.y.board;
		cts = p_def.cts;
	} else {
		(void)fclose(fid);
		return(0);
	}
	(void)fclose(fid);
	setscale();
	Zscaled = 1;
	return(1);
}
	void
gr_str(s) char *s; { (void)fprintf(digit_fid,"%s\n", s); }
	void
gr_coord(x, y) {
	double dx, dy;
	dx = x * bscale + x_base[zoom_depth];
	dy = (yTablet - y) * bscale + y_base[zoom_depth];
	(void)fprintf(digit_fid,"%.2f\t%.2f\n", dx*scale, dy*scale);
}
	void
gr_xy(x, y) {
	double dx, dy;
	dx = x * bscale + x_base[zoom_depth];
	dy = (yTablet - y) * bscale + y_base[zoom_depth];
	(void)fprintf(digit_fid,"-x %.2f -y %.2f\n", dx*scale, dy*scale);
}
	void
gr_ang(x1, y1, x2, y2) {
	double dx, dy;
	dx = x2 - x1;
	dy = y1 - y2;
	(void)fprintf(digit_fid,"-r %.4f\n",
		(dx || dy) ? atan2(dy,dx) * 57.29578 : 0.);
}
	int
gr_zoom_pan(x1, y1, x2, y2, zoom) {
	double xm = x1, ym = yTablet - y1, xt = x2, yt = yTablet - y2;
	int xl, yl, xh, yh, last;

	if (!zoom && zoom_depth == 0) {
		docomment("cannot pan at home zoom level",(char *)0,0);
		sensitivity(0);
		return(1);
	}
	last = zoom_depth;
	if (zoom && ++zoom_depth >= MAX_DEPTH) {
		zoom_depth = last;
		docomment("at maximum zoom level",(char *)0,0);
		sensitivity(0);
		return(1);
	}
	if (zoom && xm < xt) {
		xl = bscale * xm + x_base[last];
		xh = bscale * xt + x_base[last];
	} else {
		xl = bscale * xt + x_base[last];
		xh = bscale * xm + x_base[last];
	}
	if (zoom && ym < yt) {
		yl = bscale * ym + y_base[last];
		yh = bscale * yt + y_base[last];
	} else {
		yl = bscale * yt + y_base[last];
		yh = bscale * ym + y_base[last];
	}
	if ((zoom && (xl == xh || yl == yh)) ||
		(!zoom && (xl == xh && yl == yh)) ) {
		zoom_depth = last;
		docomment("zero window size or pan motion",(char *)0,0);
		sensitivity(0);
		return(1);
	}
	if (zoom) {
		x_base[zoom_depth] = xl;
		y_base[zoom_depth] = yl;
		x_board[zoom_depth] = xh - xl;
		y_board[zoom_depth] = yh - yl;
	} else {
		if ((x_base[zoom_depth] += (xh - xl)) < 0)
			x_base[zoom_depth] = 0;
		if ((y_base[zoom_depth] += (yh - yl)) < 0)
			y_base[zoom_depth] = 0;
	}
	setscale();
	return(0);
}
	int
gr_dezoom() {
	if (zoom_depth > 0) {
		--zoom_depth;
		setscale();
		return(0);
	}
	return(1);
}
	int
gr_home() {
	if (zoom_depth > 0) {
		zoom_depth = 0;
		setscale();
		return(0);
	}
	return(1);
}
#define IN(x) if ((x = getc(infile)) == EOF) return(1)
#define MAXXY 1000
static struct {short x, y; } xy[MAXXY];
	static int
splot(infile) FILE *infile; {
	int pen, nxy, c, d, x, y;

	while ((c = getc(infile)) != EOF)
		switch (c) {
		case _BOP:
			nxy = 0;
			break;
		case _EOP:
			if (nxy > 1) {
				plinesT(nxy, pen, xy);
				nxy = 0;
			}
			return(0); /* normal completion */
		case _PEN:
#ifndef PRE4
		case _PENL:
#endif
			if (nxy) {
				plinesT(nxy, pen, xy);
				nxy = 0;
			}
#ifdef PRE4
			IN(d); pen = d << 8;
			IN(d); pen += d;
#else
			IN(pen);
                        if (c == _PENL) {
                                IN(d); pen = (pen << 8) + d;
                                IN(d); pen = (pen << 8) + d;
                        }
#endif
			pen %= MAXPENS;
			break;
		case _MOVE:
			if (nxy > 1) {
				plinesT(nxy, pen, xy);
				nxy = 0;
			}
			goto load;
		case _DRAW:
			if (nxy >= MAXXY) {
				plinesT(nxy, pen, xy);
				xy[0] = xy[nxy-1];
				nxy = 1;
			}
load:
			IN(d); x = d << 8;
			IN(d); xy[nxy].x = x + d;
			IN(d); y = d << 8;
			IN(d); xy[nxy].y = yTablet - (y + d);
			++nxy;
			break;
#ifndef PRE4
		case _SPCL:
			IN(d);
			while(d) IN(d);
			break;
#endif
		default:
			return(2);
		}
	return(3);
}
	int
doplot() {
	int i;
	FILE *infile, *popen();

	if (digit_fid) {
		docomment("doing digit overlay","wait",0);
		(void)fflush(digit_fid);
		(void)strcpy(legendov, digit_name);
		(void)strcat(legendov, ".ov.tmp");
		(void)sprintf(overlay,"legend %s %s -o %s",
			def_name,digit_name,legendov);
		(void)system(overlay);
	} else legendov[0] = '\0';
	setscale();
	(void)sprintf(plot_string,
	"plotter </dev/null -d sunzoom -s %g -X -%ld -Y -%ld -DR%d,%d %s ",
		1./bscale, x_base[zoom_depth], y_base[zoom_depth],
		(int)xTablet,(int)yTablet, legendov);
	if (*plotter_name)
		if (infile = fopen(plotter_name,"r")) {
			int c, cl, skip;
			char *s;
	
			i = strlen(plot_string);
			s = plot_string + i;
			i = MAXPLOTSTR - i - 1;
			skip = 0;
			cl = '\n';
			while ( (i > 0) && ((c = getc(infile)) >= 0) ) {
				if (c == '\n') {
					if (skip) skip = 0;
					else if (cl != ' ') {
						*s++ = ' ';
						--i;
					}
				} else if (c == '#') {
					skip = 1;
				} else if (!skip && !(c == ' ' && cl == ' ')) {
					*s++ = c;
					--i;
				}
				cl = c;
			}
			*s = '\0';
			if (i <= 0) {
				docomment("command line file too long",
					"plot aborted",0);
				return(1);
			} else
				(void)fclose(infile);
		} else {
			docomment("cannot open plotter file",(char *)0,1);
			return(1);
		}
	else if (! *legendov) {
		docomment("nothing to plot",(char *)0,0);
		return(1);
	}
	docomment("plotting overlays","wait",0);
	if (!(infile = popen(plot_string, "r"))) {
		docomment("plot open failure",(char *)0,1);
		return(1);
	} else {
		if (i = splot(infile))
			docomment("corrupt graphic file",(char *)0,0);
		(void)pclose(infile);
	}
	if (*legendov)
		(void)unlink(legendov);
	return(i);
}
