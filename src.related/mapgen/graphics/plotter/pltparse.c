#ifndef lint
static char *SCCSID = "@(#)pltparse.c	USGS v.4.5";
#endif
/* pltparse	*/
# define PLOTTER
# include "graphics.h"
# include "plotter.h"

# define NO_PEN if (!pen) return;
# define BMASK 0xffffff

extern XY *(*device)();

extern BASE base;

extern long base_x, base_y;

extern int solid(), dash(), fline();

extern PEN *pen;

extern int error;
	extern unsigned
pmaplist[256];

#define MAXSTRING 256

static void position(), noarg(), string(), longbyte();

pltparse() {
	int cmd, type, metain();
	long xv, yv, strtov();
	char *mstring();

	while ((cmd = metacmd()) != EOF) {
		type = cmd & (~_LBMASK);
		switch (type) {
		case _COORD:
		case _COORD+_REL:
			cvtoxy(cmd, metain, &xv, &yv);
			position(cmd, xv, yv);
			break;
		case _NOARG:
			noarg(cmd & _LBMASK);
			break;
		case _STR:
			base.text = mstring();
			string(cmd & _LBMASK, base.text);
			break;
		default: /* either LONG or BYTE */
			xv = strtov(cmd >> 5, metain);
			longbyte(cmd & _LBMASK, xv);
		}
	}

}
static int force, tol = 1;
static float cx0, cx1, cx2, cx3, cy0, cy1, cy2, cy3;
static XY xyl;
# define labs(x) ((x) < 0 ? -(x) : (x))
	static void
rinterp(xy0, xy1, t, tf) XY xy0, xy1; double t, tf; {
	XY xym;
	long xm, ym;
	float tm;

	tm = t + tf;
	xym.x = cx0+tm*(cx1+tm*(cx2+tm*cx3))+.5;	/* determine cubic */
	xym.y = cy0+tm*(cy1+tm*(cy2+tm*cy3))+.5;	/* values and delta */
	xm = (xy0.x + xy1.x + 1) / 2 - xym.x;	/* rounded mean assuming */
	ym = (xy0.y + xy1.y + 1) / 2 - xym.y;	/* positive values */
	if  (force || labs(xm) > tol ||  labs(ym) > tol) {
		force = 0;	/* got here because of 'force' or large */
		tf *= .5;	/* variance between mean and cubic */
		rinterp(xy0, xym, t, tf);	/* do it again until */
		rinterp(xym, xy1, tm, tf);	/* we drop to tol */
	} else { /* plot point */
		clip(~pen->pendown, xyl.x, xyl.y, xy1.x, xy1.y);
		pen->pendown = ~0;
		xyl = xy1;
	}
}
	static void
position(cmd, x, y) int cmd; long x, y; {
	base.x = (x += base.x);
	base.y = (y += base.y);
	NO_PEN;
	if (!pen->bezierm) {
		if (!(cmd & _PENUP)) {
			clip(~pen->pendown, X, Y, x, y);
			pen->pendown = ~0;
		}
		else
			pen->pendown = 0;
		X = x;
		Y = y;
		if (pen->sym) {
			symbol();
			pen->pendown = 0;
		}
	} else {
		if (cmd & _PENUP)
			pen->pendown = pen->bezindx = 0;
		else if (pen->bezindx == 3) {
			static XY xyn;

			cx0 = xyl.x = pen->xy->x[0];
			cx1 = 3. * (pen->xy->x[1]- cx0);
			cx2 = 3. * (pen->xy->x[2]- pen->xy->x[1]) - cx1;
			cx3 = x - cx0 - cx1 - cx2;
			cy0 = xyl.y = pen->xy->y[0];
			cy1 = 3. * (pen->xy->y[1]- cy0);
			cy2 = 3. * (pen->xy->y[2]- pen->xy->y[1]) - cy1;
			cy3 = y - cy0 - cy1 - cy2;
			force = 1;
			xyn.x = x;
			xyn.y = y;
			rinterp(xyl, xyn, 0., .5);
			pen->bezindx = 0;
		}
		X = x;
		Y = y;
		if (!pen->bezindx) {
			if (pen->sym)
				symbol();
			pen->pendown = 0;
		}
		++pen->bezindx;
	}
}
	static void
noarg(cmd) int cmd; {
	switch (cmd) {
	caseof(DISABLE):	/* disable driver */
		(*device)(D_DISABL);
		break;
	caseof(DASH):		/* draft dashed lines */
		NO_PEN;
		if (pen->damax > 0) {
			pen->line = dash;
			pen->dresid = pen->darray[0];
			pen->dindex = 0;
			pen->dsline = 0;
		}
		break;
	caseof(SOLID):		/* draft solid lines */
		NO_PEN;
		pen->line = solid;
		pen->dsline = 0;
		break;
	caseof(DELINK):		/* unlink x-y connection */
		NO_PEN;
		delink();
		break;
	caseof(DELPEN):		/* remove logical pen */
		NO_PEN;
		delpen();
		NO_PEN;
		break;
	caseof(JLEFT):		/* set left justification */
		NO_PEN;
		pen->cflags &= ~(C_RIGHT + C_CENTER);
		return;
	caseof(JRIGHT):		/* right justification */
		NO_PEN;
		pen->cflags &= ~C_CENTER;
		pen->cflags |= C_RIGHT;
		return;
	caseof(CENTER):		/* center strings */
		NO_PEN;
		pen->cflags &= ~C_RIGHT;
		pen->cflags |= C_CENTER;
		return;
	caseof(ERASE):		/* erase screen */
		(*device)(D_ERASE);
		return;
	caseof(CBASE):		/* clear base.x/y */
		base.x = base_x;
		base.y = base_y;
		return;
	caseof(BEZIER):		/* turn on Bezier line mode */
		NO_PEN;
		pen->bezierm = 1;
		return;
	caseof(BEZIERN):	/* turn off Bezier line mode */
		NO_PEN;
		pen->bezindx = pen->bezierm = 0;
		return;
	caseof(FPLOTN):		/* select fancy line mode, no line */
		NO_PEN;
		pen->f_pline &= ~_FLINE;
		goto fplotc;
	caseof(FPLOT):		/* select fancy line mode */
		NO_PEN;
		pen->f_pline |= _FLINE;
fplotc:
		if (pen->f_nosym && !pen->dsline) {
			pen->dsline = pen->line;
			pen->line = fline;
		}
		break;
	default:
		error = E_OPC;
	}
	pen->pendown = 0;
	(*device)(D_PEN, pen->mpen < 256 ? (long)pmaplist[pen->mpen] :
		pen->mpen);
}
	static void
string(cmd, s) int cmd; char *s; {
	FONT *setfont();
	XY *p;
	double atof();

	switch (cmd) {
	caseof(TEXT):		/* plot text */
		NO_PEN;
		post(s);
		break;
	caseof(LINKXY):		/* link x-y to another pen */
		NO_PEN;
		linkxy(s);
		break;
	caseof(NEWPEN):		/* create new pen */
		if (newpen(s)) return;
		goto setmech;
	caseof(SPEN):		/* select existing pen */
		NO_PEN;
		if (setpen(s)) return;
setmech:
		(*device)(D_PEN, pen->mpen < 256 ? (long)pmaplist[pen->mpen] :
			pen->mpen);
		break;
	caseof(SFONT):		/* set main character font */
		NO_PEN;
		pen->font = setfont(s);
		return;
	caseof(SFONTA):		/* set alternate character font */
		NO_PEN;
		pen->afont = setfont(s);
		return;
	caseof(SFONTS):		/* set symbol font */
		NO_PEN;
		pen->sfont = setfont(s);
		return;
	caseof(RESCALE):	/* recale plotter */
		if (pen) error = E_RESCL;
		else {
			Dglobal.scale = atof(s);
			p = (*device)(D_SCALE);
			windinit((long)(p->x),(long)(p->y));
		}
		return;
	caseof(FSYMS):		/* fancy line symbol list */
		NO_PEN;
		flset(s);
		return;
	caseof(SPECIAL):	/* device dependent string */
		(void)(*device)(D_STRING, s);
		return;
	default:
		error = E_OPC;
	}
		/* reset line pen control for break */
	pen->pendown = 0;
}
	static void
longbyte(cmd, v) int cmd; long v; {
	switch (cmd) {
	caseof(WXL):		/* set windows limits */
	caseof(WXH):
	caseof(WYL):
	caseof(WYH):
		NO_PEN;
		window(cmd, v);
		break;
	caseof(LEAD):		/* set line leading */
		NO_PEN;
		pen->leading = v;
		break;
	caseof(SIZE):		/* character string attributes */
	caseof(ANG):
	caseof(XOFF):
	caseof(YOFF):
		NO_PEN;
		setstrsz(cmd, v);
		break;
	caseof(MPEN):		/* mechanical pen attribute */
		NO_PEN;
		pen->mpen = v & BMASK;
		pen->pendown = 1;
		(*device)(D_PEN, pen->mpen < 256 ? (long)pmaplist[pen->mpen] :
			pen->mpen);
		break;
	caseof(DMASK):		/* dash line mask */
		NO_PEN;
		pen->damax = setdmask(v);
		goto checkdash;
	caseof(DSIZE):		/* dash element size */
		NO_PEN;
		pen->damax = setdsize(v);
checkdash:
		if (pen->damax <= 0)
			pen->line = solid;
		pen->dindex = pen->dresid = 0;
		pen->pendown = 1;
		break;
	caseof(SYM):		/* symbol posting */
		NO_PEN;
		pen->sym = v & 0x7f;
		break;
	caseof(SANG):		/* symbol attributes */
	caseof(SSIZE):
		NO_PEN;
		setsymsz(cmd, v);
		break;
	caseof(P_REQ):		/* user request */
		urequest(v);
		break;
	caseof(BASEX):		/* set x base register */
		base_x = v;
		break;
	caseof(BASEY):		/* set y base register */
		base_y = v;
		break;
	caseof(F_SIZE):		/* set fancy line char size */
		NO_PEN;
		pen->f_size = v > 0 ? v << 4 : -v;
		break;
	caseof(F_DIST):		/* set fancy line inter-sym distance */
		NO_PEN;
		if (v >= 0)
			pen->f_dist = v;
		break;
	default:
		error = E_OPC;
	}
}
