#define X11STUFF
#include <stdio.h>
#include <ctype.h>
#include <fcntl.h>

/* 
 * Standard Toolkit include files:
 */
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

/*
 * Public include files for widgets used in this file.
 */
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/AsciiText.h>
#include "xzoom.h"
#define butLABEL	0
#define butMARK		100
#define butZOOM		0
#define butPAN		100
#define butALABEL	101
#define butLINE		0
#define butPOLY		100

	static char
work_str[50],
*textaddr;
	static char * /* message and expand path name */
adjst_name(s) char *s; {
	static char name[250];
	char *p = name, *getenv();

	/* skip white */
	while (*s && isspace(*s)) ++s;
	if (*s == '~') {
		if (!s[1] || s[1] == '/') {
			if (!(p = getenv("HOME"))) {
				docomment("no HOME env param",
					(char *) 0,0);
				sensitivity(0);
				return((char *)0);
			}
			(void)strcpy(name, p);
			(void)strcat(name, ++s);
		} else {
			FILE *f, *popen();
			char req[100];
			
			(void)strcpy(req,"grep '^");
			p = req + strlen(req);
			while (*++s && *s != '/') *p++ = *s;
			(void)strcpy(p, ":' /etc/passwd | cut -d: -f6");
			if (!(f = popen(req,"r"))) {
				docomment("request problem", (char *) 0,1);
				sensitivity(0);
				return((char *)0);
			}
			if (fgets(name,100,f)) {
				(void)pclose(f);
				p = name + strlen(name);
				if (p != name && (*(p - 1) == '\n')) --p;
				(void)strcpy(p, s);
			} else {
				docomment("no such name", (char *) 0,0);
				sensitivity(0);
				return((char *)0);
			}
		}
		return (name);
	}
	return(s);
}
	static void
settext() {
	int i;
	Arg arg[2];
	i = 0;
	XtSetArg(arg[i], XtNstring, &textaddr); ++i;
	XtGetValues(Ltext, arg, i);
}
	void
sensitivity(mode) {
	int i, mask, tf;

	if (mode < 0) {
		for (i = 0; list[i].name ; ++i)
			XtSetSensitive(list[i].w, FALSE);
		return;
	}
	for (i = 0; list[i].name ; ++i) { /* this code stinks, but works */
		mask = list[i].mode;
		if (mode) {
			if (!(tf = sALWAYS & mask) && Zscaled)
				tf = (mask == sSCALE) || (digit_fid &&
					mask == sDIGIT);
		} else tf = sERROR & mask;
		XtSetSensitive(list[i].w, tf ? TRUE : FALSE);
	}
}
	static void
open_digit(mode) char *mode; {
	XtPopdown(popshell);
	if (!(digit_fid = fopen(digit_name, mode))) {
		docomment("digitizing file open failure",(char *)0,1);
		sensitivity(0);
	} else {
		if (!strcmp(mode, "w+"))
			fputs("-f - -s .3 -l .5\n", digit_fid);
		sensitivity(1);
	}
}
/*ARGSUSED*/
	void
Cend_overwrite(w, client, call) Widget w; caddr_t client, call; { 
	open_digit("w+");
}
/*ARGSUSED*/
	void
Cend_append(w, client, call) Widget w; caddr_t client, call; { 
	open_digit("a+");
}
/*ARGSUSED*/
	void
Cend_abortit(w, client, call) Widget w; caddr_t client, call; { 
	XtPopdown(popshell);
	sensitivity(1);
}
/*ARGSUSED*/
	void
Cend_dialog(w, client, call) Widget w; caddr_t client, call; { 
	char *s;

	if (!(s = adjst_name(XawDialogGetValueString(dialog))))
		return;
	switch (name_mode) {
	case nDIGIT:
		(void)strncpy(digit_name, s, FNAMEMAX);
		if (!access(digit_name, 0)) {
			XtSetSensitive(overwrite, TRUE);
			XtSetSensitive(appendit, TRUE);
			XtSetSensitive(dialogdone, FALSE);
		} else
			open_digit("w+");
		return;
	case nPLOT:
		XtPopdown(popshell);
		(void)strncpy(plotter_name, s, FNAMEMAX);
		break;
	case nDEF:
		XtPopdown(popshell);
		(void)strncpy(def_name, s, FNAMEMAX);
		if (!loader(def_name))
			return;
		break;
	case nCWD:
		XtPopdown(popshell);
		if (strcmp(s, dir_name)) {
			if (chdir(s)) {
				docomment("unknown directory",(char *)0,1);
				sensitivity(0);
				return;
			}
			(void)strcpy(dir_name, s);
		}
		break;
	}
	sensitivity(1);
}
/*ARGSUSED*/
	void
Cpen_out(w, client, call) Widget w; char *client; caddr_t call; { 
	Pixel back, fore;
	Arg args[2];

	(void)sscanf(client, "%d", &pen_out);
	XtSetArg(args[0], XtNbackground, &back);
	XtSetArg(args[1], XtNforeground, &fore);
	XtGetValues(w, args, 2);
	XtSetArg(args[0], XtNbackground, back);
	XtSetArg(args[1], XtNforeground, fore);
	XtSetValues(lltext, args, 2);
}
/*ARGSUSED*/
	void
Cerror(w, client, call) Widget w; caddr_t client, call; { 
	docomment((char *)0,(char *)0,0);
	sensitivity(1);
}
	void
runplot() {
	sensitivity(-1);
	clearT();
	if (doplot()) {
		sensitivity(0);
		return;
	}
	redrawT(Tablet, (XExposeEvent *)0);
	docomment((char *)0,(char *)0,0);
	sensitivity(1);
}
/*ARGSUSED*/
	void
Cplot(w, client, call) Widget w; caddr_t client, call; { runplot(); }
/*ARGSUSED*/
	void
Cquit(w, client, call) Widget w; caddr_t client, call; { exit(0); }
	static void
get_file(comment, file) char *comment, *file; {
	static String value_name = "value";
	Position x, y;
	Dimension width;
	Arg arg[4];
	int i;

	i = 0;
	XtSetArg(arg[i], XtNwidth, &width); ++i;
	XtGetValues(topLevel, arg, i);
	XtTranslateCoords(topLevel, (Position)50, (Position)50,
		&x, &y);
	i = 0;
	XtSetArg(arg[i], XtNwidth, width-50); ++i;
	XtSetArg(arg[i], XtNx, x); ++i;
	XtSetArg(arg[i], XtNy, y); ++i;
	XtSetValues(popshell, arg, i);
	i = 0;
	XtSetArg(arg[i], XtNlabel, comment); ++i;
	XtSetArg(arg[i], XtNvalue, file); ++i;
	XtSetValues(dialog, arg, i);
	sensitivity(-1);
	XtSetSensitive(overwrite, FALSE);
	XtSetSensitive(appendit, FALSE);
	XtSetSensitive(dialogdone, TRUE);
	XtPopup(popshell, XtGrabNonexclusive);
/*
	XtSetArg(args[0], XtNtranslations, dialogTr);
	XtSetValues(XtNameToWidget(dialog, value_name), arg, 1);
*/
	XtOverrideTranslations(XtNameToWidget(dialog, value_name), dialogTr);
}
/*ARGSUSED*/
	void
Cplotter(w, client, call) Widget w; caddr_t client, call; { 
	name_mode = nPLOT;
	get_file("plot control file name", plotter_name);
}
static int NO_write;	/* flag factor that plotter file not editable */
/*ARGSUSED*/
	void
Cedit(w, client, call) Widget w; caddr_t client, call; { 
	Position x, y;
	Dimension width;
	Arg arg[4];
	int i;

	if (!access(plotter_name, 0))
		NO_write = access(plotter_name, 2);
	else {
		if ((i = open(plotter_name, O_RDWR+O_CREAT, 0666)) == -1) {
			docomment("plot control open error",(char *)0,1);
			sensitivity(0);
			return;
		} else (void)close(i);
	}
	i = 0;
	XtSetArg(arg[i], XtNwidth, &width); ++i;
	XtGetValues(topLevel, arg, i);
	XtTranslateCoords(topLevel, (Position)50, (Position)50,
		&x, &y);
	i = 0;
	XtSetArg(arg[i], XtNx, x); ++i;
	XtSetArg(arg[i], XtNy, y); ++i;
	XtSetValues(popplot, arg, i);
	i = 0;
	XtSetArg(arg[i], XtNstring, plotter_name); ++i;
	XtSetArg(arg[i], XtNwidth, width-50); ++i;
	XtSetArg(arg[i], XtNeditType, NO_write ? XawtextRead :
		XawtextEdit); ++i;
	XtSetValues(plottext, arg, i);
	XtSetSensitive(plotabt, NO_write ? FALSE : TRUE);
	sensitivity(-1);
	XtPopup(popplot, XtGrabNonexclusive);
}
/*ARGSUSED*/
	void
CPlotdon(w, client, call) Widget w; char *client; caddr_t call; { 
	Arg arg[1];
	int ret = 1;
	Widget tmp;

	if (*client == 'D' && !NO_write) {
		XtSetArg(arg[0], XtNtextSource, &tmp);
		XtGetValues(plottext, arg, 1);
		if (XawAsciiSave(tmp) != True) {
			docomment("unable to write file",
				"probably write protected",0);
			ret = 0;
		}
	}
	XtPopdown(popplot);
	sensitivity(ret);
}
/*ARGSUSED*/
	void /* change working directory */
Ccwd(w, client, call) Widget w; caddr_t client, call; { 
	name_mode = nCWD;
	getcwd(dir_name, LENDIRNAME);
	get_file("new directory name", dir_name);
}
/*ARGSUSED*/
	void
Cdef(w, client, call) Widget w; caddr_t client, call; { 
	name_mode = nDEF;
	get_file("definition file name", def_name);
}
/*ARGSUSED*/
	void
Cdigit(w, client, call) Widget w; caddr_t client, call; { 
	if (digit_fid) {
		(void)fclose(digit_fid);
		digit_fid = (FILE *) 0;
	}
	name_mode = nDIGIT;
	get_file("digitizing output file", digit_name);
}
	static void
Bsingle(button, x, y) {
	static int mode;

	switch (button) {
	/* initialize */
	case butMARK:
	case butLABEL:
		mode = button;
		return;;
	case 1: /* button 1 */
		circleT(x, y);
		gr_xy(x, y);
		if (mode != butMARK) {
			(void)sprintf(work_str,"-r 0 -t -p %d",pen_out);
			gr_str(work_str);
			settext();
			gr_str(textaddr);
			gr_str(".");
		}
		return;
	default:
		break;
	}
	but_return = 0;
	docomment((char *)0,(char *)0,0);
	sensitivity(1);
}
	static void
Bdouble(button, x, y) {
	static int pt, mode;

	switch (button) {
	/* initialize */
	case butPAN:
	case butALABEL:
	case butZOOM:
		pt = 0;
		mode = button;
		return;;
	case 1: /* button 1 */
		if (++pt < 2) { /* got everything */
			base_rat_x = x;
			base_rat_y = y;
			next_rat = 0;
			rat_mode = mode != butZOOM ? RAT_LINE : RAT_BOX;
			if (mode == butALABEL)
				circleT(x, y);
			return;
		}
	default: /* button 2, or ... */
		ratLive(Tablet, (XButtonEvent *)0);
		rat_mode = RAT_OFF;
		if (mode != butALABEL)
			but_return = 0;
	}
	if (pt == 2)
		switch(mode) {
		case butZOOM:
			if(gr_zoom_pan(base_rat_x, base_rat_y, x, y, 1))
				return;
			runplot();
			break;
		case butPAN:
			if(gr_zoom_pan(base_rat_x, base_rat_y, x, y, 0))
				return;
			runplot();
			break;
		case butALABEL:
			gr_xy(base_rat_x, base_rat_y);
			gr_ang(base_rat_x, base_rat_y, x, y);
			(void)sprintf(work_str,"-t -p %d", pen_out);
			gr_str(work_str);
			settext();
			gr_str(textaddr);
			gr_str(".");
			pt = 0;
			return;
		}
	docomment((char *)0,(char *)0,0);
	sensitivity(1);
}
/*ARGSUSED*/
	void
Czoom(w, client, call) Widget w; caddr_t client, call; { 
	docomment("ZOOM 2 button one clicks at","bounding box corners",0);
	sensitivity(-1);
	Bdouble(0, 0, 0);
	but_return = Bdouble;
}
/*ARGSUSED*/
	void
Cdezoom(w, client, call) Widget w; caddr_t client, call; { 
	if (!gr_dezoom())
		runplot();
}
/*ARGSUSED*/
	void
Chome(w, client, call) Widget w; caddr_t client, call; { 
	if (!gr_home())
		runplot();
}
/*ARGSUSED*/
	void
Ctext(w, client, call) Widget w; caddr_t client, call; { 
	settext();
	gr_str(textaddr);
}
/*ARGSUSED*/
	void
Clabel(w, client, call) Widget w; caddr_t client, call; { 
	docomment("Select point with button 1, exit with 2 or 3",
		"Text from text window",0);
	sensitivity(-1);
	Bsingle(butLABEL, 0, 0);
	but_return = Bsingle;
}
/*ARGSUSED*/
	void
Cmark(w, client, call) Widget w; caddr_t client, call; { 
	docomment("Select point with button 1",
		"button 2-3 quits mode",0);
	sensitivity(-1);
	Bsingle(butMARK, 0, 0);
	but_return = Bsingle;
}
/*ARGSUSED*/
	void
Calabel(w, client, call) Widget w; caddr_t client, call; { 
	docomment("Select base pt. with button 1",
		"Angle determined by line from pt 1",0);
	sensitivity(-1);
	Bdouble(butALABEL, 0, 0);
	but_return = Bdouble;
}
/*ARGSUSED*/
	void
Cpan(w, client, call) Widget w; caddr_t client, call; { 
	docomment("Panning operation, button 1 on current loc",
		"2nd button 1 left on new location, button 2 or 3 cancels",0);
	sensitivity(-1);
	Bdouble(butPAN, 0, 0);
	but_return = Bdouble;
}
	static int
begin_x, begin_y;
	static void
Bline(button, x, y) {
	static int pt, close_it;

	switch (button) {
	case butLINE:	/* initialize */
	case butPOLY:
		pt = 0;
		close_it = button == butPOLY;
		return;
	case 1: /* button 1 */
		if (++pt < 2) { /* got everything */
			begin_x = base_rat_x = x;
			begin_y = base_rat_y = y;
			(void)sprintf(work_str,"-L %d",pen_out);
			gr_str(work_str);
			gr_coord(x,y);
			next_rat = 0;
			rat_mode = RAT_LINE;
		} else {
			gr_coord(x,y);
			ratLive(Tablet, (XButtonEvent *)0);
			next_rat = 0;
			lineT(base_rat_x, base_rat_y, x, y);
			base_rat_x = x;
			base_rat_y = y;
		}
		return;
	default: /* button 2, or ... */
		ratLive(Tablet, (XButtonEvent *)0);
		rat_mode = RAT_OFF;
		if (pt > 2 && close_it) {
			lineT(base_rat_x, base_rat_y, begin_x, begin_y);
			gr_coord(begin_x,begin_y);
		}
		if (pt) gr_str(".");
		if (button != 3) {
			pt = 0;
			return;
		}
		but_return = 0;
	}
	docomment((char *)0,(char *)0,0);
	sensitivity(1);
}
/*ARGSUSED*/
	void
Cline(w, client, call) Widget w; caddr_t client, call; { 
	docomment("Line operation, button 1 on each coordinate",
		"button 2 breaks line, 3 quits operation",0);
	sensitivity(-1);
	Bline(0, 0, 0);
	but_return = Bline;
}
/*ARGSUSED*/
	void
Cpoly(w, client, call) Widget w; caddr_t client, call; { 
	docomment("Poly line operation, button 1 on each coordinate",
		"button 2 ends poly, 3 quits operation",0);
	sensitivity(-1);
	Bline(100, 0, 0);
	but_return = Bline;
}
