/* xzoom header file */
/*
** these commands are in command.c
*/
/* maximum size of file names */
#define FNAMEMAX 100
#define MAXPLOTSTR 2000
#define MAXPENS 10

char digit_name[FNAMEMAX]
#ifdef XZOOMMAIN
= {"digit"}
#endif
;
FILE *digit_fid;

char def_name[FNAMEMAX]
#ifdef XZOOMMAIN
= {"map.def"}
#endif
;

char plotter_name[FNAMEMAX]
#ifdef XZOOMMAIN
= {"plot"}
#endif
;
char plot_string[MAXPLOTSTR];
#define nDIGIT	1
#define nDEF	2
#define nPLOT	3
#define nCWD	4
#define LENDIRNAME 200
	char
*dir_name[LENDIRNAME+2];
int name_mode;
	int
Zscaled;	/* def read and ready */
	double
xpix_per_cm, ypix_per_cm;
	char
annot_L1[100], annot_L2[100];

void Cquit(),Czoom(),Cpan(),Clabel(),Calabel(), Cline(), Cpoly(), Ctext(),
	Cplot(), Cerror(), Cmark(), Cdef(), Cdigit(), Cplotter(), Cpen_out(),
	Chome(), Cdezoom(), Ccwd(), Cend_dialog(), CPlotdon(), Cedit(),
	Cend_overwrite(), Cend_append(), Cend_abortit(), sensitivity(),
	clearT(), redrawT(), circleT(), lineT(),
	docomment(), runplot(), gr_str(), gr_coord(), gr_xy(), gr_ang();
	int
loader(), gr_zoom_pan(), gr_dezoom(), gr_home(), doplot();
	int
pen_out,
next_rat,
base_rat_x,
base_rat_y,
next_rat_x,
next_rat_y;
	unsigned int
xTablet
#ifdef XZOOMMAIN
= 600
#endif
,
yTablet
#ifdef XZOOMMAIN
= 450
#endif
;

/* following stuff only for routines using X11 */
#ifdef X11STUFF
#define sSCALE	0x01
#define sDIGIT	0x02
#define sALWAYS	0x04
#define sERROR	0x08
	Widget
lltext,		/* pen label */
topLevel,	/* main widget */
Comment,	/* comment area */
Ltext,		/* label text area */
Tablet;		/* drafting area */
Widget textlabel, popshell, dialog, dialogdone, overwrite, appendit,
	abortit, popplot, plotbox, plotdon, plotabt, plottext;
	int
yComment
#ifdef XZOOMMAIN
= 30
#endif
;	/* x same as Tablet */
	XtTranslations
dialogTr;
	unsigned int
depth;

struct {	/* list of basic command widgets */
	char	*name;
	Widget	w;
	void	(*c)();
	int	mode;
} list[]
#ifdef XZOOMMAIN
 = {
	"RESET",	NULL, Cerror,	sERROR,
	"quit",		NULL, Cquit,	sALWAYS,
	"cwd",		NULL, Ccwd,	sALWAYS,
	"def",		NULL, Cdef,	sALWAYS,
	"(re)plot",	NULL, Cplot,	sSCALE,
	"home",		NULL, Chome,	sSCALE,
	"zoom",		NULL, Czoom,	sSCALE,
	"dezoom",	NULL, Cdezoom,	sSCALE,
	"pan",		NULL, Cpan,	sSCALE,
	"digit",	NULL, Cdigit,	sALWAYS,
	"label",	NULL, Clabel,	sDIGIT,
	"< label",	NULL, Calabel,	sDIGIT,
	"line",		NULL, Cline,	sDIGIT,
	"poly",		NULL, Cpoly,	sDIGIT,
	"mark",		NULL, Cmark,	sDIGIT,
	"text out",	NULL, Ctext,	sDIGIT,
	"plotter",	NULL, Cplotter,	sALWAYS,
	"edit pl",	NULL, Cedit,	sALWAYS,
	(char*)0
}
#endif
;

Pixmap picture;
GC draw, cdraw, dr_gc, pcopy, pclear;

/* pens for plotter drafting */
GC pens[MAXPENS];

void (*but_return)()	/* return after disabling commands */
#ifdef XZOOMMAIN
= 0
#endif
;
#define RAT_LINE 1
#define RAT_BOX 2
#define RAT_OFF 0
int rat_mode
#ifdef XZOOMMAIN
= RAT_OFF
#endif
;
#endif
/* end of X11 stuff */
