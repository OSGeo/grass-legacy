#ifndef lint
static char *SCCSID = "@(#)fence.c	AMG v.3.1";
#endif
/* generate fence overlay */

# include <stdio.h>
# include <string.h>
# include "graphics.h"
# include "mapgen.h"

char	*overlay,	/* overlay file name */
	*master,	/* map master file name */
	*cntrl;	/* string of run line control */

int	inter;		/* if set, do terminal graphics */

struct map_def def;

extern int draft();

	static long
lrnd(x) double x; { return (x < 0. ? x - .5 : x + .5); }

main(argc, argv) char **argv; {
	extern	datapt();
	char	*arg;
	FILE	*fid;
	char	**eargv = argv;
	int	eargc = 0;

	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0':
				if (arg[-1] == '-')
					eargv[eargc++] = "-";
				break;

			/* interactive display */
			case 'i': inter = 1;
				continue;

			/* set overlay file */
			case 'o': if(--argc > 0) overlay = *++argv;
				continue;

			/* map master file */
			case 'm': if (--argc > 0) master = *++argv;
				continue;

			/* line control data */
			case 'c': if (--argc > 0) cntrl = *++argv;
				continue;
				
			default:
				fprintf(stderr,
					"invalid option %c\n", *arg);
				break;
			}
			break;
		}
		else
			eargv[eargc++] = *argv;
	}
	if (! master)
		if (eargc) { /* first file */
			--eargc;
			master = *eargv++;
		} else emess(1,"no map master given",(char *)0);
	if (! eargc) /* no line sources specified */
		eargv[eargc++] = "-";
	if (! overlay)
		inter = 1;
	if (loaddef(master)) /* load map master */
		exit(1);
	geoinit(1, 0, datapt);
	if (inter) /* set interactive graphics */
		if (setpltr(1))
			exit(1);
	if (overlay && defopen(overlay)) /* open overlay file */
		emess(2,"overlay open failure",(char *)0);
	plotopt(CBASE);
	init();
	/* process line files */
	for ( ; eargc-- ; ++eargv) {
		if (! strcmp(*eargv, "-"))
			fid = stdin;
		else if ((fid = fopen(*eargv, "r")) == NULL) {
			perror(*eargv);
			continue;
		}
		dofence(fid);
		fclose(fid);
	}
	/* clear out pen(s) */
	clrpen();
	if (overlay)	/* close defered file */
		defclose();
	plotend();	/* interactive plotter */
	exit(0);
}

# define MAX_LINE 150		/* maximum input line length		*/
# define MAXC	50		/* maximum control args / line		*/
# define MAX_FLDS	20	/* maximum posting data fields		*/
# define DEF_DEL	'\t'	/* default field delimeter		*/
# define MAX_CHAN	MAX_FLDS /* maxumum channels			*/

# define NOPLOT	0x01

int delim = DEF_DEL;		/* data field delimeter			*/
int con_chr = '#';		/* col 1 tag identifying command line	*/

	static
selectp(i) {	/* select pen by no. */
	static last_pen = -1;
	static char s[]="A";

	if (i != last_pen) {
		s[0] = 'A'+i;
		plotopt(SPEN, s);
		last_pen = i;
	}
}

	static double
z_min,	/* z range */
z_max,
c_min,	/* centimeter range */
c_max,
z_scale, /* scaling */
z_off,
z_cos = 1.,	/* rotation */
z_sin = 0.;

	static int
z_flag = 0,	/* flag that z scaling OK, =4 input, -1 scaling setup */
plot_post,	/* plot fence post flag */
max_chan = 0,	/* maximum reference channel */
chan_no;	/* currently referred to channel */

	static struct {
int	flags;
int	pen;
	} chan[MAX_CHAN + 1];

control(argc, argv)
char **argv;
int argc;
{
	int c, i;
	long f, strtol();
	double fl, atof(), cos(), sin();
	char *arg;

	while (argc-- > 0)
	if ((c = *(arg = *argv++)) == '-') for (;;) {
		switch (*++arg) {
		case '\0':
			break;
		case 'b':	/* end line segment */
			for (i = 1; i < MAX_CHAN; ++i)
				chan[i].pen = _PENUP;
			continue;
		case 'd':
			if (argc-- > 0) /* lon - lat fields */
				if (setform(*argv++, 0) != 2)
					emess(1,"must be two -d fields",
						(char *)0);
			continue;
		case 't':	/* data field delimeter */
			if (arg[1])
				delim = *++arg;
			continue;
		case 'z':	/* control line character */
			if (arg[1])
				con_chr = *++arg;
			continue;
		case 's':	/* set dash size */
			if (argc-- > 0)
				plotopt(DSIZE,
					lrnd(atof(*argv++) * def.cts_cm));
			continue;
		case 'm':	/* dash line mask */
			if (argc-- > 0) {
				f = strtol(*argv++, (char *)0, 0);
				plotopt(DMASK, (long)f);
			}
			continue;
		case 'D':	/* control dash-solid line mode */
			if (argc-- > 0)
				if ((f = **argv++) == 's')
					plotopt(SOLID);
				else if (f == 'd')
					plotopt(DASH);
			continue;
		case 'c':	/* select channel */
			if (argc-- > 0) {
				chan_no = atoi(*argv++);
				if (chan_no < 0)
					chan_no = 0;
				else if (chan_no > MAX_CHAN)
					chan_no = MAX_CHAN;
				selectp(chan_no);
			}
			continue;
		case 'p':	/* select mechanical pen */
			if (argc-- > 0)
				plotopt(MPEN, (long)atoi(*argv++));
			continue;
		case 'q':	/* discontinue mode */
			chan[chan_no].flags |= NOPLOT;
			continue;
		case 'f':	/* set channel data fields */
			if (argc-- > 0) {
				if (max_chan <= 0)
					max_chan = setform(*argv++, 2);
				else
					emess(0, "second -f parm ignored",
						(char *)0);
			}
			continue;
		case 'r':	/* select fence post rotation */
			if (argc-- > 0) {
				z_sin = sin(fl = DEG_TO_R * atof(*argv++));
				z_cos = cos(fl);
			}
			continue;
		case 'l':	/* continue plotting mode */
			chan[chan_no].flags &= ~NOPLOT;
			chan[chan_no].pen = _PENUP;
			continue;
		case 'S':	/* set scaling */
			if (argc-- > 0) {
				z_flag = sscanf(*argv++,"%le,%le,%le,%le",
					&z_min,&z_max,&c_min,&c_max);
				c_min *= def.cts_cm;
				c_max *= def.cts_cm;
				z_scale = (c_max - c_min) / (z_max - z_min);
				z_off = c_min - z_min * z_scale;
			}
			continue;
		case 'v':	/* plot fence post */
			if (argc-- > 0)
				plot_post = atoi(*argv++);
			continue;
		}
		break;
	} else if (c == '#') /* skip, as remainder of line comment */
		break;
}

static char *argv[MAXC];
static char *s, line[MAX_LINE];
char *getline();

	static long /* resultant location of geographic point */
xp, yp;

datapt(x, y) long x, y; { xp = x; yp = y; return(0); }

	static
draw(argc) {
	char *s, *setfield();
	double x, y, z, atof(), dmstor();
	int i;

	if (argc <= 2) return;	/* no coordinates or fields */

	if (!(s = setfield(0))) return;
	x = dmstor(s, 0); resetf();
	if (!(s = setfield(1))) return;
	y = dmstor(s, 0); resetf();
	if (!dpoint(x, y)) { /* got a point in region */
		if (plot_post > 0) {
			selectp(0);
			moveto(xp + lrnd(z_sin * c_min),
				yp + lrnd(z_cos * c_min));
			lineto(xp + lrnd(z_sin * c_max),
				yp + lrnd(z_cos * c_max));
			if (plot_post == 1)
				plot_post = 0;
		}
		for (i = 1; i <= max_chan; ++i) {
			if (chan[i].flags & NOPLOT)
				continue;
			if ((s = setfield(1+i)) && *s) {
				selectp(i);
				z = atof(s) * z_scale + z_off;
				pxyxmit(chan[i].pen,
					xp + lrnd(z_sin * z),
					yp + lrnd(z_cos * z));
				chan[i].pen = 0;
			}
			resetf();
		}
	}
}

dofence(fid) int *fid; {
	int control();

	while (s = getline(line, MAX_LINE, fid))
		if (*s == con_chr)
			words(s+1, MAXC, argv, control);
		else if (z_flag)
			groups(s, delim, draw);

}

	static	/* initialize pens */
startpen() {
	char *s = "A";
	int i;

	plotopt(NEWPEN, "A");
	for (i = 0; i <= MAX_CHAN; ++i) {
		s[0] = 'A'+i;
		plotopt(NEWPEN, s);
		chan[i].pen = _PENUP;
	}
}

init() {
	startpen();
	setform("2,1", 0);
	selectp( chan_no = 1 );
	if (cntrl)
		words(cntrl, MAXC, argv, control);
}

clrpen() {	/* remove defined pens */
	int i;

	for (i = 0; i <= MAX_CHAN; ++i) {
		selectp(i);
		plotopt(DELPEN);
	}
}
