#ifndef lint
static char *SCCSID = "@(#)preview.c	AMG v.3.1";
#endif
# include <stdio.h>
# include <ctype.h>
# include <graphics.h>
# include "mapgen.h"
# include "plotgen.h"

char	*master;	/* map master file name */

# define MGEN	1
# define PGEN	2
int	type;
	struct map_def
m_def;
	struct PLTDEF
def;
	double
cts;
# define MAX_LINE 150
# define MAXC 30
init() { /* no_op */ }
	static
beep() { putchar('\007'); fflush(stdout); }
	static
loaddef(fid) FILE *fid; {
	char line[MAX_LINE], *s, *argv[MAXC], *getline();
	int scalein();

	while (s = getline(line, MAX_LINE, fid)) {
		if (*s == '#') ++s;
		if (! words(s, MAXC, argv, scalein))
			return;
	}
	emess(1,"failed to scale",(char *)0);
}
# define MAX_DEPTH 20
	static long
x_board[MAX_DEPTH], y_board[MAX_DEPTH],
x_base[MAX_DEPTH], y_base[MAX_DEPTH];
	static int
depth;
	static
setpltr(clear, argc, argv, zoom) char **argv; {
	static char *argl[15] = {0, 0, 0,"."};
	ANSWR *ans;
	char line[20];
	double fx, fy;
	int i;

	argl[2] = zoom ? "-iq" : "-i";
	for (i = 1; i < argc; ++i)
		argl[i+3] = argv[i];
		/* link to graphics */
	if (plotopen(argl))
		emess(1,"graphics device open failure",(char *)0);
		/* get size of plotter */
	ans = (ANSWR *)plotreq(P_SIZE);
	fx = ((double)ans->x) / x_board[depth] ;
	fy = ((double)ans->y) / y_board[depth] ;
	if (fx > fy)
		fx = fy;
	sprintf(line, "%e", fx);
	plotopt(RESCALE, line);
	plotopt(BASEX, (long)-x_base[depth]);
	plotopt(BASEY, (long)-y_base[depth]);
		/* clear screen */
	if(clear)
		plotopt(ERASE);
	return (0);
}
	static	/* draft cross lines */
cross(x, y) long x, y; {
	x -= x_base[depth];
	y -= y_base[depth];
	pxyxmit(_PENUP, x, y - 1000);
	pxyxmit(0, x, y + 1000);
	pxyxmit(_PENUP, x - 1000, y);
	pxyxmit(0, x + 1000, y);
}
	static FILE *
fdig;
	static /* digitizing control */
digit(a, cur) ANSWR *cur; {
	double xl, yl, ang, atan2();
	int f, c, cl;
	char info[50];

	xl = cur->x / cts;
	yl = cur->y / cts;
	if (a == 'a') {
		cross((long)cur->x, (long)cur->y);
		cur = (ANSWR *)plotreq(CURSOR);
		ang = atan2(cur->y /cts - yl, cur->x /cts - xl) * 
			57.29577951308232087721;
	}
	plotopt(DISABLE);
	plotreq(ERROR); /* any request to sync processes */
	while (!fdig) {
		printf("Name of digitizing output file:");
		gets(info);
		if (*info == '\n')
			return;
		if (!(fdig = fopen(info, "w")))
			perror(info);
	}
	if (a == 'l') { /* line drafting mode */
		fputs("-L 0\n", fdig);
		fprintf(fdig,"%.2f\t%.2f\n",xl, yl);
		pxyxmit(_PENUP, cur->x - x_base[depth],
			cur->y - y_base[depth]);
		while ((c = (cur = (ANSWR *)plotreq(CURSOR))->str[0]) == ' ') {
			pxyxmit(0, cur->x - x_base[depth],
				cur->y - y_base[depth]);
			xl = cur->x / cts;
			yl = cur->y / cts;
			fprintf(fdig,"%.2f\t%.2f\n",xl, yl);
		}
		fputs(".\n", fdig);
	} else {
		sprintf(info,"-xy %.2f %.2f\n",xl, yl);
		fputs(info, stdout);
		fputs(info, fdig);
		if (a == 'a') {
			sprintf(info, "-r %.1f\n",ang);
			fputs(info, stdout);
			fputs(info, fdig);
		}
		printf("Enter text (null line terminates)\n");
		for (f = 1, cl = '\n'; ; cl = c) {
			c = getchar();
			if (c == '\n' && cl == '\n')
				break;
			if (f) {
				fputs("-t\n", fdig);
				f = 0;
			}
			fputc(c, fdig);
		}
		if (!f)
			fputs(".\n", fdig);
	}
}
	static /* window control */
windo(cur) ANSWR *cur; {
	long xl, yl;

	cross(xl = cur->x, yl = cur->y);
	for (;;) {
		cur = (ANSWR *)plotreq(CURSOR);
		cross(cur->x, cur->y);
		if (cur->str[0] == ' ') {
			if (xl == cur->x || yl == cur->y) {
				beep();
				continue;
			}
			if (++depth >= MAX_DEPTH) --depth;
			x_base[depth] = xl < cur->x ? xl : cur->x;
			y_base[depth] = yl < cur->y ? yl : cur->y;
			x_board[depth] = abs(cur->x - xl);
			y_board[depth] = abs(cur->y - yl);
		} else {
			xl -= cur->x;
			yl -= cur->y;
			if (cur->str[0] == '0' && depth) {
				if ((x_base[depth] += xl) < 0)
					x_base[depth] = 0;
				if ((y_base[depth] += yl) < 0)
					y_base[depth] = 0;
			} else {
				if (++depth >= MAX_DEPTH) --depth;
				x_board[depth] = x_board[depth - 1];
				y_board[depth] = y_board[depth - 1];
				if ((x_base[depth] = x_base[depth-1] + xl) < 0)
					x_base[depth] = 0;
				if ((y_base[depth] = y_base[depth-1] + yl) < 0)
					y_base[depth] = 0;
			}
		}
		break;
	}
}
	/* main */
main(argc, argv) char **argv; {
	int	c, lastpl, i, j, zoom;
	char *s, *strrchr();
	FILE *fid;
	long tag;
	ANSWR *cur;

	if (s = strrchr(*argv, '/')) *argv = s + 1;
	zoom = strcmp("zoom",*argv) == 0;
	if (*argv[1] != '-') {
		master = argv[1];
		lastpl = 0;
		i = 1;
	} else for (i = 1; i < argc-1; ++i) /* map master file */
		if (! strcmp(argv[i], "-m")) {
			master = argv[++i];
			lastpl = i - 1;
			break;
		}

	if (! master) emess(1,"no master given",(char *)0);

	if (++i >= argc)
		emess(1, "no files given",(char *)0);
		/* find out what type of definition file */
	if (!(fid = fopen(master, "r")))
		emess(2, "master opening error:",master);
	if (fread(&tag, sizeof(long), 1, fid) != 1)
		emess(1,"load error",(char *)0);
	rewind(fid);
	if (tag == MAGIC) {	/* MAPGEN */
		type = MGEN;
		if (fread(&m_def, sizeof(m_def), 1, fid) != 1)
			emess(1,"mapdef load lenth error",(char *)0);
		x_board[depth] = m_def.B.x_max;
		y_board[depth] = m_def.B.y_max;
		cts = m_def.cts_cm;
	} else {	/* PLOTGEN */
		type = PGEN;
		loaddef(fid);
		x_board[depth] = def.x.board;
		y_board[depth] = def.y.board;
		cts = def.cts;
	}
	do {
		if (setpltr(1, lastpl, argv, zoom))
			exit(1);
		for ( j = i ; j < argc ; j++)
			plotopt(INCL, argv[j]);
		if (! zoom) {
			plotend();
			break;
		} else
			plotopt(NEWPEN, "T");
		for (;;) {
			c = (cur = (ANSWR *)plotreq(CURSOR))->str[0];
			if (isupper(c)) c += 'a' - 'A';
			switch (c) {
			case 'q':	/* done */
				break;
			case 'p':	/* replot previous */
				if (depth > 0)
					-- depth;
				break;
			case 'h':	/* replot original "home" */
				depth = 0;
				break;
			case 'a':
			case 'l':
			case 'd':	/* digitize point */
				digit(c, cur);
				continue;
			case ' ':	/* new sub-window */
				windo(cur);
				break;
			default:
				beep();
				continue;
			}
			plotend();
			break;
		}
	} while ( c != 'q' );
	if (fdig)
		fclose(fdig);
	exit(0);
}
