#ifndef lint
static char *SCCSID = "@(#)panal.c	AMG v.3.1";
#endif
/* analyse data for range and determine appropriate axis */
# include <stdio.h>
# include <string.h>
# include <math.h>

# define MAX_LINE 250
	static int
no_x = 10,	/* major division number */
no_y = 10,
log_x = 0,	/* log axis flag */
log_y = 0,
delim = '\t',	/* field delimiter */
con_chr = '#';	/* control tag (col. 1) */
	static char
line[MAX_LINE]; 
	static double
eps_x = .1,
eps_y =	.1;

main(argc, argv) char **argv; {
	char	*arg;
	FILE	*fid;
	char	**eargv;
	int	i, c, eargc;
	double	f;

	setform("1,2",0);
	eargv = argv;
	eargc = 0;
	while (--argc > 0) {
		if((c = **++argv) == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0':
				if (arg[-1] == '-')
					eargv[eargc++] = "-";
				break;
			case 'x': /* axis qualifier */
			case 'y':
				c = *arg;
				continue;
			case 'n': /* sub-division */
				if (argc-- > 0) {
					i = atoi(*++argv);
					if (c == 'x') no_x = i;
					else if (c == 'y') no_y = i;
					else no_x = no_y = i;
				}
				continue;
			case 'e': /* epsilon */
				if (argc-- > 0) {
					f = atof(*++argv);
					if (c == 'x') eps_x = f;
					else if (c == 'y') eps_y = f;
					else eps_x = eps_y = f;
				}
				continue;
			case 'l': /* logrithmic axis */
				if (c == 'x') log_x = 1;
				else if (c == 'y') log_y = 1;
				else log_x = log_y = 1;
				continue;
			case 'd':	/* x - y fields */
				if (argc-- > 0)
					if (setform(*++argv, 0) != 2)
					emess(1,"invalid no. -d fields",
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
			case 'i':	/* pre-pend file or field */
				if (argc-- > 0) {
					if (**++argv == '-') { /* line field */
						putchar(con_chr);
						putchar(' ');
						puts(*argv);
					} else if (fid = fopen(*argv,"r")) {
						while (fgets(line, MAX_LINE,
							fid))
							fputs(line, stdout);
						fclose(fid);
					} else
						emess(-2,"pre-pend",*argv,
							(char*)0);
				}
				continue;
			default:
				emess(0,"invalid runline option:",arg);
				break;
			}
			break;
		}
		else /* stack input files */
			eargv[eargc++] = *argv;
	}
		/* no line sources specified */
	if (! eargc)
		eargv[eargc++] = "-";
		/* process data files */
	for ( ; eargc-- ; ++eargv) {
		if (! strcmp(*eargv, "-"))
			fid = stdin;
		else if ((fid = fopen(*eargv, "r")) == NULL) {
			emess(-2,"input file:",*eargv);
			continue;
		}
		scan(fid);
	}
	synopsis();
	exit(0);
}
	static double
xmin, xmax, ymin, ymax;
	static int
not_new = 0;
	static char *
str;
	char
*getline();
	static
range(argc) {
	double x, y, atof();
	char *s, *setfield();

	if (argc < 2)
		return;	/* no coordinates */
	if (!*(s = setfield(0))) return;
	x = atof(s); resetf();
	if (!*(s = setfield(1))) return;
	y = atof(s); resetf();

	if (not_new) {
		if (x < xmin)
			xmin = x;
		else if (x > xmax)
			xmax = x;
		if (y < ymin)
			ymin = y;
		else if (y > ymax)
			ymax = y;
	} else {
		xmin = xmax = x;
		ymin = ymax = y;
		not_new = 1;
	}
}
scan(fid) int *fid; {
	while (str = getline(line, MAX_LINE, fid))
		if (*str != con_chr)
			groups(str, delim, range);
}
	static
delta(min, max, n, eps, tag) double min, max, eps; {
	int i, t;
	double iv, v, dv, dvl, low, hi;
	static double av[] = { 1., 2., 5., 10. };
	static int sv[] = { 5, 4, 5, 5};

	iv = floor(v = log10((max - min) / n));
	v = pow(10., fabs(v - iv));
	dvl = fabs(1. - v / av[0]);
	t = 0;
	for (i = 1; i < 4; ++i) {
		dv = fabs(1. - v / av[i]);
		if (dv < dvl) {
			dvl = dv;
			t = i;
		}
	}
	dv = pow(10., iv) * av[t];
	eps *= (max - min);
	low = floor((min - eps)/dv) * dv;
	hi = ceil((max + eps)/dv) * dv;
	printf("%c -%cdpn %g,%g %g %d %c Rng: %.3e %.3e\n",
		con_chr, tag, low, hi, dv, sv[t], con_chr, min,  max);
}
	static
ldelta(min, max, eps, tag) double min, max, eps; {
	double low, hi;

	low = pow(10., floor(log10(min * (1. - eps))));
	hi  = pow(10.,  ceil(log10(max * (1. + eps))));
	printf("%c -%cld %g,%g %c Rng: %.3e %.3e\n",
		con_chr, tag, low, hi, con_chr, min,  max);
}
synopsis() {
	if (xmin == xmax || ymin == ymax)
		emess(1,"x|y has no range",(char*)0);
	if ((log_x && xmin <= 0.) || (log_y && ymin <= 0.))
		emess(1,"x|y log. axix has value <= 0.",(char*)0);
	if (log_x)
		ldelta(xmin,xmax,eps_x,'x');
	else
		delta(xmin,xmax,no_x,eps_x,'x');
	if (log_y)
		ldelta(ymin,ymax,eps_y,'y');
	else
		delta(ymin,ymax,no_y,eps_y,'y');
	printf("%c -e %c end auto scale\n", con_chr, con_chr);
}
