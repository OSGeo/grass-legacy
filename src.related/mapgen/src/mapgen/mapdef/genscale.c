#ifndef lint
static char *SCCSID = "@(#)genscale.c	AMG v.3.1";
#endif
/* proc. for 'mapdef' */
# include <stdio.h>
# include <string.h>
# include <math.h>
# include "mapgen.h"
# include "mapdef.h"

double dmstor();
char *getline();

# define DEF_MARG 3.
# define MAXARGS 10

static double xupp, yupp;
static struct WINDOW_ *cbase;
	static
subwin(argc, argv) char **argv; {
	double v;
	struct WINDOW_ *wbase;
	int c;

	if (!argc)
		return (0);
	if (**argv == 'd') { /* relative to data window */
		argc--;
		argv++;
		wbase = &def.D;
	} else /* relative to board */
		wbase = &def.B;
	if (argc != 4)
		return (1);
	while (argc--) {
		if ((c = **argv) == '|' || c == '>') {
			(*argv)++;
			v = (argc & 1) ? wbase->x_max : wbase->y_max;
			if (c == '|')
				v = .5 * (v +
				((argc & 1) ? wbase->x_min : wbase->y_min));
		} else
			v = (argc & 1) ? wbase->x_min : wbase->y_min;
		v += atof(*argv++) * def.cts_cm;
		switch (argc) {
		case 3: cbase->x_min = floor(v); break;
		case 2: cbase->y_min = floor(v); break;
		case 1: cbase->x_max = ceil(v); break;
		case 0: cbase->y_max = ceil(v); break;
		}
	}
	if (cbase->x_min < 0 || cbase->x_max > def.B.x_max ||
		cbase->x_min >= cbase->x_max ||
		cbase->y_min < 0 || cbase->y_max > def.B.y_max ||
		cbase->y_min >= cbase->y_max) {
		quit("improperly defined window", 0);
		return (1);
	}
	return (0);
}

static
geoorig(argc, argv)
char **argv;
{
	double x, y;
	int i;

		/* set data origin (lower-left margin) */
	if (argc--) {
		x = (!strcmp(*argv, "-")) ? DEF_MARG : atof(*argv); 
		argv++;
		if (argc--) {
			y = (!strcmp(*argv, "-")) ? DEF_MARG : atof(*argv); 
			argv++;
		} else
			y = DEF_MARG;
	} else 
		x = y = DEF_MARG;
	def.xf_off += ( x *= def.cts_cm );
	def.yf_off += ( y *= def.cts_cm );
	xupp += x;
	yupp += y;
	def.D.x_min = floor(x) - 1;
	def.D.y_min = floor(y) - 1;

		/* set data range as sub geographic range */
	for (i = 0; i < 4 && argc >= 2 ; i++) {
		if (!strcmp(*argv, "-")) { /* check for omission */
			argc--;
			argv++;
			continue;
		} /* note: above "-" denotes absence of two ordinary entries */
		if ((x = dmstor(*argv++, 0)) == HUGE ||
		    (y = dmstor(*argv++, 0)) == HUGE) {
			quit("radian conversion error", 0);
			return (1);
		}
		argc -= 2;
		if (mproj(x, y, &x, &y)) {
			quit("projection failure", 0);
			return (1);
		}
		switch (i) {
		case 0:		/* new min x */
			x -= def.D.x_min;
			def.xf_off -= x;
			xupp -= x;
			break;
		case 1:		/* new min y */
			y -= def.D.y_min;
			def.yf_off -= y;
			yupp -= y;
			break;
		case 2:		/* new max x */
			xupp = x;
			break;
		case 3:		/* new max y */
			yupp = y;
			break;
		}
	}
	def.D.x_max = ceil(xupp) + 1;
	def.D.y_max = ceil(yupp) + 1;
	if (def.D.x_max <= def.D.x_min ||
		def.D.y_max <= def.D.y_min) {
		quit("range error", 0);
		return (1);
	}
	if (verbose)
		printf("x, y size: %.2f %.2f (cm.)\n",
			def.D.x_max / def.cts_cm,
			def.D.y_max / def.cts_cm);
	return (0);
}
	/* set up scaling */
genscale() {
	char *args[MAXARGS];
	double temp, temp2, ll[5], xa, xb, ya, yb;
	int i;
	char *s, *sv;

		/* set plotter resolution */
	def.cts_cm = (cpc <= 0.) ? DEF_CTS_CM : cpc;

/*
** set map scaling
** input lines -s lines 1 (optionally 1a) and 2
*/
start1:
	if (prompt) puts("enter scale or 0 if calib. mode desired");
	if (getline(line, MAXLINE, stdin) == 0)
		quit("no scale line", 1);
	if ((def.scale = atof(line)) <= 0.) {
		if (prompt) puts(
			"enter pt_A lon lat, pt_B lon lat, dist (cm)");
		if (!(s = getline(line, MAXLINE, stdin)))
			quit("no range data", 1);
		for (i = 0; i < 5; i++) {
			if ((sv = strtok(s, " \t")) == NULL ||
			    (temp = (i < 4 ? dmstor(sv, 0) : atof(sv)))
			            == HUGE) {
				quit("bad or null value",0);
				goto start1;
			}
			ll[i] = temp;
			s = 0;
		}
		if (ll[4] <= 0.) {
			quit("bad dist parameter", 0);
			goto start1;
		}
		if (approj(ll[0], ll[1], &xa, &ya) ||
		    approj(ll[2], ll[3], &xb, &yb)) {
			quit("values out of lat-lon window", 0);
			goto start1;
		}
		def.scale = hypot(xa-xb, ya-yb) * 100. / ll[4];
		if (def.scale == 0.) {
			quit("scale == 0", 0);
			goto start1;
		}
		if (verbose) {
			printf("calibrated scale: 1 : %.3f\n", def.scale);
			if (prompt) {
	 		puts("   is this acceptable?");
				if (! getline(line,MAXLINE,stdin)) {
					quit("no answer", 0);
					goto start1;
				}
				if (*line != 'y' && *line != 'Y')
					goto start1;
			}
		}
	}

	adjcoef(1.);

		/* get data rotation */
	for (;;) {
		if (prompt) puts("enter axis rotation (DMS CCW)");
		if (! getline(line, MAXLINE, stdin))
			quit("eof on rotation input", 1);
		if ((temp = dmstor(line, 0)) != HUGE)
			break;
		puts("improper DMS value");
	}
	temp2 = (def.cts_cm * 100.) / def.scale;
	def.cosr = cos(temp) * temp2;
	def.sinr = sin(temp) * temp2;
	def.xf_off = def.yf_off = 0.;
		/* mproj output now in plotter units */

		/* determine range of projected data */
	if (range(&xa, &xupp, &ya, &yupp))
		quit("fatal range determination error",1);
	xupp += (def.xf_off = -xa);
	yupp += (def.yf_off = -ya);
	if (verbose)
		printf("x, y size: %.2f %.2f (cm.)\n",
			xupp/def.cts_cm, yupp/def.cts_cm);

/*
** input line -s 3
*/

	for (;;) {
		if (prompt)
			puts("data x, y origin [geographic sub-range]");
		if (!(s = getline(line, MAXLINE, stdin)))
			quit("EOF @ datat range line",1);
		if (!words(s, MAXARGS, args, geoorig))
			break;
	}

/*
** input line -s 4
*/
	def.B.x_max = def.D.x_max;
	def.B.y_max = def.D.y_max;
	if (prompt)
		printf("enter right (%.2f) & top (%.2f) margin values (cm.)\n",
			DEF_MARG, DEF_MARG);
	if (!(s = getline(line, MAXLINE, stdin)))
		quit("no margin line",1);
	for (i = 0; i < 2 ; ++i) {
		if ((sv = strtok(s, " \t")) != NULL && *sv != '-')
			temp = floor(def.cts_cm * atof(sv));
		else
			temp = floor(def.cts_cm * DEF_MARG);
		s = NULL;
		if (i)
			def.B.y_max += temp;
		else
			def.B.x_max += temp;
	}

	if (verbose)
		printf("final full scale map size: %.2f %.2f (cm.)\n",
			def.B.x_max / def.cts_cm, def.B.y_max / def.cts_cm);

	def.S1.x_max = def.S2.x_max = 0;
# ifdef SUB_WIND
/*
** line 5 and 6 sub-windows
*/
	if (verbose)
		printf("sub-window specs: [d] x y (llc) x y (urc)\n");
	for (;;) {
		if (verbose) printf("A sub-window\n");
		if ((s = getline(line, MAXLINE, stdin)) == NULL) break;
		cbase = &def.S1;
		if (!words(s, MAXARGS, args, subwin)) break;
	}
	for (;;) {
		if (verbose) printf("B sub-window\n");
		if ((s = getline(line, MAXLINE, stdin)) == NULL) break;
		cbase = &def.S2;
		if (!words(s, MAXARGS, args, subwin)) break;
	}
# endif

		/* clean and initialize remainder of scaling */

	if (verbose)
		puts("scaling done");
}
