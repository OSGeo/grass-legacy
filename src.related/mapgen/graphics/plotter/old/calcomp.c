#ifndef lint
static char *SCCSID = "@(#)calcomp.c	USGS v.4.4";
#endif
# include <stdio.h>
#define EXTDEV
#define PLOTTER
# include "graphics.h"
/* External driver to access Calcomp style FORTRAN routines
**
** External define is expected to define factors peculiar
** to each plotter generated.  Failure to properly define
** one of the following devices will result in compilation failure.
**
** Scaling is set up so that 1 unit of input is considered 1/200
** of a centimeter and the FORTRAN entries are expecting inches.
*/

# ifdef DP8 /* Houston DP8 plotter */
static int max_pen = 3;
# endif
# ifdef C1077 /* Calcomp 1077 plotter */
static int max_pen = 4;
# endif
# ifdef APPLE /* Apple lazer writer */
static int max_pen = 1;
# endif
# ifdef IMOGEN /* Imogen */
static int max_pen = 1;
# endif
# ifdef C5800 /* Calcomp 5800 electrostatic */
static int max_pen = 1024;
# endif
#define IN(x) if ((x = getchar()) == EOF) done(0)

	static float
fact = .00196850393700787401;
	static long
dev = 2,	/* output device */
draw = 2,
move = 3;
	static char *
name;
	static char
*error[] = {
	"EOF during sub-field input",
	"Invalid control code",
	"EOF detected before End Of Plot",
	"No Begining Of Plot tag"
};
	static void
done(err) {
	fprintf(stderr,"driver %s failure because %s\n",name,error[err]);
	exit(1);
}
	int
main(argc, argv) char **argv; {
	int c, d;
	long t;
	float x, y;

	name = *argv; /* save for error reference */
	if ((c = getchar()) != _BOP)
		done(3);
	x = 0;
	plots_(&x, &x, &dev);
	factor_(&fact);
	while ((c = getchar()) != EOF)
		switch (c) {
		case _EOP:
			t = 999; x = 0.;
			plot_(&x, &x, &t);
			exit(0); /* normal completion */
		case _PEN:
		case _PENL:
			IN(t);
			if (c == _PENL) {
				IN(d); t = (t << 8) + d;
				IN(d); t = (t << 8) + d;
			}
			t = t % max_pen + 1;
			newpen_(&t);
			break;
		case _DRAW:
		case _MOVE:
			IN(d); t = d << 8; IN(d); x = t + d;
			IN(d); t = d << 8; IN(d); y = t + d;
			plot_(&x, &y, c == _DRAW ? &draw : &move);
			break;
		case _SPCL:
			IN(c); while (c) IN(c); /* flush */
			break;
		default:
			done(1);
		}
	done(2);
}
