static char *SCCSID = "@(#)fmod.c	AMG v.1.1";
/*
**	fmod(a,b) supplied as patch for BSD systems failure to
**		supply ANSI standard library.
*/

# include <math.h>
	double
fmod(a, b) double a, b; {
	double t;

	if (b) {
		modf(a/b, &t);
		return(a-t*b);
	} else
		return(a);
}
