static char *SCCSID = "@(#)hypot.c	AMG v.1.1";
/* routine for determining hypotenuse function:
**	hypot(x, y) ::= sqrt(x*x + y*y);
**
**	double hypot(x, y)
**	double x, y;
**
**	Rational fraction coefficients from Hart, et.al. (1968),
**	"Computer Approximations", John Wiley & Sons., New York.
**
**	This routine is offered as an alternative to the broken
**	hypot function often delivered with AT&T UNIX V which
**	either returns a corrupt value or causes floating point
**	exception when both arguments are 0..
**
**	Even for correct functioning of UNIX V hypot this version
**	is often at least 10% faster.  Some improvement in timing
**	could be achieved by determining rational fraction coeffi-
**	cients for the 1. -> 2. range and eliminating the range
**	correction statement.
*/

# define P0 .29730278874026
# define P1 8.9403076206457
# define P2 21.1252240569754
# define P3 5.9304944591466
# define Q0 2.4934718253158
# define Q1 17.7641338280541
# define Q2 15.0357233129921
# define P(x) (P0 + (x) * (P1 + (x) * ( P2 + (x) * P3 )))
# define Q(x) (Q0 + (x) * (Q1 + (x) * ( Q2 + (x))))
# define RS2 .707106781186547524400844362104

	double
hypot(x, y) double x, y; {
	double register t, s;

	if (x < 0.)	x = -x;
	if (y < 0.)	y = -y;
	if (x > y)	t = y / x;
	else {
		if (y == 0.) return(0.); /* ignored by AT&T */
		t = x / y;
		x = y;
	}
	t = .5 * ( 1. + t * t ); /* rat. fract. arg. range shift */
	s = P(t) / Q(t);
	return ( x * (s + t / s) * RS2); /* 1 cycle Raphson-Newton */
}
