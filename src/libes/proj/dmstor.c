/* Convert DMS string to radians */
#ifndef lint
static char RCSID[] = "@(#)$Id: dmstor.c,v 4.2 1992/04/04 01:34:59 gie Exp $";
#endif
#include "projects.h"
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>

/* following should be sufficient for all but the rediculous */
#define MAX_WORK 64
	static char
*sym = "NnEeSsWw",
work[MAX_WORK];
	static double
vm[] = {
	.0174532925199433,
	.0002908882086657216,
	.0000048481368110953599
};
double
#ifdef __STDC__
dmstor(const char *is, char **rs)
#else
dmstor(is, rs)
    char *is;
    char **rs;
#endif
{
	int sign, n, nl;
	char *p, *s;
	double v, tv;

	if (rs)
		*rs = (char *)is;
	/* copy sting into work space */
	while (isspace(sign = *is)) ++is;
	for (n = MAX_WORK, s = work, p = (char *)is; isgraph(*p) && --n ; )
		*s++ = *p++;
	*s = '\0';
	/* it is possible that a really odd input (like lots of leading
		zeros) could be truncated in copying into work.  But ... */
	sign = *(s = work);
	if (sign == '+' || sign == '-') s++;
	else sign = '+';
	for (v = 0., nl = 0 ; nl < 3 ; nl = n + 1 ) {
		if (!(isdigit(*s) || *s == '.')) break;
		tv = strtod(s, &s);
		switch (*s) {
		case 'D': case 'd':
			n = 0; break;
		case '\'':
			n = 1; break;
		case '"':
			n = 2; break;
		case 'r': case 'R':
			if (nl)
				return HUGE_VAL;
			++s;
			v = tv;
			goto skip;
		default:
			v += tv * vm[nl];
		skip:	n = 4;
			continue;
		}
		if (n < nl)
			return HUGE_VAL;
		v += tv * vm[n];
		++s;
	}
		/* postfix sign */
	if (*s && (p = strchr(sym, *s))) {
		sign = (p - sym) >= 4 ? '-' : '+';
		++s;
	}
	if (sign == '-')
		v = -v;
	if (rs) /* return point of next char after valid string */
		*rs = (char *)is + (s - work);
	return v;
}
