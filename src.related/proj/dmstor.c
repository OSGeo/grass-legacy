#ifndef lint
static char *SCCSID = "@(#)dmstor.c	USGS v.3.2";
#endif
/* Convert DMS string to radians */
# include <string.h>
# include <ctype.h>
# include <math.h>
	double
strtod();
	static char
*sym = "NnEeSsWw";
	static double
vm[] = {
	.0174532925199433
	,.0002908882086657216
	,.0000048481368110953599
};
	double
dmstor(s, rs) char *s, **rs; {
	int sign, n, nl, tc, cl;
	char *p, *pl;
	double v, tv;

	while (isspace(sign = *s)) ++s;
	if (sign == '+' || sign == '-') s++;
	else sign = '+';
	/* force end of string at next blank, compensate for 9e 9 */
	for (cl = *(pl = s); cl && (cl != ' '); cl = *++pl) ;
	*pl = '\0';
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
			if (nl) goto error;
			++s;
			v = tv;
			goto skip;
		default:
			v += tv * vm[nl];
		skip:	n = 4;
			continue;
		}
		if (n < nl) goto error;
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
	goto end;
error:
	v = HUGE;
end:
	if (rs)
		*rs = s;
	*pl = cl;
	return v;
}
