#ifndef lint
static char *SCCSID = "@(#)dmstor.c	AMG v.3.1";
#endif
/* Convert DMS string to radians */
# include <string.h>
# include <ctype.h>
# include <math.h>

static char *sym = "NnEeSsWw";
static double vm[] = {
	.0174532925199433
	,.0002908882086657216
	,.0000048481368110953599
};

	double
dmstor(s, rs) char *s, **rs; {
	int sign, n, nl, tc;
	char *p;
	double v, tv;

	while (isspace(sign = *s)) ++s;
	if (sign == '+' || sign == '-') s++;
	else sign = '+';
	while (*s == ' ') ++s;
	for (v = 0., nl = 0 ; nl < 3 ; nl = n + 1 ) {
		p = s;
		while (isdigit(*s)) ++s;
		if (*s == '.') while(isdigit(*++s)) ;
		tc = *s;
		*s = '\0';
		if ((tv = atof(p)) == HUGE) goto error;
		switch ((*s = tc)) {
		case 'D': case 'd':
			n = 0; break;
		case '\'':
			n = 1; break;
		case '"':
			n = 2; break;
		case ' ':
			n = nl;
			break;
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
		while (*++s == ' ') ;
		if (n < nl) goto error;
		v += tv * vm[n];
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
	return v;
}
