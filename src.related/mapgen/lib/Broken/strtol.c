static char *SCCSID = "@(#)strtol.c	AMG v.1.1";
/*
**	strtol() supplied as patch for BSD systems failure to
**		supply ANSI standard library.
*/

# include <stdio.h>
# include <ctype.h>
	long
strtol(s, ns, base) char *s, **ns; int base; {
	register c, bases;
	int sign;
	long value;

	if (ns) *ns = s;
	while (isspace(*s)) ++s;
	if (*s == '-') { sign = -1; ++s; }
	else {
		sign = 1;
		if (*s == '+') ++s;
	}
	value = 0;
	if ((bases = base) <= 0) {
		if (*s == '0') { /* binary or hex */
			s++;
			if (*s == 'x' || *s == 'X') {
				bases = 16;
				s++;
			} else
				bases = 8;
		} else
			bases = 10;
	}
	for (c = *s; isxdigit(c); c = *++s) {
		if (isdigit(c)) {
			value = value * bases + c - '0';
		} else if (bases == 16) {
			c -= 'a';
			value = value * bases + 10 +
				( c >= 0 ? c : c + 'a' - 'A');
		} else
			break;
	}
	if (ns) *ns = s;
	return (sign * value);
}
