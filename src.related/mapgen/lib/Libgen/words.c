#ifndef lint
static char *SCCSID = "@(#)words.c	AMG v.3.1";
#endif
/*	two forms: traditional white space words for control and
**		"tab"ed words for data.
*/
# include <ctype.h>
# include <string.h>
	static
escpar(s) char **s; {
	register n, c, i;
	char *curr;
	curr = *s;

	if (isdigit(c = *++curr)) /* octal constant */
		for (n = c - '0', i = 0; isdigit(c = *++curr) && i < 2 ; i++)
			n = (n << 3) + (c - '0');
	else if (c == 'x' || c == 'X') /* hex constant */
		for (n = i = 0; isxdigit(c = *++curr) && i < 2 ; ++i) {
			n = (n << 4) + c;
			if (isdigit(c)) n -= '0';
			else n += c > 'F' ? 10-'a' : 10-'A';
		}
	else { /* special escape characters */
		switch (c) {
		case '\0': n = 0; --curr; break;
		case 't': n = '\t'; break;
		case 'b': n = '\b'; break;
		case 'r': n = '\r'; break;
		case 'n': n = '\n'; break;
		case 'f': n = '\f'; break;
		default: n = c; break; /* else anything */
		}
		++curr;
	}
	*s = curr-1;
	return(n); /* return current source ptr */
}
	/* control words, separated by 'white space' */
words(s, maxc, argv, func) char *s, **argv; int maxc; int (*func)(); {
	int  argc;
	register c;
	char *d;

	for (c = *s, argc = 0; c && argc < maxc ; ) {
		while (isspace(c)) c = *++s;
		if (c) {
			argv[argc++] = d = s;
			do
				*d++ = (c == '\\') ? escpar(&s) : c;
			while ((c = *++s) && !isspace(c));
			*d = '\0';
		}
	}
	return ((*func)(argc, argv));
}
	/* rework string for escape codes */
escline(s) char *s; {
	char *d;
	int c;

	for (d = s; c = *s ; ++s)
		*d++ = (c == '\\') ? escpar(&s) : c;
	*d = c;
}
