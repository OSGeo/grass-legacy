#ifndef lint
static char *SCCSID = "@(#)getline.c	AMG v.3.1";
#endif
/* input line continued with '\\n' */

# include <stdio.h>

char *
getline(s, n, file)
char *s;
FILE *file;
{
	register c, cl;
	char *sp;

	cl = '\0';
	sp = s;
	while ((c = getc(file)) != EOF) {
		if (c == '\n')
			if (cl == '\\') {
				if (n)
					*(sp-1) = ' ';
			} else {
				*sp = '\0';
				return (s);
			}
		else if (--n > 0) /* else quietly gobble up overflow */
			*sp++ = c;
		cl = c;
	}

	return ((char *) NULL);
}
