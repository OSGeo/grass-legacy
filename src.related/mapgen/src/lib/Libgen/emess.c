#ifndef lint
static char *SCCSID = "@(#)emess.c	AMG v.3.1";
#endif
/* general error message handler */
# include <stdio.h>

emess(code, s, s2) char *s, *s2; {

	fputs(s, stderr);
	if (s2) {
		fputc(' ', stderr);
		fputs(s2, stderr);
	}
	if (code == 2 || code == -2)
		perror();
	if (code > 0) {
		fputs("\nprogram abnomally terminated\n", stderr);
		exit(1);
	}
	putc('\n', stderr);
}
