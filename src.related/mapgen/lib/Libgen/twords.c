#ifndef lint
static char *SCCSID = "@(#)twords.c	AMG v.3.1";
#endif
/* single char delimited list */

twords(s, delim, maxc, argv, func)
char *s, **argv;
int (*func)();
{
	int argc;
	char *strchr();

	for (argc = 0; s && argc < maxc; ) {
		argv[argc++] = s;
		if (s = strchr(s, delim))
			*s++ = '\0';
	}
	return((*func)(argc, argv));
}
