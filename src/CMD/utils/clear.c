#include <stdio.h>
#include <stdlib.h>

char PC;
char *BC;
char *UP;
short ospeed;

static
out (c)
{
    fprintf (stderr, "%c", c);
}

main()
{
	int out();
	char *term;
	char buf[1024];
	static char clear[100];
	static char first = 1;
	char *c;

	if (first)
	{
		*clear = 0;
		first = 0;
		if ((term = getenv ("TERM"))
		&& (tgetent (buf, term) > 0))
		{
			c = clear;
			tgetstr ("cl", &c) ;
		}
	}
	fflush (stderr);
	fflush (stdout);
	if (*clear) tputs (clear, 0, out);
	fflush (stderr);
	fflush (stdout);
}
