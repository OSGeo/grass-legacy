/*  @(#)clr_scrn.c	2.1  6/26/87  */
char PC;
char *BC;
char *UP;
short ospeed;
#include <stdio.h>
#include <stdlib.h>
#undef putchar

clear_screen()
{
	int putchar();
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
	if (*clear) tputs (clear, 0, putchar);
	fflush (stderr);
	fflush (stdout);
}
