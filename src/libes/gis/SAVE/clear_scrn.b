/**********************************************************************
 *
 *   G_clear_screen()
 *
 *   clears the terminal screen
 *
 *   note:
 *      Programs using this must load with "-ltermcap"
 **********************************************************************/

#include <stdio.h>

#ifdef CAN_CLEAR

char PC;
char *BC;
char *UP;
short ospeed;

G_clear_screen()
{
	int out();
	char *getenv();
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

static
out (c)
{
    fprintf (stderr, "%c", c);
}

#else

G_clear_screen()
{
	system ("clear");
}

#endif

