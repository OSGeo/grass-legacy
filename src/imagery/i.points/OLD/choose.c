#include "globals.h"
/* given a file, contents of the file are displayed to the user
 * the user "selects" one of the lines and the line number
 * is returned. lines start at 1
 * 0 is returned if no items in file or user hits ESC
 */
choose_from_list (fd, nitems, label)
    FILE *fd;
    char *label;
{
    char buf[100];
    int item,n;
    int line;
    char c;

    if (nitems == 0)
	return 0;

    item = nitems;
    while (1)
    {
	if (item >= nitems)
	{
	    fseek(fd, 0L, 0);
	    item = 0;
	}
	n = show (fd);
	sprintf (buf, "Mark 'x' by %s", label);
	if (item+n < nitems)
	    strcat (buf, ", Ctrl-p for next page");
	Curses_clear_window (PROMPT_WINDOW);
	Curses_write_window (PROMPT_WINDOW, 1, 1, buf);
	Curses_write_window (PROMPT_WINDOW, 2, 1, "Hit ESC to cancel request");

	line = 1;
	while(1)
	{
	    Curses_write_window (MENU_WINDOW, line, 1, "");
	    c = Curses_getch(0);
	    if (c == 'x')
		return done(item+line);
	    if (c == 033)
		return done(0);
	    if (c == '\r')
	    {
		line++;
		if (line > n)
			line = 1;
	    }
	    if (c == '\016')
		break;
	}
	item += n;
    }
}
static
show (fd)
{
    char buf[100];
    int nlines;
    int n;

    nlines = MENU_WINDOW->bottom - MENU_WINDOW->top - 1;

    Curses_clear_window (MENU_WINDOW);
    n = 0;
    while (n < nlines)
    {
	if (fgets(buf, sizeof buf, fd) == NULL)
	    return n;
	n++;
	Curses_write_window (MENU_WINDOW, n, 2, buf);
    }
}

static
done(n)
{
    Curses_clear_window (MENU_WINDOW);
    Curses_clear_window (PROMPT_WINDOW);
    return n;
}
