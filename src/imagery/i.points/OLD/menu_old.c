#include "globals.h"

menu_driver (menu)
    Menu *menu;
{
    int nlines;
    int line;
    char c;
    char buf[80];
    char *b;

    Clear_window (MENU_WINDOW);
    for (nlines = 0; menu[nlines].msg; nlines++)
	Write_window (MENU_WINDOW, nlines+1, 2, menu[nlines].msg);
    Write_window (PROMPT_WINDOW, 1,1, "Use RETURN key to advance cursor, space bar to make your choice\n");

    line = 0;
    do
    {
	Write_window (MENU_WINDOW, line+1, 1, "");
	if((c = Curses_getch(0)) == '\r')
	{
	    line++;
	    if (line >= nlines)
		line = 0;
	}
    }
    while (c != ' ');

    strcpy (buf, menu[line].msg);
    for (b = buf; *b; b++)
	if (*b >= 'a' && *b <= 'z')
	    *b += 'A' - 'a';
    Write_window (MENU_WINDOW, line+1, 2, buf);
    Write_window (PROMPT_WINDOW, 1, 1, "\n");
    return menu[line].key;
}
