#include "globals.h"

menu_driver (menu)
    Menu *menu;
{
    int nlines;
    int line;
    char c;
    char msg[80];

    Clear_window (MENU_WINDOW);
    for (nlines = 0; menu[nlines].msg; nlines++)
    {
	sprintf (msg, "%c  %s", 'A'+nlines, menu[nlines].msg);
	Write_window (MENU_WINDOW, nlines+1, 1, msg);
    }
    Write_window (PROMPT_WINDOW, 1,1, "\n");

    line = 0;
    do
    {
	c = Curses_getch(0);
	if (c >= 'a' && c <= 'z')
	    line = c-'a';
	else if (c >= 'A' && c <= 'Z')
	    line = c-'A';
	else
	    line = -1;
    }
    while (line < 0 || line >= nlines);
    return menu[line].key;
}
