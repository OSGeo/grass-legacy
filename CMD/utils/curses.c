#include <curses.h>
main()
{
    initscr();
    raw();
    noecho();
    nonl();
    crmode();
    clear();
    refresh();
    endwin();
}
