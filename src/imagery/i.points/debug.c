
#include "globals.h"
debug (msg) char *msg;
{
    R_stabilize();
    Curses_write_window (PROMPT_WINDOW, 1, 1, msg);
    Curses_getch(0);
}
