#include "glob.h"

make_current (window, name)
    struct Cell_head *window;
    char *name;
{
    char prompt[100];
    sprintf (prompt, "would you like to make window [%s] your current window?", name);
    if (!yes(prompt,1))
	return 1;

    set_window (window, name);
    return 0;
}
