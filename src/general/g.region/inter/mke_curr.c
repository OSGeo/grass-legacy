#include "glob.h"
#include "local_proto.h"

int make_current (struct Cell_head *window, char *name)
{
    char prompt[100];
    sprintf (prompt, "would you like to make region [%s] your current region?", name);
    if (!yes(prompt,1))
	return 1;

    set_window (window, name);
    return 0;
}
