#include "glob.h"
modify_cur()
{
    if(edit_window (&cur_window))
	G_put_window (&cur_window);

    return 1;
}
