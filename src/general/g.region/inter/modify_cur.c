#include "glob.h"
#include "local_proto.h"
int 
modify_cur (void)
{
    if(edit_window (&cur_window))
	G_put_window (&cur_window);

    return 1;
}
