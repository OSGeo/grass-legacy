#include "gis.h"

int 
write_window (struct Cell_head *win)
{
    G__write_Cell_head (stdout, win, 0);

    return 0;
}
