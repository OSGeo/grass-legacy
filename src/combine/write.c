#include "gis.h"

write_window(win)
    struct Cell_head *win ;
{
    G__write_Cell_head (stdout, win, 0);
}
