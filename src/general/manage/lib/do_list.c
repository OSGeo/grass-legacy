#include "list.h"
do_list(n,mapset)
    char *mapset;
{
    G_list_element (list[n].element[0], list[n].desc[0], mapset, (int(*)())0);
}
