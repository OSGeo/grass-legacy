#include <stdio.h>
#include "ps_map.h"

int hitreturn (void)
{
    char buf[100];
    fprintf (stdout,"set the printer and hit RETURN -->");
    input(buf);

    return 0;
}
