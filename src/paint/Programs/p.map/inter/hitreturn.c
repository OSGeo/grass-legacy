#include <stdio.h>
#include "local_proto.h"

int hitreturn (void)
{
    char buf[100];
    fprintf (stdout,"set the printer and hit RETURN -->");
    input(buf);

    return 0;
}
