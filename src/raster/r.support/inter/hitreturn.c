#include "gis.h"

int hitreturn (void)
{
    char buf[100];

    fprintf (stdout,"\nhit RETURN to continue -->");
    G_gets(buf);

    return 0;
}
