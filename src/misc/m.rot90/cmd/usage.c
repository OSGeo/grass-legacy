#include <stdio.h>
usage(me)
    char *me;
{
    fprintf (stderr, "%s [-v] if=file of=file rows=# cols=# bpc=#\n", me);
    exit(1);
}
