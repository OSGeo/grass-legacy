#include <stdio.h>
int 
usage (char *me)
{
    fprintf (stderr, "%s [-v] if=file of=file rows=# cols=# bpc=#\n", me);
    exit(1);
}
