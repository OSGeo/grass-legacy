/* %W%  %G% */

#include <stdio.h>
usage (me)
    char *me;
{
    fprintf (stderr,
"usage: %s [-v0] if=input of=output ff=filterfile \\\n",me);
    fprintf (stderr,
"       %*s [repeat=#] [title=title]\n",strlen(me),"");
    exit(1);
}
