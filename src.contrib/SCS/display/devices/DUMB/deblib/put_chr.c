

/*---------- Function: put_chr ----------*/
#include <stdio.h>
put_chr(c)
char c;
{
    putc(c, stdout);
    fflush(stdout);
}
