


/*---------- Function: cls ----------*/
#include <stdio.h>
cls()
{
    fwrite("\033c", 1, 2, stdout);
    fflush(stdout);
}
