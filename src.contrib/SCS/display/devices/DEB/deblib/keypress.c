

/*---------- Function: keypress ----------*/
#include <stdio.h>
keypress()
{
    int n;
    char c;

    n = fread(&c, 1, 1, stdin);
    return(n);
}
