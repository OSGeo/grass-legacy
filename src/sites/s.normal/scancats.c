#include<stdio.h>
int scan_cats (s, x, y)
    char *s;
    long *x, *y;
{
    char dummy[2];
 
    *dummy = 0;
    if (sscanf (s, "%ld-%ld%1s", x, y, dummy) == 2)
        return (*dummy == 0 && *x <= *y);
    *dummy = 0;
    if (sscanf (s, "%ld%1s", x, dummy) == 1 && *dummy == 0)
    {
        *y = *x;
        return 1;
    }
    return 0;
}

