#include <stdio.h>

int divider (int n)
{
    while (n-- > 0)
	fprintf (stdout,"=");
    fprintf (stdout,"\n");

    return 0;
}
