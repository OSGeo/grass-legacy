#include <stdio.h>
#include <stdlib.h>
Pnpixels (rows, cols)
    int *rows, *cols;
{
    char *p;

    if (NULL == (p = getenv ("HEIGHT")) || sscanf(p,"%d",rows) != 1 || *rows <= 0)
	    *rows = 792;
    if (NULL == (p = getenv ("WIDTH")) || sscanf(p,"%d",cols) != 1 || *cols <= 0)
	    *cols = 612;
}
