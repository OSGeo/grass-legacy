#include <stdio.h>
#define MARGINS 36 /*space allowed for margins .5 * 72 */
Pnpixels (rows, cols)
    int *rows, *cols;
{
    char *getenv(), *p;
	int n;

    if (NULL == (p = getenv ("HEIGHT")) || sscanf(p,"%d",&n) != 1 || n <= 0)
	    n = 570;
	*rows = n - MARGINS;
    if (NULL == (p = getenv ("WIDTH")) || sscanf(p,"%d",&n) != 1 || n <= 0)
	    n = 600;
	*cols = n -MARGINS;
}
