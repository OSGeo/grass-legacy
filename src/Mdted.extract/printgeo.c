/* %W% %G% */
#include <stdio.h>
printgeo (fd, x, h)
    FILE *fd;
    long x;
    char *h;
{
    long a;
    int b,c;

    if ((a = x) < 0)
	a = -x;
    b = a / 10 ;
    c = a - b * 10 ;

    fprintf (fd, "%3d.%02d.%02d", b/3600,(b%3600)/60, b%60);
    if (c)
	fprintf (fd, ".%d", c);
    fprintf (fd, "%c", x < 0 ? h[0] : h[1]);
}
