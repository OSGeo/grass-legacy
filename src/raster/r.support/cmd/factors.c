#include <string.h>
#include <stdio.h>

int factors (FILE *fd, register long n, int div)
{
    register long m;
    register long x;
    char buf[30];
    int len, totlen;
    char fmt[30];


/* determine number of chars in largest factor that will be printed */
    sprintf (buf, "%ld", n);
    len = strlen (buf);
    n /= div;

/* build printf format */
    sprintf (fmt, "%%%dld * %%-%dld", len, len);

/* find the factors */
    totlen = 0;
    for (m = 1;; m++)
    {
	if (n%m == 0)
	{
	    x = n/m;
	    if (x < m) break;
	    sprintf (buf, fmt, m, x) ;
	    len = strlen (buf) + 3;
	    if (totlen + len > 75)
	    {
		fprintf (fd, "\n");
		totlen = 0;
	    }
	    fprintf (fd, "%s   ", buf);
	    totlen += len;
	}
    }
    if (totlen) fprintf (fd, "\n");

    return 0;
}
