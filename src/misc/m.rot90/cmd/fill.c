#include <unistd.h>
#include "gis.h"

int fill (int out, int nbytes)
{
    char buf[10240];
    register int n;

/* this code zeros buf. it has been removed since what is written
   into out at this point is irrelevant
    n = nbytes;
    if (n > sizeof(buf))
	n = sizeof(buf);
    b = buf;
    while (n-- > 0)
	*b++ = 0;
*/
    
    while (nbytes > 0)
    {
	n = nbytes;
	if (n > sizeof(buf))
	    n = sizeof(buf);
	if(write (out, buf, n) != n)
	{
	    fprintf (stderr, "%s: ", G_program_name());
	    perror ("error writing output file");
	    exit(1);
	}
	nbytes -= n;
    }

    return 0;
}
