/* %W% %G% */

#include "dma.h"

/* reads from tapefd into tapebuf
   ignores isolated physical eof on tape.
   2 eofs in succession indicate end of tape (eot)
*/
readtape()
{
    int n;
    int eof;
    eof = 0;
    while (eof < 2)
    {
	n = read (tapefd, tapebuf, tapebuflen);
	if (n < 0)
	{
	    error ("error reading tape",1);
	    exit(1);
	}
	if (n)
	    return 1;
	eof++;
    }
    return 0;
}
