#include "P.h"
write_rasterfile(buf,n)
    char *buf;
{
    if(write (rasterfd, buf, n) != n)
    {
	unlink (rasterfile);
	perror (rasterfile);
	error ("can't write to rasterfile");
    }
}
