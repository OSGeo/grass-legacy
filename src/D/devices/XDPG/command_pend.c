#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

/*
 * Check the input fifo and see if anything is there.
 * Return false if nothing, true is there is.
 */


int
command_pending(_rfd)
int _rfd;
{

    struct stat sbuf;

    if (fstat(_rfd,&sbuf) == -1)
    {
	fprintf(stderr,"Cannot get fifo status\n");
	fflush(stderr);
	exit(-1);
    }
    return ( sbuf.st_size > 0);
}
