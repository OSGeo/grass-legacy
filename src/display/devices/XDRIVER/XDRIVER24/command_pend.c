#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
/* Check the input fifo and see if anything is there. Return false if
 * nothing, true is there is. */

int command_pending (int _rfd)
{
    struct stat sbuf;

    if (fstat(_rfd, &sbuf) == -1) {
        fprintf(stderr, "Cannot get fifo status\n");
        exit(-1);
    }
    return (sbuf.st_size > 0);
}
