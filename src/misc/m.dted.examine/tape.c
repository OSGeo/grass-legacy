#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "local_proto.h"
static int tape = -1 ;

int opentape (char *dev)
{
    tape = open (dev, 0);
    if (tape < 0)
    {
	perror (dev);
	exit(1);
    }

    return 0;
}

int readtape (char *buf, int n)
{
    int x;

    x = read (tape, buf, n);
    if (x >= 0) return x;

    perror ("error reading tape");
    exit(1);
}

int closetape (void)
{
    close (tape);
    tape = -1;

    return 0;
}
