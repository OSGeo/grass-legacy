/* %W% %G% */

static int tape = -1 ;

opentape (dev)
    char *dev;
{

    tape = open (dev, 0);
    if (tape < 0)
    {
	perror (dev);
	exit(1);
    }
}

readtape (buf, n)
    char *buf;
{
    int x;

    x = read (tape, buf, n);
    if (x >= 0) return x;

    perror ("error reading tape");
    exit(1);
}

closetape()
{
    close (tape);
    tape = -1;
}
