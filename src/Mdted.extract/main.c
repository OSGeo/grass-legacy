/* %W% %G% */
#define MAIN
#include "dma.h"
main(argc, argv) char *argv[];
{
    PGM = argv[0];

    if (!getargs(argc, argv))
	return usage();

    if (west <= east || north <= south)
    {
	error ("illegal window specified",0);
	return usage();
    }

    if (access (outname, 0) == 0)
    {
	char msg[200];

	sprintf (msg, "file %s exists. Sorry!", outname);
	error (msg, 0);
	exit(1);
    }
    if (access (headname, 0) == 0)
    {
	char msg[200];

	sprintf (msg, "file %s exists. Sorry!", headname);
	error (msg, 0);
	exit(1);
    }

    tapefd = open (tapename, 0);
    if (tapefd < 0)
    {
	error (tapename,1);
	exit(1);
    }

    outfd = creat (outname, 0666);
    if (outfd < 0)
    {
	error (outname,1);
	exit(1);
    }

    headfd = fopen (headname, "w");
    if (headfd == NULL)
    {
	error (headname,1);
	exit(1);
    }

    if(!extract ())
	exit(1);

    close (tapefd);
    close (outfd);

    exit(0);
}
