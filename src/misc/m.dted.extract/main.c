#define MAIN
#include "dma.h"
#include <unistd.h>
#include <fcntl.h>

int main (int argc, char *argv[])
{
	struct GModule *module;

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Extracts digital terrain elevation data (DTED - "
		"levels 1 and 2) produced and supplied by the Defense Mapping Agency "
		"(DMA) on 1/2-inch magnetic tapes.";

    if (!getargs(argc, argv))
	return usage();

    if (west <= east || north <= south)
    {
	error ("illegal region specified",0);
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
