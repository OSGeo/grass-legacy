#include "glob.h"

void
write_results (filename)
    char *filename;
{
    int pt;
    FILE *fd;

    fd = fopen (filename, "w");
    if (fd == NULL)
    {
	perror (filename);
	exit(1);
    }

    if (!be_quiet())
	fprintf (stderr, "Writing output file [%s] ...\n", filename);

    for (pt = 0; pt < pointlist.npoints; pt++)
    {
	if (report_point (fd, pt))
	    fprintf (fd,"\n");
    }

    if (fd != stdout) fclose(fd);
}
