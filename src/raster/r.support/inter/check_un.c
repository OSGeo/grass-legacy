#include <unistd.h>
#include <stdlib.h>
#include "gis.h"
#include "local_proto.h"

int check_uncompressed (struct Cell_head *cellhd, long filesize)
{
    long x;
    int i;
    static char *tempfile = NULL;
    FILE *fd;
    char command[1024];

    x = cellhd->rows * cellhd->cols * cellhd->format ;
    if (x == filesize)
	return 1;

    if (tempfile == NULL)
	tempfile = G_tempfile();
    fd = fopen (tempfile, "w");
    fprintf (fd,
        "The product of the rows(%d), cols(%d) and bytes per cell(%d) = %ld\n",
	cellhd->rows, cellhd->cols, cellhd->format, x);
    fprintf (fd, "does not equal the file size (%ld)\n", filesize);

    fprintf (fd, "The following combinations will produce the correct file size\n\n");
    if (cellhd->format == 0 || filesize%cellhd->format != 0)
    {
	for (i = 1; i <= sizeof (CELL); i++)
	{
	    if (filesize % i) continue;
	    fprintf (fd, "%d byte%s per cell\n", i, i==1?"":"s");
	    factors (fd, filesize,i);
	}
    }
    else
    {
	i = cellhd->format;
	fprintf (fd, "%d byte%s per cell\n", i, i==1?"":"s");
	factors (fd, filesize,i);
    }
    fclose (fd);
    sprintf (command, "more %s", tempfile);
    system (command);
    unlink (tempfile);
    return 0;
}
