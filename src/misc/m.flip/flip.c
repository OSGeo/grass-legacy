#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <stdlib.h>
/*#include <sys/file.h>*/

int flip (int in, int out, int nrows, int ncols, int bpc, int verbose)
{
    char *buffer;
    int row;

    buffer = malloc (ncols* bpc);

    if (buffer == NULL)
    {
	fprintf (stderr, "Not Enough Memory\n");
	exit(1);
    }

/*
    extract from input starting at rear, write to output beginning
    at front.
*/

    for (row = nrows; row; row--)
    {
	/*lseek (in, (row-1)*ncols*bpc, L_SET);*/
	lseek (in, (row-1)*ncols*bpc, 0);
	read (in, buffer, bpc * ncols);
	write(out, buffer, bpc * ncols);
    }
    free (buffer);

    return 0;
}
