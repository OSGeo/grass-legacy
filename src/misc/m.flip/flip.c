#include <stdio.h>
/*#include <sys/file.h>*/
flip (in, out, nrows, ncols, bpc, verbose)
{
    char *buffer;
    int row;
    char *malloc();

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
}
