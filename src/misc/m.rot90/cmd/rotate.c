#include <stdio.h>
#define SIZE 100
rotate (in, out, nrows, ncols, bpc, verbose)
{
    char *input, *block;
    char *inputp, *blockp;
    int row, col;
    int nr, nc;
    int orow, ocol;
    int i,c,r;
    int b_incr;
    char *malloc();

    input = malloc (SIZE * bpc);
    block = malloc (SIZE * SIZE * bpc);

    if (input == NULL || block == NULL)
    {
	fprintf (stderr, "Not Enough Memory\n");
	exit(1);
    }

/* create a complete output file */
    fill (out, nrows*ncols*bpc);

    for (row = 0; row < nrows; row += SIZE)
    {
	nr = SIZE;
	if ((row + nr) > nrows)
	    nr = nrows - row;
	b_incr = nr * bpc ;

	for (col = 0; col < ncols; col += SIZE)
	{
	    nc = SIZE;
	    if ((col + nc) > ncols)
		nc = ncols - col;

	    if (verbose)
		printf("%d,%d\n", row+1,col+1);

/* read this block */
	    for (r = 0; r < nr; r++)
	    {
		if(lseek (in, ((row+r)*ncols + col) * bpc, 0) < 0)
		{
		    fprintf (stderr, "%s: ", G_program_name());
		    perror ("cant seek into input file");
		    exit(1);
		}
		if(read (in, input, nc*bpc) != nc*bpc)
		{
		    fprintf (stderr, "%s: ", G_program_name());
		    perror ("error reading input file");
		    exit(1);
		}

/* copy input row to appropriate column in the block */
		inputp = input + (nc - 1) * bpc;
		blockp  = block + r * bpc;
		for (c = 0; c < nc; c++)
		{
		    for (i = 0; i < bpc; i++)
			blockp[i] = inputp[i];
		    inputp -= bpc;
		    blockp  += b_incr;
		}
	    }


/* write the rotated block into the correct place in the output file */
	    orow = ncols - col - nc;
	    ocol = row;

	    for (r = 0; r < nc; r++)
	    {
		if(lseek (out, ((orow+r)*nrows + ocol)*bpc, 0) < 0)
		{
		    fprintf (stderr, "%s: ", G_program_name());
		    perror ("cant seek into output file");
		    exit(1);
		}
		if(write (out, block + r*nr*bpc, nr*bpc) != nr*bpc)
		{
		    fprintf (stderr, "%s: ", G_program_name());
		    perror ("error writing output file");
		    exit(1);
		}
	    }
	}
    }
}
