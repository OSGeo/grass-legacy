#include "gis.h"

/**************************************************************
 * filter: apply 3x3 filter to map
 *
 *  in_fd:   file descriptor for map to be filtered
 *  out_fd:  file descriptor for result
 *  nrows:   number of rows in map
 *  ncols:   number of columns in map
 *  matrix:  multiplicative factor matrix
 *  divisor: result will be divided by divisor
 *  verbose: flag to be verbose (if value is 1)
 **************************************************************/

filter (in_fd, out_fd, nrows, ncols, matrix, divisor, verbose)
    FILE *in_fd, *out_fd;
    int matrix[3][3];
{
    int row;
    register int col;
    CELL *m0, *m1, *m2, *f;
    CELL *in_cell[3];
    CELL *filt_row;
    CELL *temp;
    int f00,f01,f02;
    int f10,f11,f12;
    int f20,f21,f22;

    f00 = matrix[0][0] ;
    f01 = matrix[0][1] ;
    f02 = matrix[0][2] ;
    f10 = matrix[1][0] ;
    f11 = matrix[1][1] ;
    f12 = matrix[1][2] ;
    f20 = matrix[2][0] ;
    f21 = matrix[2][1] ;
    f22 = matrix[2][2] ;

    in_cell[0] = G_allocate_cell_buf();
    in_cell[1] = G_allocate_cell_buf();
    in_cell[2] = G_allocate_cell_buf();

    filt_row = G_allocate_cell_buf();

    if (fread(in_cell[1],sizeof(CELL),ncols,in_fd)  != ncols)
    {
        fprintf(stderr,"In filter:  error reading input\n");
        exit(5);
    }
    if (fwrite(in_cell[1],sizeof(CELL),ncols,out_fd)  != ncols)
    {
        fprintf(stderr,"In filter:  error writing output\n");
        exit(5);
    }
    if (fread(in_cell[2],sizeof(CELL),ncols,in_fd)  != ncols)
    {
        fprintf(stderr,"In filter:  error reading input\n");
        exit(5);
    }

/* perform the filter */

    if (verbose)
	percent (0,nrows,25);

    for (row = 1; row < nrows-1; row++)
    {
        temp = in_cell[0];
        in_cell[0] = in_cell[1];
        in_cell[1] = in_cell[2];

        if (fread(in_cell[2]=temp,sizeof(CELL),ncols,in_fd)  != ncols)
        {
            fprintf(stderr,"In filter:  error reading input\n");
            exit(5);
        }
            
        m0 = in_cell[0];
        m1 = in_cell[1];
        m2 = in_cell[2];
        f = filt_row;
        *f++ = *m1++;
        m0++; m2++;

        if (verbose)
	    percent (row,nrows,25);

        for (col = 1; col < ncols-1; col++)
        {
            *f++ = (
                *(m0 - 1) * f00 +
                *m0++     * f01 +
                *m0       * f02 +
                *(m1 - 1) * f10 +
                *m1++     * f11 +
                *m1       * f12 +
                *(m2 - 1) * f20 +
                *m2++     * f21 +
                *m2       * f22
               )/divisor;
        }

        *f = *m1;

        if (fwrite(filt_row,sizeof(CELL),ncols,out_fd)  != ncols)
        {
            fprintf(stderr,"In filter:  error writing output\n");
            exit(5);
        }
    }

    if (fwrite(in_cell[2],sizeof(CELL),ncols,out_fd)  != ncols)
    {
        fprintf(stderr,"In filter:  error writing output\n");
        exit(5);
    }

    if (verbose)
	percent (nrows,nrows,25);

}
