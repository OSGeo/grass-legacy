#include "imagery.h"
#include "files.h"
#include "parms.h"

write_img (img, ncols, nrows, S, parms, files)
    unsigned char **img;
    struct SigSet *S;            /* class parameters */ 
    struct parms *parms;         /* parms: command line parameters */
    struct files *files;         /* files: contains file to output */
{
    int row, col;
    int class;
    if (!parms->quiet) fprintf (stderr, "Writing %s ... ", parms->output_map);

    for (row = 0; row < nrows; row++)
    {
	if (!parms->quiet) G_percent (row, nrows, 2);
	for (col = 0; col < ncols; col++)
	{
		class = (int)img[row][col];
		files->cellbuf[col] = (CELL)S->ClassSig[class].classnum;
	}
	G_put_map_row (files->output_fd, files->cellbuf);
    }
    if (!parms->quiet) G_percent (row, nrows, 2);
}
