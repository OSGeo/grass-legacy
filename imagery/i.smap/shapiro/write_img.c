#include "imagery.h"
#include "bouman.h"

int write_img (
    unsigned char **img, int ncols, int nrows,
    struct SigSet *S,            /* class parameters */ 
    struct parms *parms,         /* parms: command line parameters */
    struct files *files)         /* files: contains file to output */
{
    int row, col;
    int class;
    if (!parms->quiet) fprintf (stderr, "Writing %s ... ", parms->output_map);

    for (row = 0; row < nrows; row++)
    {
	if (!parms->quiet) G_percent (row, nrows, 2);
	for (col = 0; col < ncols; col++)
	{
	   if(G_is_c_null_value((CELL *)&img[row][col]))
		   G_set_c_null_value(&files->cellbuf[col], 1);
           else
	   {
		class = (int)img[row][col];
		files->cellbuf[col] = (CELL)S->ClassSig[class].classnum;
           }
	}
	G_put_c_raster_row (files->output_fd, files->cellbuf);
    }
    if (!parms->quiet) G_percent (row, nrows, 2);

    return 0;
}
