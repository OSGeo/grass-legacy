#include "imagery.h"
#include "files.h"
read_data (files, S)
    struct files *files;
    struct SigSet *S;
{
    int n;
    int b;
    int nrows, ncols, row, col;
    CELL *class;
    struct ClassData *Data;

    nrows = G_window_rows();
    ncols = G_window_cols();
    class    = (CELL *) G_calloc (ncols, sizeof(CELL));

    fprintf (stderr, "Reading image data ...");

    for (row = 0; row < nrows; row++)
    {
	G_percent (row, nrows, 2);
	read_training_map (class, row, ncols, files);
	for (b = 0; b < files->nbands; b++)
	    if (G_get_map_row (files->band_fd[b], files->band_cell[b], row) < 0) exit(1);

	for (col = 0; col < ncols; col++)
	{
	    n = class[col];
	    if (n < 0) continue;
	    Data = &S->ClassSig[n].ClassData;
	    for (b = 0; b < files->nbands; b++)
		Data->x[Data->count][b] = (double) files->band_cell[b][col];
	    Data->count++;
	}
    }
    G_percent (row, nrows, 2);
    free(class);
}
