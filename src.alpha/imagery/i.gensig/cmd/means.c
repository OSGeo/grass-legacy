#include "imagery.h"
#include "files.h"
compute_means (files, S)
    struct files *files;
    struct Signature *S;
{
    int n;
    int b;
    int nrows, ncols, row, col;
    CELL *class, *cell;

    for (n = 0; n < S->nsigs; n++)       /* for each signature (aka class) */
	for (b = 0; b < S->nbands; b++)  /* for each band file */
	    S->sig[n].mean[b] = 0.0;

    nrows = G_window_rows();
    ncols = G_window_cols();
    class    = (CELL *) G_calloc (ncols, sizeof(CELL));

    fprintf (stderr, "Calculating class mean%s ...", S->nsigs==1?"":"s");

    for (row = 0; row < nrows; row++)
    {
	G_percent (row, nrows, 2);
	read_training_map (class, row, ncols, files);
	for (b = 0; b < files->nbands; b++)	/* NOTE: files->nbands == S->nbands */
	{
	    if (G_get_map_row (files->band_fd[b], cell = files->band_cell[b], row) < 0) exit(1);
	    for (col = 0; col < ncols; col++)
	    {
		n = class[col];
		if (n < 0) continue;
		S->sig[n].mean[b] += cell[col];
	    }
	}
    }
    G_percent (row, nrows, 2);
    for (n = 0; n < S->nsigs; n++)       /* for each signature (aka class) */
	for (b = 0; b < S->nbands; b++)  /* for each band file */
	    S->sig[n].mean[b] /= S->sig[n].npoints;
    free(class);
}
