/* tape only has 1 file, band interleaved */
#include "tape.h"

bil()
{
    int band;
    int row;

    bandfd = (int *) G_malloc (nbands * sizeof(int));
    for (band=0; band < nbands; band++)
    {
	if (wantband[band])
	    bandfd[band] = I_open_band_new (band);
    }

    printf ("\nadvancing to row %d ...", firstrow); fflush (stdout);
    I_tape_advance (tapefd, (firstrow-1)*nbands+skiprecords);

    printf ("\nextracting ... "); fflush (stdout);
    for (row = 0; row < nrows; row++)
    {
	G_percent (row, nrows, 10);
	for (band = 0; band < nbands; band++)
	{
	    if(!readbil (row, band)) goto done;
	}
    }
    G_percent (row, nrows, 10);
done:
    printf ("\n");
    for (band=0; band < nbands; band++)
	if (wantband[band])
	    I_close_band (bandfd[band], &tape_info, band);
}
