/* tape only has 1 file, band interleaved */
#include "tape.h"

bil()
{
    int band;
    int row;

    bandfd = (int *) G_malloc (nbands * sizeof(int));
    first = (int *) G_malloc (nbands * sizeof(int));
    for (band=0; band < nbands; band++) {
      if (wantband[band]) {
	bandfd[band] = I_open_band_new (band);
        first[band] = 1;
      }
    }

    fprintf (stderr, "\nadvancing to row %d ...", firstrow); fflush (stdout);
    I_tape_advance (tapefd, (firstrow-1)/blocking_factor*nbands+skiprecords);

    fprintf (stderr, "\nextracting ... "); fflush (stdout);
    for (row = 0; row < nrows; row+=blocking_factor)
    {
	G_percent (row, nrows, 2);
	for (band = 0; band < nbands; band++)
	{
	    if(!readbil(row, band)) goto done;
	}
    }
    G_percent (row, nrows, 2);
done:
    fprintf (stderr, "\n");
    for (band=0; band < nbands; band++)
	if (wantband[band])
	    I_close_band (bandfd[band], &tape_info, band);
}
