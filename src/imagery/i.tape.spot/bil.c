/****************************************************************/
/* NAME:	bil						*/
/*								*/
/* FUNCTION: 	read band interleaved file(only 1 file)		*/
/*								*/
/* USAGE:	bil()						*/
/*								*/
/* INPUT:	none						*/
/*								*/
/* OUTPUT:	none						*/
/****************************************************************/
#include "tape.h"

bil()
{
    int band;
    int row;
    int y;

    tape.tapebuf=(unsigned char *) G_malloc(tape.tapebufsize); 
    I_edit_tape_info(&tape.info);
    ask_window();
    tape.wantband=I_ask_bands(tape.nbands);
    for (band=0; band < tape.nbands; band++)
    {
	if (tape.wantband[band])
	    tape.band[band].fd = I_open_band_new (band);
    }

    printf ("\nAdvancing to row %d ...\n", tape.firstrow); fflush (stdout);
    printf ("\nExtracting...\n"); fflush (stdout);
    for (row = 0; row < tape.nrows; row++)
    {
	G_percent (row, tape.nrows, 5);
	for (band = 0; band < tape.nbands; band++)
	{
	  y=find_row(band+1, row+tape.firstrow);
	  if(!put_image (row+tape.firstrow, band, y)) goto DONE;
	}
    }
    G_percent (row, tape.nrows, 5);
DONE:
    printf ("\n");
    for (band=0; band < tape.nbands; band++)
	if (tape.wantband[band])
	    I_close_band (tape.band[band].fd, &tape.info, band);
    free (tape.tapebuf);
}
