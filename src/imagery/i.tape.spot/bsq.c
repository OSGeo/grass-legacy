#include "tape.h"

bsq()
{
    int b;
    int r;
    int first;
    int row;
    int y;

    tape.tapebuf = (unsigned char *) G_malloc(tape.tapebufsize);
    I_edit_tape_info (&tape.info);
    ask_window();
    tape.wantband = I_ask_bands (tape.nbands);

    for (b = 0; b < tape.nbands; b++)
    {
	if (tape.wantband[b])
	{
	    tape.band[b].fd = I_open_band_new (b);
	    first = 1;
	    mount_vol (tape.band[b].vol);
	    row = 0;
	    for (r = tape.firstrow; r <= tape.lastrow; r++)
	    {
		if (first)
		    printf("advancing to band %d ...\n", b+1);
#ifdef DEBUG
printf ("find_row(band=%d,row=%d)\n",b+1,r);
#endif
		y = find_row (b+1, r+tape.firstrow);
#ifdef DEBUG
if(!y) printf ("not found\n");
#endif
		if (first)
		{
		    printf("extracting ...\n");
		    first = 0;
		}
		G_percent (r, tape.nrows, 5);
		if (! put_image (row+tape.firstrow,b,y))
		{
		    printf("** error writing band file **\n");
		    exit(-1);
		}
	    }
	    I_close_band (tape.band[b].fd, &tape.info, b);
	    printf("\n");
	}
    }
}
