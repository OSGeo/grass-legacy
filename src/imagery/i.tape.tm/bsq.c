#include "tape.h"

int bsq (void)
{
    int b;
    int r;
    int first;
    int row;
    int ok;

    I_edit_tape_info (&tape.info);

    ask_window();
    tape.wantband = I_ask_bands (THEMATIC_MAPPER_NBANDS);

    for (b = 0; b < THEMATIC_MAPPER_NBANDS; b++)
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
		    fprintf(stderr, "advancing to band %d ...\n", b+1);
#ifdef DEBUG
fprintf (stderr, "find_image(band=%d,row=%d)\n",b+1,r);
#endif
		ok = find_image (b+1, r);
#ifdef DEBUG
if(!ok) fprintf (stderr, "not found\n");
#endif
		if (first)
		{
		    fprintf(stderr, "extracting ...\n");
		    first = 0;
		}
		if (! put_image (b,row++,ok))
		{
		    fprintf(stderr, "** error writing band file **\n");
		    exit(-1);
		}
	    }
	    I_close_band (tape.band[b].fd, &tape.info, b);
	    fprintf(stderr, "\n");
	}
    }

    return 0;
}
