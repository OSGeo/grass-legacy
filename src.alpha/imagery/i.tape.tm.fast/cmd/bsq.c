/****************************************************************/
/* NAME:	bsq						*/
/*								*/
/* FUNCTION: 	read band sequential files			*/
/*								*/
/* USAGE:	bsq()						*/
/* 								*/
/* INPUT:	none						*/
/* 								*/
/* OUTPUT:	none						*/
/****************************************************************/
#include "tape.h"

bsq()
{
    int b;
    int r;
    int first=1;
    int ok;
    int f_count = 0;

    tape.tapebuf = (unsigned char *) G_malloc(tape.tapebufsize);
    ask_window();

    for (b = 0; b < THEMATIC_MAPPER_NBANDS; b++) 
    {
	if (tape.wantband[b])
	{
	    while (f_count && f_count--) {
              if (!tape.bnd_present[b])
 		break;
		if (verbose)
                  fprintf(stderr, "\nskipping file ...\n");
		I_tape_advance(tape.fd, -1);
	    }
	    tape.band[b].fd = open_band_new (b);
	    mount_vol (tape.band[b].vol, tape.bnd_present[b], b);
	    for (r = tape.firstrow; r <= tape.lastrow; r+=tape.blocking_factor)
	    {
		if (first && verbose) {
		    fprintf(stderr, "\nadvancing to band %d row %d ...\n", b+1, 
                            tape.firstrow); fflush (stdout);
		}
		ok = 0;
		ok = find_row (b, r, first);
		put_image (b,r,ok,first);
		if (first)
		{
		  if (verbose) {
		    fprintf(stderr, "\nextracting ...\n"); fflush (stdout);
		  }
		  first = 0;
		}
	        if (verbose)
                  G_percent (r, tape.lastrow, 2);
	    }
	    close_band (tape.band[b].fd, &tape.info, b);
	    if (verbose)
	      fprintf(stderr, "\n");
	    f_count++;
	    first = 1;
	}
	if (tape.bnd_present[b] && (!(tape.wantband[b])))
          f_count++;
    }
free (tape.tapebuf);
}
