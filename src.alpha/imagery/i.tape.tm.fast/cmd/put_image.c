/****************************************************************/
/* NAME:	put_image					*/
/*								*/
/* FUNCTION:	determine the starting position of the row	*/
/*								*/
/* USAGE:	put_image(b,r,ok,first)				*/
/*								*/
/* INPUT:	b -- band to be extracted			*/
/*		r -- row to be extracted			*/
/*		ok -- whether this row is found correctly	*/
/*		first -- whether this row is the first row	*/
/*								*/
/* OUTPUT:	none						*/
/****************************************************************/
#include "tape.h"

put_image (b,r,ok,first)
{
    int r_skip=0;
    register int j;
    unsigned char *n;

    if (ok && (tape.blocking_factor == 1))
      put_row (tape.band[b].fd, tape.tapebuf+tape.firstcol-1);
    else if (ok && (tape.blocking_factor != 1)) {
      if (first)
        r_skip = (r-1) % tape.blocking_factor;
      for (j=0; j<tape.blocking_factor-r_skip; j++) {
        n = tape.tapebuf+r_skip*tape.ncols+j*tape.ncols;
	put_row(tape.band[b].fd, n+tape.firstcol-1); 
      }
    }
    else
    {
	G_zero (tape.tapebuf, tape.tapebufsize);
	for (j=0; j<tape.blocking_factor; j++)
	  put_row (tape.band[b].fd, tape.tapebuf);
	fprintf (stderr, "\nWARNING: zero cell file\n");
    }
}
