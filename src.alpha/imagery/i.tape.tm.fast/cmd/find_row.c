/****************************************************************/
/* NAME:        find_row                                        */
/*                                                              */
/* FUNCTION:    find band and row from current tape             */
/*                                                              */
/* USAGE:       find_row (b,r,first)                       	*/
/*                                                              */
/* INPUT:       b -- band to be extracted                    	*/
/*              r -- row of the extracting window    		*/
/*		first -- 1 if the first row to be extracted;	*/
/*			 0 if the rest rows.			*/
/*                                                              */
/* OUTPUT:      1 -- successfuly extracting one line from tape  */
/*		0 -- error happened while extracting		*/
/****************************************************************/
#include "tape.h"

find_row (b, r, first)
{
    int rec;

    if (tape.bnd_present[b]) {
      if (first && (rec = (r-1) / tape.blocking_factor))
          I_tape_advance (tape.fd, rec); 
      tape.n = 0;
      if ((tape.n=read(tape.fd, tape.tapebuf, tape.tapebufsize))
	   ==tape.tapebufsize)
          return 1;
      else {
          G_zero (tape.tapebuf, tape.tapebufsize);
          return 0;
      }
    }
    else {
      G_zero (tape.tapebuf, tape.tapebufsize);
      fprintf (stderr, "WARNING: row %d in band %d missing from current tape\n",
          r, b+1);
      return 0;
    }
}
