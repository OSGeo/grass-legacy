/********************************************************/
/* NAME:        mount_vol                               */
/*                                                      */
/* FUNCTION:    mount correct tape for reading bands    */
/*                                                      */
/* USAGE:       mount_tape (n)                          */
/*                                                      */
/* INPUT:       n -- volume number of the tape          */
/*                                                      */
/* OUTPUT:      none                                    */
/********************************************************/
#include "tape.h"
mount_vol (n, bnd_exist, bnd)
{
    tape.eof = 0;
    if ((n == tape.vol) && bnd_exist )
	return;
    else {
      tape.vol = -1;
      fprintf(stderr, "\nPlease mount volume %d or with band %d on the tape\n",
	      n, bnd+1);
      unmount_tape();
      exit(0);
    }
}
