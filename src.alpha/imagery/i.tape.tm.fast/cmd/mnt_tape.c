/************************************************/
/*						*/
/* NAME:	mount_tape ()			*/
/*						*/
/* FUNCTION:	open tape drive for reading 	*/
/*						*/
/* USAGE:	mount_tape ()          		*/ 
/*						*/
/* INPUT:	none				*/
/*						*/
/* OUTPUT:	none				*/
/************************************************/

#include "tape.h"
mount_tape ()
{
  tape.eof = 0;
  if ((tape.fd = open(tape.name, 0)) < 0) {
    fprintf (stderr, "\nWARNING: Tape drive is not mounted.\n");
    exit(1);
  }
}
