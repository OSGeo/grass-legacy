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
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "tape.h"

int mount_tape (void)
{
  tape.eof = 0;
  if ((tape.fd = open(tape.name, 0)) < 0) {
    fprintf (stderr, "\nWARNING: Tape drive is not mounted.\n");
    exit(1);
  }

  return 0;
}
