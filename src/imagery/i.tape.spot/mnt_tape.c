/********************************************************/
/* NAME:	mount_tape				*/
/*							*/
/* FUNCTION:	open tape drive for reading		*/
/*							*/
/* USAGE:	mount_tape ()				*/
/*							*/
/* INPUT:	none					*/
/*							*/
/* OUTPUT:	none					*/
/********************************************************/

#include "tape.h"
mount_tape ()
{
    tape.eof = 0;
    while((tape.fd = open(tape.name, 0)) < 0)
	I_ask("\nMount tape and put on-line. Then hit RETURN-->", 0, 1) ;
}
