/********************************************************/
/* NAME: 		unmount_tape			*/
/*							*/
/* FUNCTION:		close tape device		*/
/*							*/
/* USAGE:		unmount_tape()			*/
/*							*/
/* INPUT:		none				*/
/*							*/
/* OUTPUT:		none				*/
/********************************************************/
#include <unistd.h>
#include "tape.h"

int unmount_tape (void)
{
    close (tape.fd);
    tape.fd = -1;

    return 0;
}
