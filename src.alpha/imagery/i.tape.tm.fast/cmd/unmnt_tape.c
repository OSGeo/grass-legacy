/****************************************/
/* NAME:	unmount_tape		*/
/*					*/
/* FUNCTION:	close tape device	*/
/*					*/
/* USAGE:	unmount_tape()		*/
/* 					*/
/* INPUT:	none			*/
/*					*/
/* OUTPUT: 	none			*/
/****************************************/
#include "tape.h"

unmount_tape()
{
    close (tape.fd);
    tape.fd = -1;
}
