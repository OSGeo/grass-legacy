/***************************************************

NAME:		mount_tape ()

FUNCTION:	open tape drive for reading

USAGE:		mount_tape ()

****************************************************/

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "tape.h"

int mount_tape (void)
{
    tape.eof = 0;
    while((tape.fd = open(tape.name, 0)) < 0)
	I_ask("\nMount tape and put on-line. Then hit RETURN-->", 0, 1) ;

    return 0;
}
