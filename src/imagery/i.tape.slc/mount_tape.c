/***************************************************

NAME:		mount_tape ()

FUNCTION:	open tape drive for reading
****************************************************/
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "imagery.h"

int 
mount_tape (char *tapename)
{
    int tapefd;
    while((tapefd = open(tapename, 0)) < 0)
	I_ask("\nMount tape and put on-line. Then hit RETURN-->",0,1) ;
    return tapefd;
}
