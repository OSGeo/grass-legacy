/***************************************************

NAME:		mount_tape ()

FUNCTION:	open tape drive for reading
****************************************************/
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "tape.h"

int mount_tape (char *tapename)
{
  while((tapefd = open(tapename, 0)) < 0)
    I_ask("\nMount tape and put on-line. Then hit RETURN-->",0,1) ;
  return 0;
}
