/***************************************************

NAME:		mount_tape ()

FUNCTION:	open tape drive for reading
****************************************************/
#include "tape.h"

mount_tape (tapename)
  char *tapename;
{
  while((tapefd = open(tapename, 0)) < 0)
    I_ask("\nMount tape and put on-line. Then hit RETURN-->",0,1) ;
  return;
}
