#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include "tape.h"

tape_advance (fd, n)
{
  struct mtop cmd;

  if (n<0) {
    cmd.mt_op = MTFSF;
    cmd.mt_count = 1;
  }
  else if (n>0) {
    cmd.mt_op = MTFSR;
    cmd.mt_count = n;
  }
  else return;

  ioctl (fd, MTIOCTOP, &cmd);
}
