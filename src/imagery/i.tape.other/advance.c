#include "tape.h"

tape_advance(fd,n)
{
    char buf[32767]; /* hope this is big enough for most records */
    if (n < 0)
        while (read (fd, buf, sizeof buf) > 0)
          continue;
    else
        while (n-- > 0 && read (fd, buf, sizeof buf) > 0)
          continue;
}
