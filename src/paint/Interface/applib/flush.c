#include "interface.h"

int Pflush (void)
{
 /*   P__opcode (FLUSH);*/
 P__flushdev (); 
 /* bugfix 23/03/00 ash - otherwise xbuf[] is not empty before close (wfd) */
 /*problem description - as P_send evokes P__writedev, which does not 
  guarantee that the buffer is flushed, we must check that all data has been
  sent before we close pipes*/
    return 0;
}
