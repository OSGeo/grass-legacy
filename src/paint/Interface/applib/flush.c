#include "interface.h"

int Pflush (void)
{
 /*   P__opcode (FLUSH);*/
 P__flushdev (); 
 /* bugfix 23/03/00 ash - otherwise xbuf[] is not empty before close (wfd) */

    return 0;
}
