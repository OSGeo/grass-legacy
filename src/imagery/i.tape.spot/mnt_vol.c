#include "tape.h"
#include <unistd.h>
int 
mount_vol (int n)
{
    char msg[100];

    tape.eof = 0;
    if (n == tape.vol)
	return 1;

    tape.vol = -1;
    sprintf (msg,"please mount volume %d and hit RETURN-->", n);
    while (tape.vol != n)
    {
	unmount_tape();
	I_ask (msg, 0, 1);
	mount_tape();
	if (!(read_tape(0) && tape.record_type == VOLUME_DESCRIPTOR))
	    continue;
	header (1);
    }

    return 0;
}
