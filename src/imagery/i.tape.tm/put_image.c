#include "tape.h"

put_image (b,row,ok)
{
    if (ok)
	put_row (tape.band[b].fd, tape.buf + IMAGE_DATA_START + tape.firstcol - 1, row);
    else
    {
	char zero[TAPE_BUF_SIZE];
	G_zero (zero, sizeof zero);
	put_row (tape.band[b].fd, zero, row);
    }
    return 1;
}
