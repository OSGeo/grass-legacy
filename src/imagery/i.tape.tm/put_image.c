#include "tape.h"

int put_image (int b, int row, int ok)
{
    if (ok)
	put_row (tape.band[b].fd, tape.buf + IMAGE_DATA_START + tape.firstcol - 1, row);
    else
    {
	unsigned char zero[TAPE_BUF_SIZE];
	G_zero (zero, sizeof zero);
	put_row (tape.band[b].fd, zero, row);
    }
    return 1;
}
