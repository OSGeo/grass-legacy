#include <stdlib.h>

Pnpixels (rows, cols)
    int *rows, *cols;
{
    *cols = 1016;
    if (getenv ("SHORT"))
	*cols = 680;
    *rows = 0;
}
