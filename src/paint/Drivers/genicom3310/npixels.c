#include <stdlib.h>

Pnpixels (rows, cols)
    int *rows, *cols;
{
    sscanf (getenv ("NPIXELS"), "%d", cols);
    *rows = 0;
}
