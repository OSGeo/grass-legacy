#include "raster.h"

int Pclose (void)
{
    R_close_driver();

    return 0;
}
