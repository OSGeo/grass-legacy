/* %W% %G% */
#include "P.h"

int rastermd;
Praster ()
{

    if (!(rastermd))
    {
        putc(RASTER,yellow_f);	   /* Enter Raster Graphics Mode   */
        putc((char)0x50,yellow_f); /* Portrait Raster Graphics Mode*/
        rastermd = 1;
    }
    else
        return;
}
