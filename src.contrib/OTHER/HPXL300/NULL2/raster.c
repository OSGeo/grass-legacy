#include "P.h"
Praster()
{
    esc("%1A");
    sprintf(buffer,"*r%dS",ncolumns); /* Set the graphics resolution */
    esc(buffer);
    esc("*r3A");  /* Place in Raster Mode */
    esc("*b1M");
}
