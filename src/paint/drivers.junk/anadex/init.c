#include "P.h"
Pinit()
{
    darken = getenv ("DARKEN") != NULL;
    ras_row = 0;
    Poutc('\r');
    Poutc('\f');
}
