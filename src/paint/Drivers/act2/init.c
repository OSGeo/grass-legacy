#define GLOBAL
#include "P.h"
Pinit()
{
/* set forground text color to blue, background to white */
    Palpha();
    Poutc (COLORF);
    Poutc (COLOR_BLUE);
    Poutc (COLORB);
    Poutc (COLOR_WHITE);

/* load default color lookup table */
    Praster();
    Poutc (DLOOKUP);
}
