#define GLOBAL
#include "P.h"
Pinit()
{
    ras_row = 0;
    ras_nrows = 4;
    esc("");	/* cancel any characters in printer buffer */
    Poutc ((char)24);
    esc("4");	/* clear-parameters */
    esc("C10");	/* foreground black, no background */
}
