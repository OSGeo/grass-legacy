#include "P.h"
Pinit()
{
    ras_row = 0;
    ras_nrows = 24;
    esc("O");	/* cancel skip over perforation */
}
