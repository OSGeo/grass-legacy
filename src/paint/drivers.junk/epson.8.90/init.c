#include "P.h"
Pinit()
{
    ras_row = 0;
    ras_nrows = 8;
    esc("O");	/* cancel skip over perforation */
}
