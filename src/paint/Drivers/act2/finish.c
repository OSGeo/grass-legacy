#include "P.h"

Pfinish()
{

/*
 * ACT printer paints a line in 2 passes. This means that a line
 * doesn't get finished until the next one comes in.  This implies
 * that the very last line before the close will not get painted
 * So, we force it to be painted by printing a new line
 */

    Palpha() ;
    Pout ("\n\n",2);
    Pflush();
}
