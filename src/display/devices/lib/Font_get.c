#include "driverlib.h"

/* Font() conflicts with an X declaration */
int Font_get (char *filename)
{
    return (init_font(filename));
}
