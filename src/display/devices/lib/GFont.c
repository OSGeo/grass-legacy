#include "driverlib.h"

/* Font() conflicts with an X declaration */
int GFont (char *filename)
{
    return (init_font(filename));
}
