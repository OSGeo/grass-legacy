#define GLOBAL
#include "P.h"
Pinit()
{
    char *malloc();

    NPIXELS = 2048;

    BLACK    = (unsigned char *) malloc (NPIXELS/8+1);
    CYAN     = (unsigned char *) malloc (NPIXELS/8+1);
    YELLOW   = (unsigned char *) malloc (NPIXELS/8+1);
    MAGENTA  = (unsigned char *) malloc (NPIXELS/8+1);

    ras_row = 0;
    ras_nrows = 4;

/* cancel any characters in printer buffer */
    esc("");
    Poutc ((char)24);

/* clear-parameters */
/*  esc("4"); */
    esc("c");

/* foreground black, no background */
    esc("C10");
}
