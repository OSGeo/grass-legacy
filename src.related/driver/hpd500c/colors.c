#include "P.h"


/*  Created from hpd550c driver by Garth Tier, 
    CSIRO Division of Wildlife and Ecology, 
    Alice Springs NT 0870, Australia
    Email: tie013@its.csiro.au*/

    /* All references to BLACK removed as we are now using composite
       black  */


clear_colors()
{
    clear (YELLOW, nbytes);
    clear (CYAN, nbytes);
    clear (MAGENTA, nbytes);
}
output_colors()
{
    char NB[6];

    sprintf (NB, "%d", nbytes);

    esc("*b");
    Pouts(NB);
    Pouts("V");
    Pout (CYAN, nbytes);

    esc("*b");
    Pouts(NB);
    Pouts("V");
    Pout (MAGENTA, nbytes);

    esc("*b");
    Pouts(NB);
    Pouts("W");
    Pout (YELLOW, nbytes);

}
