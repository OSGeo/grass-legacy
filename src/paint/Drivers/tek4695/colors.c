#include "P.h"

clear_colors()
{
    clear (BLACK, nbytes);
    clear (YELLOW, nbytes);
    clear (CYAN, nbytes);
    clear (MAGENTA, nbytes);
}
output_colors()
{
    static char *MN_black   = "0123" ;
    static char *MN_magenta = "4567" ;
    static char *MN_yellow  = "89:;" ;
    static char *MN_cyan    = "<=>?" ;
    char NB[4];

    sprintf (NB, "%03d", nbytes);

    esc("I");
    Poutc (MN_yellow[ras_row]);
    Pouts(NB);
    Pout (YELLOW, nbytes);

    esc("I");
    Poutc (MN_cyan[ras_row]);
    Pouts(NB);
    Pout (CYAN, nbytes);

    esc("I");
    Poutc (MN_magenta[ras_row]);
    Pouts(NB);
    Pout (MAGENTA, nbytes);

    esc("I");
    Poutc (MN_black[ras_row]);
    Pouts(NB);
    Pout (BLACK, nbytes);
}
