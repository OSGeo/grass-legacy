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
    char TST[40];

    sprintf (NB, "%03d", nbytes);

     esc("g"); 
    Poutc (MN_yellow[ras_row]);
    Pouts(NB);
    Poutc('\040');
    Pout (YELLOW, nbytes);

     esc("g");
    Poutc (MN_cyan[ras_row]);
    Pouts(NB);
    Poutc('\040');
    Pout (CYAN, nbytes);

     esc("g"); 
    Poutc (MN_magenta[ras_row]);
    Pouts(NB);
    Poutc('\040');
    Pout (MAGENTA, nbytes);

     esc("g"); 
    Poutc (MN_black[ras_row]);
    Pouts(NB);
    Poutc('\040');
    Pout (BLACK, nbytes);
}
