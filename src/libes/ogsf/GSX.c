/*  GSX.c 
    Bill Brown, USACERL  
    December 1993
*/

#include "gstypes.h"

static int Cxl=0;

int 
GS_check_cancel()
{
    Cxl_func();
    return(Cxl);
}

GS_set_cancel(c)
int c;
{
    Cxl = c;
}

GS_set_cxl_func(f)
void (*f)();
{
    Cxl_func = f;
}


GS_set_swap_func(f)
void (*f)();
{
    Swap_func = f;
}

