#include "../XDRIVER.h"

int Polydots_abs (int *xarray, int *yarray, int number)
{
    register int i;
    register int *xptr, *yptr;

    xptr = xarray;
    yptr = yarray;
    for (i = 0; i < number; i++) {
        Move_abs(*xptr, *yptr);
        Cont_rel(0, 0);
        xptr++;
        yptr++;
    }

    return 0;
}

int Polydots_rel (int *xarray, int *yarray, int number)
{
    register int i;
    register int *xptr, *yptr;

    xptr = xarray;
    yptr = yarray;
    for (i = 0; i < number; i++) {
        Move_rel(*xptr, *yptr);
        Cont_rel(*xptr, *yptr);
        xptr++;
        yptr++;
    }

    return 0;
}
