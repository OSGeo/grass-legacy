/* Function: Polyline_abs    	P.W. Carlson		April 1990  */

Polyline_abs(xarray, yarray, number)
int *xarray, *yarray, number;
{
    int *xptr, *yptr;

    xptr = xarray;
    yptr = yarray;

    Move_abs(*xptr++, *yptr++);

    while(--number) Cont_abs(*xptr++, *yptr++);
}
