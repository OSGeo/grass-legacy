/* Function: Polydots_abs    	P.W. Carlson	April 1990   */

Polydots_abs(xarray, yarray, number)
int *xarray, *yarray, number;
{
    register int i;
    register int *xptr, *yptr;

    xptr = xarray;
    yptr = yarray;
    for(i = 0; i < number; i++)
    {
        Move_abs(*xptr, *yptr);
    	Cont_abs(*xptr + 1, *yptr);
    	xptr++;
    	yptr++;
    }
}
