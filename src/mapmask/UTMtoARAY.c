/* %W% %G% */
#include "mapmask.h"

UTMtoARAY(n)
    int n;
{
    int i;
    double  UtoAxconv,UtoAyconv,UtoAxadd,UtoAyadd;

    extern double	*Ux,*Uy;
    extern double	*Ax,*Ay;


    UtoAyconv = 1.0/window.ns_res;
    UtoAxconv = 1.0/window.ew_res;
    UtoAyadd  = -(window.south)/window.ns_res;
    UtoAxadd  = -(window.west )/window.ew_res;

    UtoAyconv *= -1;
    UtoAyadd = (double)window.rows - UtoAyadd;

    for(i=0; i <= n; i++)
    {
	*(Ax+i) = *(Ux+i) * UtoAxconv + UtoAxadd;
	*(Ay+i) = *(Uy+i) * UtoAyconv + UtoAyadd;
    }
    return;
}
