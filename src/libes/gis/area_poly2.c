#include "gis.h"

/*!
 * \brief planimetric polygon area
 *
 *  \param x
 *  \param y
 *  \param n
 *  \return int
 */


double G_planimetric_polygon_area(double *x,double *y,int n)
{
    double x1,y1,x2,y2;
    double area;

    x2 = x[n-1];
    y2 = y[n-1];

    area = 0;
    while (--n >= 0)
    {
	x1 = x2;
	y1 = y2;

	x2 = *x++;
	y2 = *y++;

	area += (y2+y1)*(x2-x1);
    }

    if((area /= 2.0) < 0.0)
	area = -area;
    
    return area;
}
