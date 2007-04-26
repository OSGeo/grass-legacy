/* rotate.c
 *
 *   Copyright (C) 2007 by Hamish Bowman, and the GRASS Development Team
 *   Author(s): Hamish Bowman, Dunedin, New Zealand
 *
 *   This program is free software under the GNU General Public
 *   License (>=v2). Read the file COPYING that comes with GRASS
 *   for details.
 */

#include <math.h>

# define RpD ((2 * M_PI) / 360.)        /* radians/degree */
# define D2R(d) (double)(d * RpD)       /* degrees->radians */
# define R2D(d) (double)(d / RpD)       /* radians->degrees */

/*!
 * \fn int G_rotate_around_pt(int, int, int, int, double)
 *
 * \desc given a point, angle, and origin, rotate the point around the origin 
 * by the given angle. Uses int as it's designed for use with the display.
 *
 * \param X0  X component of origin (center of circle)
 * \param Y0  Y component of origin (center of circle)
 * \param X1  X component of point to be rotated (variable is modified!)
 * \param Y1  Y component of point to be rotated (variable is modified!)
 * \param angle  in degrees, measured CCW from east
 * \return always returns 0
 */

int G_rotate_around_pt(int X0, int Y0, int *X1, int *Y1, double angle)
{
    double linelength, theta;
    int Xadj, Yadj;

    /* convert ray to polar coords */
    linelength = sqrt(pow(X0-*X1, 2) + pow(Y0-*Y1, 2));
    theta = atan2((Y0-*Y1), (X0-*X1));

    /* adjust angle */
    theta -= D2R(angle);

    /* convert back to cartesian coords */
    Xadj = (int)floor( (linelength * cos(theta)) + 0.5 );
    Yadj = (int)floor( (linelength * sin(theta)) + 0.5 );

    /* calculate new point from origin+adjustments */
    *X1 = X0 - Xadj;
    *Y1 = Y0 - Yadj;

    return 0;
}
