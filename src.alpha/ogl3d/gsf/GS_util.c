
/*  gs_util.c
    Bill Brown, USACERL  
    January 1993
*/

#include "gstypes.h"
#include "math.h"

/* DEBUG */
#include <stdio.h>

/************************************************************************/

float
GS_distance (from, to)
float *from, *to;
{
    float x, y, z;

    x = from[X] - to[X];
    y = from[Y] - to[Y];
    z = from[Z] - to[Z];
    return (float) sqrt (x*x + y*y + z*z);
}


/************************************************************************/

float
GS_P2distance (from, to)
float *from, *to;
{
    float x, y;

    x = from[X] - to[X];
    y = from[Y] - to[Y];
    return (float) sqrt (x*x + y*y);
}


/************************************************************************/
/* v1 = v2 */

GS_v3eq(v1, v2)
float v1[3], v2[3];
{

    v1[X] = v2[X];
    v1[Y] = v2[Y];
    v1[Z] = v2[Z];

}

/************************************************************************/
/* v1 += v2 */

GS_v3add(v1, v2)
float v1[3], v2[3];
{

    v1[X] += v2[X];
    v1[Y] += v2[Y];
    v1[Z] += v2[Z];

}

/************************************************************************/
/* v1 -= v2 */

GS_v3sub(v1, v2)
float v1[3], v2[3];
{

    v1[X] -= v2[X];
    v1[Y] -= v2[Y];
    v1[Z] -= v2[Z];

}

/************************************************************************/
/* v1 *= k */

GS_v3mult(v1, k)
float v1[3];
float k;
{

    v1[X] *= k;
    v1[Y] *= k;
    v1[Z] *= k;

}

/************************************************************************/
/* Changes v1 so that it is a unit vector */

GS_v3norm(v1)
float v1[3];
{
float n;
    
    n = sqrt(v1[X] * v1[X] + v1[Y] * v1[Y] + v1[Z] * v1[Z]);
    if(n == 0.0) return(0);
    v1[X] /= n; 
    v1[Y] /= n; 
    v1[Z] /= n; 

}

/************************************************************************/
/* Changes v2 so that v1v2 is a unit vector */

GS_v3normalize(v1, v2)
float v1[3], v2[3];
{
float n, dx, dy, dz;
    
    dx = v2[X] - v1[X];
    dy = v2[Y] - v1[Y];
    dz = v2[Z] - v1[Z];
    n = sqrt(dx * dx + dy * dy + dz * dz);
    if(n == 0.0) return(0);
    v2[X] = v1[X] + dx/n; 
    v2[Y] = v1[Y] + dy/n; 
    v2[Z] = v1[Z] + dz/n; 

}


/************************************************************************/
/* get a normalized direction from v1 to v2, store in v3 */

GS_v3dir(v1, v2, v3)
float v1[3], v2[3], v3[3];
{
float n, dx, dy, dz;
    
    dx = v2[X] - v1[X];
    dy = v2[Y] - v1[Y];
    dz = v2[Z] - v1[Z];
    n = sqrt(dx * dx + dy * dy + dz * dz);
    if(n == 0.0) return(0);
    v3[X] =  dx/n; 
    v3[Y] =  dy/n; 
    v3[Z] =  dz/n; 

}


/************************************************************************/
/* get a normalized direction from v1 to v2, store in v3 */

GS_v2dir(v1, v2, v3)
float v1[2], v2[2], v3[2];
{
float n, dx, dy;
    
    dx = v2[X] - v1[X];
    dy = v2[Y] - v1[Y];
    n = sqrt(dx * dx + dy * dy);
    v3[X] =  dx/n; 
    v3[Y] =  dy/n; 

}

/************************************************************************/

