#include <stdio.h>
#include <math.h>
#include "stage3.h"

#define RADIUS_OF_EARTH 6371.2  /* In Kilometers */
#define RADIAN_TO_DEGREE 57.29
#define MESHSIZE 4.7625              /* In Kilometers */
#define EAST_N_POLE 401.0
#define NORTH_N_POLE 1601.0

LL
hraptolatlon(h_point, stlat, stlon)
HRAP h_point;
double stlat, stlon;

{

	
	double  x, y, distance, gi, angle;
	LL   ll_point;

	x = h_point.x - EAST_N_POLE;
	y = h_point.y - NORTH_N_POLE;

	distance = x*x + y*y;

	gi = ((RADIUS_OF_EARTH * (1 + sin(stlat)))/ MESHSIZE);
	gi = gi*gi;


	ll_point.lat = asin((gi - distance)/ (gi + distance))*RADIAN_TO_DEGREE ;

	angle = atan2(y,x)* RADIAN_TO_DEGREE;
	/*
	if(angle < 0)    
		angle = angle + 360.0;
		*/

	ll_point.lon = 270.0 + stlon - angle;
	if(ll_point.lon < 0) 
		ll_point.lon = ll_point.lon + 360.0;

	if(ll_point.lon > 360)
		ll_point.lon = 360.0 - ll_point.lon;

	return ll_point;
}
