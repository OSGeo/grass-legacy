/****************************************************************/
/*	calc_angles.c		in	~/src/Gtraj		*/
/*								*/
/*	This function calculates the initial angles of fire	*/
/*	using numerical integration				*/
/*								*/
/****************************************************************/

#include "gis.h"
#include "constants.h"

calculate_initial_angles(lower, higher, x,y)

        double *lower, *higher, x, y;
{
	extern double vel_initial;
	extern double high_angle;
	extern struct Cell_head window;
        double delta_time_lower, delta_time_higher,
		sin(),iterate_trajectory(),
		increment, decrement;


	delta_time_lower = window.ns_res / vel_initial / 1.5;

	/* increment in initial angle of fire			*/
	increment = (*lower) * 0.0025;		/* 0.25 percent	*/ 

	/* calculate lower initial angle of fire		*/
	*lower = iterate_trajectory(delta_time_lower, increment,
						*lower, x, y);

	/* calculate higher initial angle of fire		*/
	if(high_angle > (0.9 * (*higher)))
	{
	decrement =  -(*higher) * 0.0025;
	delta_time_higher = 2.0 * vel_initial / GRAVITY 
			* sin(*higher) * window.ns_res / x / 10.0;
	
	*higher = iterate_trajectory(delta_time_higher, decrement,
						*higher, x, y);
	}
}

/********* END OF FUNCTION "CALCULATE_INITIAL_ANGLES" ***********/


								
/****************************************************************/
/*								*/
/*	Iteration routine for identifying exact trajectory	*/
/*	required to hit the cell under consideration		*/
/*								*/

double iterate_trajectory(delta_time, change, theta, x, y)

	double delta_time, change, theta, x, y;
{
	extern double TOL_X;
	double distance, height, fabs();
	
	while(1)	/* loop until correct traj is found	*/
	{
	/* initialize hor and vert distances covered by shell	*/
	distance = 0.0;
	height   = 0.0;

	numerically_integrate(&distance, &height, x, y,
					 theta, delta_time);


		if(fabs(distance - x) > TOL_X)
		{
		/* reduce by 5 percent	*/
		delta_time = delta_time * 0.95;
		}
		else
		{
		if(height >= y) return(theta);	/* closure	*/
		
		theta += change;

		}
	}
}

/*************** END OF FUNCTION "ITERATE_TRAJECTORY" ***********/



/****************************************************************/
/*								*/
/*	This function uses numerical integration to trace	*/
/*	the trajectory of a shell from the time it is fired	*/
/*	to the time it nears the center of the cell being	*/
/*	considered for impact.					*/ 

numerically_integrate(distance, height, x, y, theta, delta_time)

	double *distance, *height, x, y, theta, delta_time;
{
	extern double vel_initial, MASS, DIAMETER;
	double velocity, velocity_x, velocity_y, 
		del_x, del_y,
	       	del_vel_x,del_vel_y,
		sin(), cos(), atan(), sqrt(),
		a, b, drag;

	velocity = vel_initial;		/* muzzle velocity	*/

	velocity_x = velocity * cos(theta);	/* velocity	*/
	velocity_y = velocity * sin(theta);	/* components	*/

        while(1)
        {
        drag = AIR_DENSITY * DIAMETER * DIAMETER * velocity
                * velocity * DRAG_COEFF;
 
        a = (- drag * cos(theta) / MASS) ;
        b = -(drag * sin(theta) / MASS + GRAVITY);
 
        del_vel_x = a * delta_time;
        del_vel_y = b * delta_time;
 
        del_x = (2.0*velocity_x+del_vel_x)/2.0*delta_time;
        del_y = (2.0*velocity_y+del_vel_y)/2.0*delta_time;
 
        *distance += del_x;
        *height   += del_y;

        if(check_for_closure(x, *distance)) break;
 
        velocity_x += del_vel_x;
        velocity_y += del_vel_y;

	/*	vectorial sum of velocity components		*/
        velocity = sqrt(velocity_x * velocity_x
                                + velocity_y * velocity_y);

	/*	theta for the next time increment		*/
        theta = atan(velocity_y / velocity_x);
        }
 
}

/************ END OF FUNCTION "NUMERICALLY_INTEGRATE" ***********/

 
int check_for_closure(x, distance)
        double x, distance;
{
	extern double TOL_X;

        if(distance >= x || fabs(distance - x) <= TOL_X) 
		return(1);
        else 	return(0);
}

/****************************************************************/


