/****************************************************************/
/*								*/
/*	point.h 	in	~/src/Gtraj			*/
/*								*/
/*	This header file defines the the data structure	of	*/
/*	a point	(structure containing various attributes of	*/
/*	a grid cell for trajectory analysis			*/
/*								*/
/****************************************************************/

	struct point{

	double orientation;	
	/* horizontal angle(radians) measured counter-clockwise	*/
	/* from	the +ve x direction                             */

	double distance;     /* distance from the firing point  */

	int x,y;
	/* map array coordinates of a point.			*/
	/* origin at the firing point.				*/

	double low_angle,high_angle;
	/* initial angles of fire (in radians) for the low	*/
	/* and high trajectories required to hit the point	*/

	struct point *next;
	/* pointer to the next point in the point list		*/
	
	struct point *previous;
	/* pointer to the previous point in the point list	*/

	} ;

#define		POINT	struct point

/****************************************************************/
