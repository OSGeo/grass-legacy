/*	This file contains the definition of the data structure	*/
/*	of a point						*/	


struct new_point {

	double theta;	
	/* horizontal angle measured counter-clockwise from	*/
	/* the +ve x direction					*/

	double r;
	/* distance from the firing point 			*/

	int ii,jj;
	/* map array coordinates of a point.			*/
	/* origin at the firing point.				*/

	double l_traj,h_traj;
	/* initial angles of fire (in radians) for the low	*/
	/* and high trajectories.				*/

	struct new_point *next;
	/* pointer to the next point in the point list		*/
	
	struct new_point *back;
	/* pointer to the previous point in the point list	*/

	};
