/****************************************************************/
/*	pts_elim.c	in 	~/src/Gtraj			*/
/*								*/
/*	This function eliminates those pts from the point list	*/
/*	that cannot be hit by a shell				*/
/*								*/
/****************************************************************/

#include "point.h"
#include "gis.h"
#include "radians.h"
#include "segment.h"

#define		BLOCK_PT_DIST		BLOCK_PT->distance
#define		NEXT_BLOCK_PT		BLOCK_PT->next
#define		BLOCK_PT_LOW_ANGLE	BLOCK_PT->low_angle
#define		BLOCK_PT_HIGH_ANGLE	BLOCK_PT->high_angle
#define		BLOCK_PT_ORIENT		BLOCK_PT->orientation
#define		BLOCK_PT_Y		BLOCK_PT->y
#define		BLOCK_PT_X		BLOCK_PT->x

#define		NEXT_CHECK_PT		CHECK_PT->next
#define		CHECK_PT_LOW_ANGLE	CHECK_PT->low_angle
#define		CHECK_PT_HIGH_ANGLE	CHECK_PT->high_angle
#define		CHECK_PT_ORIENT		CHECK_PT->orientation
#define		CHECK_PT_Y		CHECK_PT->y
#define		CHECK_PT_X		CHECK_PT->x

#define 	SEARCH_PT_X		SEARCH_PT->x
#define		SEARCH_PT_Y		SEARCH_PT->y
#define		SEARCH_PT_LOW_ANGLE	SEARCH_PT->low_angle
#define		SEARCH_PT_HIGH_ANGLE	SEARCH_PT->high_angle
#define		NEXT_SEARCH_PT		SEARCH_PT->next

POINT *unhittable_pts_elimination(head, seg_in)

	POINT *head;
	SEGMENT seg_in;
{

	POINT *BLOCK_PT, *CHECK_PT, *check();
	extern struct Cell_head window;
	extern double high_angle;
	double dtheta;
	extern int row_firept, col_firept;


	for(BLOCK_PT=head->next; 
	    BLOCK_PT !=NULL; 
	    BLOCK_PT= NEXT_BLOCK_PT) 
	{
        dtheta= window.ns_res / BLOCK_PT_DIST;
 
        for(CHECK_PT=head;
	    CHECK_PT!=BLOCK_PT;
            CHECK_PT=NEXT_CHECK_PT)
	{
        if (BLOCK_PT_LOW_ANGLE >=  CHECK_PT_LOW_ANGLE)  
        head = check(CHECK_PT,BLOCK_PT,head,dtheta,&seg_in);
        	}
        }       /* end of outer for     */
	
	return(head);

}	/* end of unhitt */



POINT *check(CHECK_PT,BLOCK_PT,head,dtheta,seg_in_p)
 
	POINT *CHECK_PT,*BLOCK_PT,*head;
	double dtheta;
	SEGMENT *seg_in_p;

{       
        int neighbor_x, neighbor_y, abs();
        double del_x,del_y,dist,fabs(),atan(),sqrt();
        double higher,lower;    /* low and high angles of impact*/
        double neighbor_orientation, i_higher, i_lower;
	extern int row_firept, col_firept;
	extern int  find_ABabcdisc(), find_angles();
	extern double find_orientation();
	extern struct Cell_head window;
        double A,B,a,b,c,
                disc;  /* discriminant in the quadratic equation*/
        POINT *delete();
	extern CELL firept_elev;
	CELL neighbor_elevation;
	char *value;

	/*      out of horizontal angle checking range  */
	if(fabs(BLOCK_PT_ORIENT - CHECK_PT_ORIENT) > dtheta) 
	{
       	return(head);
        }

 
/*if directly behind blocking pt, farther point's l_traj blocked*/
	if(CHECK_PT_ORIENT == BLOCK_PT_ORIENT)
	{
	if(obscured(BLOCK_PT_LOW_ANGLE,BLOCK_PT_HIGH_ANGLE,
		CHECK_PT_LOW_ANGLE,CHECK_PT_HIGH_ANGLE))
        	{
	head = delete(CHECK_PT,head);
        return(head);
        	}
	}


	if((BLOCK_PT_ORIENT < PIBYTWO) && (BLOCK_PT_ORIENT!=0.0))
			{
        	if(CHECK_PT_ORIENT < BLOCK_PT_ORIENT)
					{
			neighbor_x = BLOCK_PT_X;
			neighbor_y = BLOCK_PT_Y - 1;
                                        }
                                else    {
			neighbor_x = BLOCK_PT_X - 1;
			neighbor_y = BLOCK_PT_Y;
                                        }
                        }

	else if(BLOCK_PT_ORIENT > PIBYTWO && 
				BLOCK_PT_ORIENT < PI)
			{
        	if(CHECK_PT_ORIENT < BLOCK_PT_ORIENT)
					{
			neighbor_x = BLOCK_PT_X + 1;
			neighbor_y = BLOCK_PT_Y;
                                        }
                                else    {
			neighbor_x = BLOCK_PT_X;
			neighbor_y = BLOCK_PT_Y - 1;
                                        }
                        }
 
	else if(BLOCK_PT_ORIENT > PI && 
				BLOCK_PT_ORIENT < THREEPIBYTWO)
			{
        	if(CHECK_PT_ORIENT < BLOCK_PT_ORIENT)
					{
			neighbor_x = BLOCK_PT_X;
			neighbor_y = BLOCK_PT_Y + 1;
                                        }
                                else    {
			neighbor_x = BLOCK_PT_X + 1;
			neighbor_y = BLOCK_PT_Y; 
                                        }
                        }

	else if(BLOCK_PT_ORIENT > THREEPIBYTWO)
			{
        	if(CHECK_PT_ORIENT < BLOCK_PT_ORIENT)
					{
			neighbor_x = BLOCK_PT_X - 1;
			neighbor_y = BLOCK_PT_Y;
                                        }
                                else    {
			neighbor_x = BLOCK_PT_X;
			neighbor_y = BLOCK_PT_Y + 1;
                                        }
                        }
 
	else if(BLOCK_PT_ORIENT == 0.0)
			{ 
			neighbor_x = BLOCK_PT_X;
			neighbor_y = BLOCK_PT_Y + 1;
                        }
 
	else if(BLOCK_PT_ORIENT == PIBYTWO)
			{
			neighbor_y = BLOCK_PT_Y;

                if(CHECK_PT_ORIENT < BLOCK_PT_ORIENT) 

			neighbor_x = BLOCK_PT_X + 1;
		else	neighbor_x = BLOCK_PT_X - 1;
                        }

	else if(BLOCK_PT_ORIENT == PI)
			{
			neighbor_x = BLOCK_PT_X;
                if(CHECK_PT_ORIENT < BLOCK_PT_ORIENT) 

			neighbor_y = BLOCK_PT_Y + 1;
		else	neighbor_y = BLOCK_PT_Y - 1;
                        }
 
	else if(BLOCK_PT_ORIENT == THREEPIBYTWO)
			{
			neighbor_y = BLOCK_PT_Y;
                if(CHECK_PT_ORIENT < BLOCK_PT_ORIENT) 
			neighbor_x = BLOCK_PT_X - 1;
		else	neighbor_x = BLOCK_PT_X + 1;
                        }
 
	if(CHECK_PT_Y == neighbor_y && CHECK_PT_X == neighbor_x)
		{
		return(head);
		}

	neighbor_orientation = find_orientation (neighbor_x,
						 neighbor_y);
 
	if(fabs(BLOCK_PT_ORIENT - neighbor_orientation) <=
           fabs(BLOCK_PT_ORIENT - CHECK_PT_ORIENT))
                { 
		return(head);
		}
 

	/* search for neighbor pt's firing angles from list	*/
	if(!search_angles(head, BLOCK_PT, &lower, &higher,
					neighbor_x, neighbor_y))
		{
	/* otherwise	*/
	/* calculate initial firing angles to hit neighbor pt.	*/

	del_x = (double) abs(neighbor_x);
        del_y = (double) abs(neighbor_y);

        dist = sqrt(del_x*del_x + del_y*del_y) * window.ns_res;


 	find_ABabcdisc(&A,&B,&a,&b,&c,&disc,
	 	seg_in_p,dist,row_firept-neighbor_y,
				col_firept+neighbor_x);

	if(disc < 0.0) return(head);

      	find_angles(&lower,&higher,&disc,&a,&b);

	value = (char *) &neighbor_elevation;
	segment_get(seg_in_p, value, 
		row_firept-neighbor_y, col_firept+neighbor_x);

	calculate_initial_angles(&lower, &higher, dist,
		(double) (neighbor_elevation - firept_elev));
		}

 
                /* INTERPOLATION */
 
	i_lower= BLOCK_PT_LOW_ANGLE + (CHECK_PT_ORIENT - 
		 BLOCK_PT_ORIENT)/(neighbor_orientation -
                 BLOCK_PT_ORIENT)*(lower - BLOCK_PT_LOW_ANGLE);

	i_higher= BLOCK_PT_HIGH_ANGLE + (CHECK_PT_ORIENT - 
		  BLOCK_PT_ORIENT)/(neighbor_orientation -
           	  BLOCK_PT_ORIENT)*(higher-BLOCK_PT_HIGH_ANGLE); 


	if(i_lower >= CHECK_PT_LOW_ANGLE) {
		if(obscured(i_lower,i_higher,
			CHECK_PT_LOW_ANGLE,CHECK_PT_HIGH_ANGLE))
        	{
		head = delete(CHECK_PT,head);
	 	return(head);
                }
                        		  }
 
	return(head);
                          
                        } /*  END OF CHECK  */



/****************************************************************/

/*	This function searches for the neighboring pt's 	*/
/*	firing angles from the list of pts			*/

search_angles(head, BLOCK_PT, low, high, neighbor_x, neighbor_y)

	POINT *head, *BLOCK_PT;
	double *low, *high;
	int neighbor_x, neighbor_y;
{
	POINT *SEARCH_PT;

	if( BLOCK_PT_ORIENT == 0.0 || 
		BLOCK_PT_ORIENT == PIBYTWO ||
			BLOCK_PT_ORIENT == PI  || 
				BLOCK_PT_ORIENT == THREEPIBYTWO)
		SEARCH_PT = head;
	else 	SEARCH_PT = BLOCK_PT;

	while(SEARCH_PT != NULL)
	{

	if(neighbor_x == SEARCH_PT_X && 
	   neighbor_y == SEARCH_PT_Y)
		{
		*low  = SEARCH_PT_LOW_ANGLE;
		*high = SEARCH_PT_HIGH_ANGLE;
		return (1);
		}

	SEARCH_PT = NEXT_SEARCH_PT;
	}

	return (0);

}



/****************************************************************/
/*								*/
/*	This function checks whether the blocking pt. obscures	*/
/*	the pt being checked or not				*/

obscured(bp_ltraj,bp_htraj,cp_ltraj,cp_htraj)
 
        double bp_ltraj,bp_htraj,cp_ltraj,cp_htraj;
{
        extern double high_angle;

    /* check if the farther point can be hit by the high traj */
        if(cp_htraj > high_angle) return (1);

/* check if ltraj of bp can block the htraj of the farther pt.*/
        if(bp_ltraj > cp_htraj) return (1);

        /* high trajectory check        */
        if(bp_htraj > high_angle)
                return (0);     /* bp cannot obscure    */

        else
	{
	if(bp_htraj <= cp_htraj) return (1); /* obscured */
	else return (0);
	}

}       /* END OF OBSCURED CHECK       */
