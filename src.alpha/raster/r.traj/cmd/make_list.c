/****************************************************************/
/*								*/
/*	make_list.c 	in 	~/src/Gtraj			*/
/*								*/
/*	This function makes a list of points that lie in the	*/
/*	zone of fire				 		*/
/*								*/
/****************************************************************/

#include "point.h"
#include "segment.h"
#include "gis.h"
#include "radians.h"
#include "constants.h"

POINT *make_list(head,y,x,seg_in_p)

        POINT *head;
        SEGMENT *seg_in_p;
        int x,y;

{
	extern struct Cell_head window;
	extern double tent_max_dist, vel_initial;
	extern double range1, range2;
	extern int row_firept, col_firept, total;
	extern CELL pt_elev, firept_elev;
	extern double high_angle, low_angle;
        double del_x, del_y, dist, sqrt(), orientation, atan(),
				find_orientation();
        POINT *present_point, *make_point();
        double higher,lower;  /* low and high angles of impact*/
        double A,B,a,b,c,
             disc;   /* discriminant in the quadratic equation*/

        del_x= (double) abs(x)  ;
        del_y= (double) abs(y)  ;

        dist = sqrt(del_x*del_x +del_y*del_y) * window.ns_res;

        if(dist > tent_max_dist) return(head);

	orientation = find_orientation(x, y);

        if(orientation < range1 || orientation > range2)
        return(head);   /* out of horizontal angle range        */
 
	/* find initial angles using parabolic trajectory	*/
    	find_ABabcdisc(&A,&B,&a,&b,&c,&disc,
		seg_in_p,dist,row_firept-y,x+col_firept);
 
        if(disc < 0.0) return(head);    /* unhittable   */
 
        find_angles(&lower,&higher,&disc,&a,&b);
 

	/* find initial angles by numerical integration		*/
	calculate_initial_angles(&lower,&higher,dist,
			(double)(pt_elev - firept_elev)); 
 
        if(higher> high_angle  && lower< low_angle)
        return(head);   /* unhittable due to gun parameters     */
                                                                 
        total++;

        if(head== NULL){      /* first point ? */
 
        head= make_point(dist,orientation,y,x,lower,higher);
        present_point = head;
 
        }
 
	else 
	{
	present_point->next =
		make_point(dist,orientation,y,x,lower,higher);
        present_point = present_point->next;
        }
        return(head);
 
        }  /* END OF FUNCTION MAKE_LIST  */

/*************************************************************** */

double find_orientation(x,y)
	int x,y;
{
	int abs();
	double del_x, del_y, atan(), angle;

	del_x =  (double) abs(x); 
        del_y =  (double) abs(y);

	if(del_x == 0.0) angle = PIBYTWO;
        else             angle = atan(del_y/del_x) ;

	     if (x >= 0 && y >= 0);
	else if (x <  0 && y >= 0) angle = PI - angle;
	else if (x <= 0 && y <  0) angle = PI + angle;
	else			   angle = TWOPI - angle;

	return(angle);
}

/****************************************************************/

find_ABabcdisc(A,B,a,b,c,disc,seg_in_p,dist,ii,jj)
        double *A,*B,*a,*b,*c,*disc,dist;
        SEGMENT *seg_in_p;
        int ii,jj;

{
        extern CELL pt_elev;   /* the "y" for any point  */
	extern CELL firept_elev;
	extern double vel_initial;
        char *value;

        value = (char *) &pt_elev;

        segment_get(seg_in_p,value,ii,jj);
        *A = (pt_elev - firept_elev)/dist;
        *B = GRAVITY * dist / 2.0 /vel_initial/ vel_initial;

        *a = *A * *A + 1.0;
        *b = 2.0* *A* *B - 1.0;
        *c = *B * *B;

        *disc = *b* *b - 4.0 * *a * *c;
}


find_angles(low,high,disc,a,b)
        double *low,*high,*disc,*a,*b;
{
        double sqrt(),acos();

        *disc = sqrt(*disc);
        *a = 2.0 * *a;
 
        *high= acos(sqrt((-(*b) - *disc)/(*a)));
        *low = acos(sqrt((-(*b) + *disc)/(*a)));
 
}

/**************************************************************/

 
