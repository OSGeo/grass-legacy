/****************************************************************/
/*								*/
/*	make_list.c	in	~/src/Glos			*/
/*								*/
/*	This function adds a new point to the point list	*/
/*	for any segment of the map.				*/
/*								*/
/****************************************************************/	

#include "segment.h"
#include "gis.h"
#include "point.h"

#define		NEXT_PT		PRESENT_PT->next

struct point *make_list(head,y,x,seg_in_p,viewpt_elev,quadrant,
					 row_viewpt,col_viewpt)

        int y,x,viewpt_elev,quadrant,row_viewpt,col_viewpt;
        struct point *head;
        SEGMENT *seg_in_p;

{
        double del_x,del_y,dist,sqrt(),orientation,inclination,
			find_orientation(), find_inclination();
        CELL zp;
        char *value;
        struct point *make_point();
        static struct point  *PRESENT_PT;
	extern struct Cell_head window;
	extern double max_dist;

        del_x= abs(x)  ;
        del_y= abs(y)  ;

        dist = sqrt(del_x*del_x +del_y*del_y) * window.ns_res;

	/* if distance from viewpt is greater than the max	*/
	/* range specified, neglect that point			*/
        if(dist > max_dist) return(head);

	/* otherwise find orientation and inclination		*/
	orientation = find_orientation(x,y,quadrant);
	inclination = find_inclination(x,y,viewpt_elev,seg_in_p,
					  row_viewpt,col_viewpt); 
 
	if(head== NULL)	
	{  			    /* 	first point ? 		*/
        	head= make_point(orientation,inclination,y,x);
        	PRESENT_PT = head;
	}
	else 
	{	/*	add new point to tail of list		*/
	NEXT_PT = make_point(orientation,inclination,y,x);
        PRESENT_PT = NEXT_PT ;
	}

        return(head);
 
}  

/********** END OF FUNCTION "MAKE_LIST" *************************/

