/****************************************************************/
/*								*/
/*	mark_pts.c	in	~/src/Gtraj			*/
/*								*/
/*	This function marks all points that can be hit by	*/
/*	the projectile in the azimuth range under consideration	*/
/*								*/
/****************************************************************/

#include "segment.h"
#include "point.h"
#include "gis.h"

#define		PT_TO_MARK_X		PT_TO_MARK->x
#define		PT_TO_MARK_Y		PT_TO_MARK->y
#define		NEXT_PT_TO_MARK		PT_TO_MARK->next
#define		PT_TO_MARK_HIGH		PT_TO_MARK->high_angle
#define		PT_TO_MARK_LOW		PT_TO_MARK->low_angle

mark_impact_points(head,seg_out_p,row_firept,col_firept,
		low_angle, high_angle,color_factor,COLOR_SHIFT)

        SEGMENT *seg_out_p;
        POINT *head;
        int row_firept,col_firept;
        double low_angle,high_angle,color_factor,COLOR_SHIFT;
{
        POINT *PT_TO_MARK;
        double fabs(), angle_to_be_marked; 
        CELL data ;
        char *value;

	value = (char *) &data;

        PT_TO_MARK =  head;

        while(PT_TO_MARK != NULL)   /* 	loop till end of list 	*/
        {

	if(PT_TO_MARK_LOW > low_angle)
		angle_to_be_marked = PT_TO_MARK_LOW;
	else	angle_to_be_marked = PT_TO_MARK_HIGH;

        data = (CELL ) (angle_to_be_marked * 57.3 * 
				color_factor + COLOR_SHIFT); 

        segment_put(seg_out_p,value, 
	       row_firept-PT_TO_MARK_Y, PT_TO_MARK_X+col_firept);
		      

	PT_TO_MARK = NEXT_PT_TO_MARK; 	/* next impact point 	*/
        }
}

/********* END OF FUNCTION "MARK_IMPACT_POINTS" *****************/
 
