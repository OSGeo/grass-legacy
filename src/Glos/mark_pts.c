/****************************************************************/
/*								*/
/*	mark_visible_points.c	in	~/src/Glos		*/
/*								*/
/*	This function marks all points that are visible in	*/
/*	any one segment on the outputmap			*/
/*								*/
/****************************************************************/

#include "segment.h"
#include "point.h"
#include "gis.h"

#define		PT_TO_MARK_X		PT_TO_MARK->x
#define		PT_TO_MARK_Y		PT_TO_MARK->y
#define		NEXT_PT_TO_MARK		PT_TO_MARK->next
#define		PT_TO_MARK_INCL		PT_TO_MARK->inclination

mark_visible_points(head,seg_out_p,row_viewpt,col_viewpt,
					color_factor,COLOR_SHIFT)

        SEGMENT *seg_out_p;
        struct point *head;
        int row_viewpt,col_viewpt;
        double color_factor, COLOR_SHIFT;
{
        struct point *PT_TO_MARK;
        double p_slope, fabs();
        CELL data ;
        char *value;

	value = (char *) &data;

        PT_TO_MARK =  head;

        while(PT_TO_MARK != NULL)   /* 	loop till end of list 	*/
        {
	segment_get(seg_out_p,value,
               row_viewpt-PT_TO_MARK_Y, PT_TO_MARK_X+col_viewpt);

	if(data != 1){	/* point has not been deleted previously	*/
/* old value	
data = (CELL ) (PT_TO_MARK_INCL* 57.3 * color_factor 
						+ COLOR_SHIFT);
end of old data	*/
		data = (CELL ) (PT_TO_MARK_INCL * 57.325 + 90.0);
        	segment_put(seg_out_p,value, 
	       		row_viewpt-PT_TO_MARK_Y, PT_TO_MARK_X+col_viewpt);
	} 

	PT_TO_MARK = NEXT_PT_TO_MARK; 	/* next visible point 	*/
        }
}

/********* END OF FUNCTION "MARK_VISIBLE_POINTS" ****************/
 
