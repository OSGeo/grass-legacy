/****************************************************************/
/*								*/
/*	delete.c	in	~/src/Glos			*/
/*								*/
/*	This function detaches a point data structure from	*/
/*	the linked list and frees memory allocated for it.	*/
/*								*/
/****************************************************************/

#include "point.h"
#include "gis.h"
#include "segment.h"

#define  PT_TO_DELETE_X		  PT_TO_DELETE->x
#define  PT_TO_DELETE_Y		  PT_TO_DELETE->y
#define  PT_NEXT_TO_DELETED	  PT_TO_DELETE->next	
#define  PT_PREVIOUS_TO_DELETED	  PT_TO_DELETE->previous
#define  NEXT_PT_BACK_PTR	  PT_TO_DELETE->next->previous
#define	 PREVIOUS_PT_NEXT_PTR     PT_TO_DELETE->previous->next	

struct point *delete(PT_TO_DELETE,head,seg_out_p,row_viewpt,col_viewpt)

        struct point *PT_TO_DELETE,*head;
        int row_viewpt,col_viewpt;
        SEGMENT *seg_out_p;

{
        CELL data;
        char *value;

/*	mark deleted points by light brownish color	*/
        data = 1;
        value = (char *) &data;
        segment_put(seg_out_p,value,
	   row_viewpt-PT_TO_DELETE_Y,PT_TO_DELETE_X+col_viewpt);

        if(PT_TO_DELETE==head)   /*  first one ?  */
        {
                NEXT_PT_BACK_PTR = NULL;
                head = PT_NEXT_TO_DELETED;
                free(PT_TO_DELETE); 
                return(head);
        }

                         /*  otherwise  */

                NEXT_PT_BACK_PTR = PT_PREVIOUS_TO_DELETED;
                PREVIOUS_PT_NEXT_PTR = PT_NEXT_TO_DELETED;
                free(PT_TO_DELETE); 

                return(head);
 
}

/************* END OF FUNCTION "DELETE" *************************/
