/****************************************************************/
/*								*/
/*	delete.c	in	~/src/Gtraj			*/
/*								*/
/*	This function detaches a point data structure from	*/
/*	the linked list and frees memory allocated for it.	*/
/*								*/
/****************************************************************/

#include "point.h"
#include "gis.h"
#include "segment.h"

#define  PT_NEXT_TO_DELETED	  PT_TO_DELETE->next	
#define  PT_PREVIOUS_TO_DELETED	  PT_TO_DELETE->previous
#define  NEXT_PT_BACK_PTR	  PT_TO_DELETE->next->previous
#define	 PREVIOUS_PT_NEXT_PTR     PT_TO_DELETE->previous->next	

POINT *delete(PT_TO_DELETE,head)

        POINT *PT_TO_DELETE,*head;

{
        if(PT_TO_DELETE==head)		      /*  first one ?   */
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
