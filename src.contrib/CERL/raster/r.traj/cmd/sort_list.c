/****************************************************************/
/*								*/
/*	sort_list.c	in 	~/src/Gtraj			*/
/*								*/
/*	This function sorts a list of points in order of 	*/
/*	decreasing distance from the firing point using an	*/
/*	"insertion pointer sort" algorithm.			*/
/*								*/
/****************************************************************/

#include "gis.h"
#include "point.h"
#define		NULL	0

#define         HEAD_BACK_PTR  		head->previous

#define		NEXT_PT			PRES_PT->next
#define         NEXT_PT_BACK_PTR      	PRES_PT->next->previous

#define		TEMP_PT_DISTANCE	TEMP_PT->distance

POINT *sort_list(total,head)

        int total;
        POINT *head;

{
        POINT **ptr, *TEMP_PT, *PRES_PT ;
        int count, k;
        char *malloc();


	/* allocate an array of pointers to POINT structures	*/
	ptr= (POINT **)G_malloc(total*sizeof(POINT *));

	/* initialize pointers to point to all POINT structures */
	PRES_PT = head;
	for(count=0;count<total;count++)
	{
        *(ptr+count)= PRES_PT;
        PRES_PT= NEXT_PT;
        }   

	/* sort the array of pointers				*/
	for(count=1;count<total;count++)
	{
        TEMP_PT= *(ptr+count);
        for(k=count;
        k>0
        && (*(ptr+k-1))->distance < TEMP_PT_DISTANCE;
        k--){
        *(ptr+k)= *(ptr+k-1);
        *(ptr+k-1)= TEMP_PT;
        }
        *(ptr+k)= TEMP_PT;
        }
         

        /*  reordering of POINT list  */
	for(count =0, head = PRES_PT = *ptr;
        			count< (total-1); count++)
	{
        NEXT_PT = *(ptr+count+1);
        PRES_PT = NEXT_PT;
        }
 
        NEXT_PT = NULL;

        free(ptr);
 

        /* assignment of back pointers				*/
        HEAD_BACK_PTR = NULL;                
        PRES_PT = head;

        while(NEXT_PT != NULL)
	{
        NEXT_PT_BACK_PTR = PRES_PT;
        PRES_PT = NEXT_PT;
        }                                    
	
 
        return(head);
        }       

/**************** END OF FUNCTION "SORT_LIST" *******************/
 

