/****************************************************************
 *
 *	sort_list.c	in 	~/src/Gsurface
 *
 *	This function sorts a list of neighboring points in
 *	in order of increasing distance from the point whose
 *	value is to be interpolated using an "insertion
 *	pointer sort" algorithm.
 *
 ****************************************************************/

#include "gis.h"
#include "point.h"
#define		NULL	0

#define 	NBR			struct neighbor
#define         HEAD_BACK_PTR  		head->previous

#define		NEXT_PT			PRES_PT->next

#define		TEMP_PT_DISTANCE	TEMP_PT->distance

NBR *sort_list(total,head)

        int total;
        NBR *head;

{
        NBR **ptr, *TEMP_PT, *PRES_PT ;
        int count, k;

	/* allocate an array of pointers to NBR structures	*/
	ptr= (NBR **)G_malloc(total*sizeof(NBR *));

	/* initialize pointers to point to all NBR structures 	*/
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
        && (*(ptr+k-1))->distance > TEMP_PT_DISTANCE;
        k--){
        *(ptr+k)= *(ptr+k-1);
        *(ptr+k-1)= TEMP_PT;
        }
        *(ptr+k)= TEMP_PT;
        }
         

        /*  reordering of NBR list  				*/
	for(count =0, head = PRES_PT = *ptr; 
        			count< (total-1); count++)
	{
        NEXT_PT = *(ptr+count+1);
        PRES_PT = NEXT_PT;
        }
 
        NEXT_PT = NULL;

        free(ptr);
 
        return(head);
        }       

/**************** END OF FUNCTION "SORT_LIST" *******************/
 

