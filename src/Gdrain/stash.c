/* %W% %G% */
/**************************************************************/
/*                                                            */
/*     stash.c           in    ~/src/Gdrain	              */
/*                                                            */
/*     This routine assigns the values of the command line    */
/*     arguments to the appropriate variables depending       */
/*     on the positions occupied by those arguments on the    */
/*     command line.                                          */
/*                                                            */
/**************************************************************/

#include "gis.h"
#include "stash.h"

#define		POINT		struct point 
#define		NEW_ROW		NEW_START_PT->row
#define		NEW_COL		NEW_START_PT->column
#define		NEW_NEXT	NEW_START_PT->next
#define		NEXT_START_PT	PRESENT_PT->next

extern struct Cell_head window;

stash_away(pos,option)
    int pos;
    char *option;
{
double east, north;
int row, col;
POINT *NEW_START_PT, *PRESENT_PT;

   if (pos == DRAIN_PATH_LAYER) {
        strcpy(drain_path_layer,option);
        return(0);
    }

    if (pos == ELEVATION_LAYER) {
        strcpy(elevation_layer,option);
        return(0);
    }

    if (pos >= START_PT) {
        if(2 != sscanf(option,"%lf,%lf",&east,&north))
             return(-1);
	
	row = (window.north - north) / window.ns_res;
	col = (east - window.west) / window.ew_res;

 
/*   allocate memory to the new starting cell  */
       NEW_START_PT = (POINT *) (malloc (sizeof (POINT)));

/*   assign attributes */
        NEW_ROW = row;
        NEW_COL = col;
	NEW_NEXT= NULL;
    
/*   make a linked list of starting points       */
        if(head_start_pt == NULL) {
            head_start_pt = NEW_START_PT;
	    PRESENT_PT = head_start_pt;
            return(0);
 				}

	else {
	    NEXT_START_PT = NEW_START_PT;
	    PRESENT_PT = NEXT_START_PT ;
	    return(0);
	      }
     }
}
                
/************** END OF FUNCTION "STASH_AWAY" *****************/
