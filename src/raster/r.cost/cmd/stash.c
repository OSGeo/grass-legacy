/**************************************************************/
/*                                                            */
/*     stash_away         in    ~/src/Gcost                   */
/*                                                            */
/*     This routine assigns the values of the command line    */
/*     arguments to the appropriate variables depending       */
/*     on the positions occupied by those arguments on the    */
/*     command line.                                          */
/*                                                            */
/**************************************************************/

#include "cost.h"
#include "gis.h"
#include "stash.h"

extern struct Cell_head window;

stash_away(pos,option)
    int pos;
    char *option;
{
int row, col;
double east, north;
struct start_pt *pres_start_pt, *new_start_pt;

   if (pos == CUM_COST_LAYER) {
        strcpy(cum_cost_layer,option);
        return(0);
    }

    if (pos == COST_LAYER) {
        strcpy(cost_layer,option);
        return(0);
    }

    if (pos >= START_PT) {
        if(2 != sscanf(option,"%lf,%lf",&east,&north))
             return(-1);

	row = (window.north - north) / window.ns_res;
	col = (east - window.west) / window.ew_res;
 
/*   allocate memory to the new starting cell  */
       new_start_pt = (struct start_pt *)(malloc(sizeof(struct start_pt)));

/*   assign attributes */
        new_start_pt->row = row;
        new_start_pt->col = col;
    
/*   start a new linked list ordering cells in        
 *   increasing rows and cols   
 */              
        if(head_start_pt == NULL) {
            head_start_pt = new_start_pt;
            new_start_pt->next = NULL;
            return(0);
         }

         if(head_start_pt->row > row ||
           (head_start_pt->row == row && head_start_pt->col > col)){
              new_start_pt->next = head_start_pt;
              head_start_pt = new_start_pt;
              return(0);
         }

         pres_start_pt = head_start_pt; 
         while(pres_start_pt->next != NULL) {
             if(pres_start_pt->next->row > row ||
                (pres_start_pt->next->row == row &&
                 pres_start_pt->next->col > col)) 
                 break;
              pres_start_pt = pres_start_pt->next;
         }
       
         new_start_pt->next = pres_start_pt->next;
         pres_start_pt->next = new_start_pt;
         return(0);
     }
}
                
/************** END OF FUNCTION "STASH_AWAY" *****************/

         

