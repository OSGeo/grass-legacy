/****************************************************************/
/*								*/
/*	transform_pattern.c	in	~/src/i_range		*/
/*								*/
/*	This function transforms all the node, area and		*/
/*	line coordinates of a pattern. The values of		*/
/*	translation and/or rotation are specified by the 	*/
/*	user on the graphics screen by pointing with the 	*/
/*	mouse. The position of the origin of the pattern	*/
/*	is also updated.					*/
/*								*/
/****************************************************************/

#include "dlg.h"

transform_pattern()
{
	int i,j;
	extern struct dlg dlg;
	extern int *n_coors_info;
	extern double *node_coor_info, *area_coor_info,
			**line_coor_info, *line_bounds_info;
	extern double origin_x, origin_y, 
		      translation_x, translation_y;
	double north_bound, south_bound, west_bound, east_bound;

	/* in  'struct dlg_coors' , transformation of boundary	*/
	for(i=0; i<4; i++)
	{
	trans_coors(&(dlg.coors.utm_e[i]), &(dlg.coors.utm_n[i]));
	}

	/*    transformation of coordinates for nodes           */
	for(i=1; i<= dlg.max_nodes; i++)
	{
	trans_coors(node_coor_info+2*i-2, node_coor_info+2*i-1);
	}

	/*      transformation of coordinates for areas         */
	for(i=1; i<= dlg.max_areas; i++)
	{
	trans_coors(area_coor_info+2*i-2, area_coor_info+2*i-1);
	}

	/*      transformation of coordinates for lines		*/
	for(i=1; i<= dlg.max_lines; i++){    /* loop over lines */

	/*	loop over coors of a line			*/
	for(j=1;j<= *(n_coors_info +i-1);j++){ 

	trans_coors(*(line_coor_info+i-1)+2*(j-1),
		    *(line_coor_info+i-1)+2*j-1);

					}

	/* find the bounding N,S,W,E of the transformed line	*/
	 
        /*      initialize   					*/
        north_bound = *(*(line_coor_info + i-1)+1);
        south_bound = north_bound;
        west_bound =  *(*(line_coor_info + i-1)); 
        east_bound = west_bound;

        /*     search over all coors for limits 	        */
        for(j=1;j<= *(n_coors_info+i-1);j++){
        
        if(*(*(line_coor_info+i-1)+2*j-1)>north_bound)
                north_bound = *(*(line_coor_info+i-1)+2*j-1);

        if(*(*(line_coor_info+i-1)+2*j-1)<south_bound)
                south_bound = *(*(line_coor_info+i-1)+2*j-1);

        if(*(*(line_coor_info+i-1)+2*(j-1))>east_bound)
                east_bound = *(*(line_coor_info+i-1)+2*(j-1));
 
        if(*(*(line_coor_info+i-1)+2*(j-1))<west_bound)
                west_bound = *(*(line_coor_info+i-1)+2*(j-1));
 
                                        }
	
	/* assign to the array containing info about line bounds*/
	*(line_bounds_info + 4*i - 4) = north_bound;
	*(line_bounds_info + 4*i - 3) = south_bound;
	*(line_bounds_info + 4*i -2) = west_bound;
	*(line_bounds_info + 4*i -1) = east_bound;
			
			}	/* end of loop over lines	*/

	/*	update position of the origin of pattern	*/
	origin_x += translation_x;
	origin_y += translation_y;
	
}

/*********** END OF FUNCTION "DLG_TRANSFORM" ********************/
