
	/*---------------------------------------------------------*
	 *               AGNPS/GRASS Interface Project             *
	 *  Developed in the Agriculture Engineering Department    *
	 *                at Purdue University                     *
	 *                        by                               *
	 *         Raghavan Srinivasan and Bernard Engel           *
	 *                                                         *
	 *   (c)Copyright, 1992 Purdue Research Foundation, West   *
	 *   Lafayette, Indiana 47907. All Rights Reserved. Unless *
	 *   permission is granted, this material shall not be     *
	 *   copied, reproduced or coded for reproduction by any   *
	 *   electrical, mechanical or chemical processes,  or     *
	 *   combinations thereof, now known or later developed.   *
	 *---------------------------------------------------------*/

/*	August, 1991  Agricultural Engineering, Purdue University
	Raghavan Srinivasan (srin@ecn.purdue.edu)
	
	chkdata()

	To check the aspect map which is in agnps format for any path holes
	circularity and only one outlet cell from a watershed.

*/

#include "agnps_input.h"


int chkdata()
{
	int	i, j, k, ct;
	int	outlet_num, count, error;
        void clean_up();

	outlet_num = NO;
	error	= NO;
		 
	printf("Start routing through each cell\n");

	for(i = 1; i <= tot_cells; i++)
	{
		count = 0;
		j = i;
	   while(j <=tot_cells && j != 0){
		   ct = j;
		   j = cel[j].rcell_num;
		   if(ct == cel[j].rcell_num){
			printf("Cells %d and %d collide each other\n",ct,j);
			error = YES;
			}

		   if(j == 0){
			if(outlet_num == 0 || outlet_num == ct) {
			  outlet_num = ct;
			  cel[outlet_num].rcell_num = tot_cells+1;
			  break;
			  }
			else{
		          printf("More than one outlet cell %d and %d\n",outlet_num,ct);
			error = YES;
			  }
			 }
		   count = count +1;
		   if (count > tot_cells) {
			printf("Found Circularity in cells %d %d\n",j,ct);
			error = YES;
			break;
			}
	       }
	   }

	for(i = 1; i <= tot_cells; i++){
		if(cel[i].rcell_num == 0){
			printf("Sink holes found in cell number %d\n",i);
			error	= YES;
			}
		}
		if (error == YES) {clean_up(); exit(0);}
		printf("outlet cell number is %d\n",outlet_num);

/* find number of cells accumulating to a cell and the flow direction entering
the cell*/

	for(ct = 1; ct <= tot_cells; ct++){
	k = ct;
	while(k <= tot_cells  && k != 0){
		cel[cel[k].rcell_num].acc = cel[cel[k].rcell_num].acc + 1;
		k = cel[k].rcell_num;
		}
	}
       return 0;
}
