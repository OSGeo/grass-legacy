
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

#include "map_gen.h"

#define Y_BORDER        5
#define X_BORDER        5
#define SIZE        	5

/* this routine is to erase the lable on top of a window
   THis routine is written when the overlay option is used in visualization tool */

int win_label_erase()
{
	int t=0, l=0, b=0, r=0 ;
	int top=0, bot=0, left=0, rite=0;
	int text_size ;
        int len ;
	int dots_per_line ;
	float factor = 0.8;
int D_get_screen_window(), D_translate_color(),R_close_driver();
int R_open_driver(), R_standard_color(), R_box_abs(), R_flush(); 

	len = 50;

	R_open_driver();
	D_get_screen_window(&t,&b,&l,&r);

	top = t;
	bot = b;
	left = l;
	rite = r;


	dots_per_line = (bot - top) * SIZE /100;

	text_size = (int)(factor * (float)dots_per_line) ;

	top = top + Y_BORDER;
	bot = Y_BORDER + top + dots_per_line;
	left = l + X_BORDER;
	rite = left + text_size * len ;
	
	while(rite>r){
		factor = factor - 0.01;
		text_size = (int)(factor* (float)dots_per_line) ;
		rite = left + text_size * len ;
		}

	R_standard_color(D_translate_color("black"));
	R_box_abs(left,top,rite,bot);
	R_flush() ;
	R_close_driver();
        return 0;
}
