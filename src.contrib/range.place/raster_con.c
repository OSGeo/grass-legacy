/****************************************************************/
/*								*/
/*	raster_conv.c	in	~/src/i_range			*/
/*								*/
/*	These functions convert coordinates expressed in	*/
/*	percentages of the screen to actual raster coordinates.	*/
/*								*/
/****************************************************************/	


int raster_x(percent_x)
        int percent_x;

{       
	extern int scr_top,scr_bot, scr_left, scr_rite ;

	/*	return raster x-coordinate			*/
	return(scr_left+(scr_rite-scr_left)*percent_x/100.0);

}

/********** END OF FUNCTION "RASTER_X" **************************/
 
 
int raster_y(percent_y)
        int percent_y;
 
{ 
 	extern int scr_top, scr_bot, scr_left, scr_rite ; 

	/*	return raster y-coordinate			*/
	return(scr_top+(scr_bot-scr_top)*(100-percent_y)/100.0) ;

}

/********** END OF FUNCTION "RASTER_Y" **************************/ 


