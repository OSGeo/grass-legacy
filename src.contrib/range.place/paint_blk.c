/****************************************************************/
/*								*/
/*	paint_block 	in 	~/src/i_range			*/
/*								*/
/*	This function paints a block of the current color	*/
/*	on the graphics monitor whose location is specified	*/	
/*	by the raster coors. of	the top, bottom, left and 	*/
/*	edges.							*/
/*								*/
/****************************************************************/

paint_block(t,b,l,r)
	int t,b,l,r;

{
	register line;

	for(line=t; line<=b; line++)
        {
                R_move_abs(l, line) ;
                R_cont_abs(r, line) ;
        }

}

/*********** END OF FUNCTION "PAINT_BLOCK" **********************/
