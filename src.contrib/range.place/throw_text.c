/****************************************************************/
/*								*/
/*	throw_text.c	in	~/src/i_range			*/
/*								*/
/*	This function writes on the graphics screen the text 	*/
/*	string pointed to by "text_ptr" at the location		*/
/*	specified by the raster	coordinates "x" and "y" 	*/
/*                                                              */
/****************************************************************/

int throw_text(text_ptr,x,y)
	char *text_ptr;
	int x,y;
{
	int color;
	
	/*	change color to white				*/
	color= D_translate_color("white");
	R_standard_color(color);

	R_move_abs(x,y);
	R_text(text_ptr);	/*	write text		*/
	R_flush();

	/*	restore previous black color			*/
	color= D_translate_color("black"); 
        R_standard_color(color);

}

/********** END OF FUNCTION "THROW_TEXT" ************************/
