/****************************************************************/	
/*	erase_display.c	  in  ~/src/i_range			*/
/*								*/
/*	This function erases the display window.		*/
/*								*/
/****************************************************************/

struct box *erase_display(box)
	struct box *box;
{
	extern char displayed_map[];
	extern char displayed_pattern[];
	int color;
	int t,b,l,r;

	if(displayed_map[0]!='\0' || displayed_pattern[0]!='\0')
	{
	/*      clear window info   				*/
	D_clear_window();	/*	clear window info	*/

	/*	paint the display window with black color	*/
	D_get_screen_window(&t, &b, &l, &r) ;
	paint_block(t,b-1,l,r-1);
	R_flush();

	/*	set the map and pattern names to NULL		*/
	displayed_map[0] = '\0';
	displayed_pattern[0] = '\0';
	}

	return(box);
}	

/**********  END OF FUNCTION "ERASE_DISPLAY"  *******************/
								
