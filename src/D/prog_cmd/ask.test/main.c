/*
 *   Dask - test program for screen panel save and redraw
 *
 */

#include <stdio.h>

main(argc, argv)
	int argc ;
	char **argv ;
{
	int background_color ;
	int text_color ;
	int choice ;

	static char *options[] = {
		"MAIN MENU",
		"1: A menu in the middle",
		"2: A long menu",
		"3: Quit",
		NULL } ;

	G_gisinit("Dask") ;

	R_open_driver();

	background_color = D_translate_color("white") ;
	text_color       = D_translate_color("black") ;

	for(choice=0; choice != 3;)
	{
		choice = D_popup(
			background_color,
			text_color,
			10,     /* The col of the top left corner */
			10,     /* The row of the top left corner */
			20,     /* The size of the characters in pixles */
			options /* The text */
			) ;

		printf("\nYou chose main option number %d\n", choice) ;

		switch(choice)
		{
		case 1:
			middle_opts() ;
			break ;
		case 2:
			long_opts() ;
			break ;
		case 3:
			break ;
		}
	}

	R_panel_delete("junk") ;
	R_close_driver();
	exit(0) ;
}
