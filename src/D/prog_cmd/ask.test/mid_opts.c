#include <stdio.h>
middle_opts()
{
	int background_color ;
	int text_color ;
	int choice ;
	int T, B, L, R ;

	static char *options[] = {
		"MIDDLE MENU",
		"1: Middle 1",
		"2: Middle 2",
		"3: Middle 3",
		"4: Return",
		NULL } ;

	background_color = D_translate_color("brown") ;
	text_color       = D_translate_color("yellow") ;

	T = (R_screen_top() + R_screen_bot()) / 2 ;
	L = (R_screen_rite() + R_screen_left()) / 2 ;

	for(choice=0; choice != 4;)
	{
		choice = D_popup(
			background_color,
			text_color,
			T,     /* The col of the top left corner */
			L,     /* The row of the top left corner */
			30,     /* The size of the characters in pixles */
			options /* The text */
			) ;

		printf("    You chose MIDDLE option number %d\n", choice) ;
	}
}
