#include <stdio.h>
long_opts()
{
	int background_color ;
	int text_color ;
	int choice ;
	int T, B, L, R ;

	static char *options[] = {
		"LONG MENU",
		"1:  Long  1",
		"2:  Long  2",
		"3:  Long  3",
		"4:  Long  4",
		"5:  Long  5",
		"6:  Long  6",
		"7:  Long  7",
		"8:  Long  8",
		"9:  Long  9",
		"10: Long 10",
		"11: Long 11",
		"12: Long 12",
		"13: Return",
		NULL } ;

	background_color = D_translate_color("red") ;
	text_color       = D_translate_color("black") ;

	T = 20 ;
	L = 20 ;

	for(choice=0; choice != 13;)
	{
		choice = D_popup(
			background_color,
			text_color,
			T,     /* The col of the top left corner */
			L,     /* The row of the top left corner */
			20,     /* The size of the characters in pixles */
			options /* The text */
			) ;

		printf("    You chose LONG option number %d\n", choice) ;
	}
}
