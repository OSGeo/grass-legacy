#include "menu.h"

menu_handler (menu, buf)

	MENU menu;
	char *buf;
{
	int nchoices;
	int choice;
	int i;

	G_clear_screen ();

/* display the text and count the choices */

	for (i = nchoices = 0; menu[i].text; i++)
	{
		if (menu[i].choice > 0)
			printf("  %2d  ", ++nchoices);
		printf("%s\n", menu[i].text);
	}

/* input users choice. */

	printf("\nchoice> ");

	if (!G_gets (buf))
	{
		*buf = 0;
		return -4;
	}

	if (scan_int (buf, &choice) == 0)
		return -3;
	if (choice <= 0 || choice > nchoices)
		return -2;

	for (i = 0; menu[i].text; i++)
		if (menu[i].choice > 0)
		{
			choice--;
			if (choice <= 0)
			{
				G_clear_screen();
				printf("%s\n", menu[i].text);
				return ( menu[i].choice );
			}
		}
	return -1;
}
