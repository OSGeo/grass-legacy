/* %W% %G% */
#include "gis.h"
#include "method.h"

ask_method ()
{
    char buf[200];
    int nchoices;
    int choice;
    int len;
    int i;

    new_screen ();

    len = 0;
    for (nchoices = 0; menu[nchoices].method; nchoices++)
    {
	if ((i = strlen(menu[nchoices].title)) > len)
	    len = i;
    }

/* display the text and count the choices */

    printf("Please select the method, by number, for computing new cell values\n\n\n");
    for (i = 0; i < nchoices; i++)
    {
	printf("  %2d  ", i+1);
	printf("%-*s - %s\n", len, menu[i].title, menu[i].text);
    }
    printf("\nEnter <exit> to quit\n");
    printf("\n");


    printf("\nchoice> ");

    if(!G_gets(buf)) return -1;

    if (strcmp(buf,"exit") == 0)
	    exit(0);
    if (scan_int (buf, &choice) == 0)
	    return -1;
    if (choice <= 0 || choice > nchoices)
	    return -1;

    choice--;
    new_screen();
    printf("method selected: %s - %s\n", menu[choice].title,menu[choice].text);
    return choice;
}
