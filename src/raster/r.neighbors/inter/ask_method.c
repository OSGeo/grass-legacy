#include "gis.h"

static struct
{
	char *name;
	char *description;
} menu[] =
{
    "average",       "average value",
    "median",        "median value",
    "mode",          "most frequently occuring value",
    "minimum",       "lowest value",
    "maximum",       "highest value",
    "stddev",        "statistical standard deviation",
    "variance",      "statistical variance",
    "diversity",     "number of different values",
    "interspersion", "number of values different than center value",
    (char *)NULL, (char *) NULL
} ;
	
ask_method (method)
    char *method;
{
    char buf[200];
    int count;
    int len;
    int n;


    len = 0;
    for (count = 0; menu[count].name; count++)
    {
	if ((n = strlen(menu[count].name)) > len)
	    len = n;
    }

    do
    {
	new_screen ();

	printf("Please select the method, by number, for computing new values\n\n\n");
	for (n = 0; n < count; n++)
	{
	    printf("  %2d  ", n+1);
	    printf("%-*s - %s\n", len, menu[n].name, menu[n].description);
	}
	printf("\nEnter <exit> to quit\n");
	printf("\n");
	printf("\nchoice> ");

	if(!G_gets(buf))
	    continue;

	G_strip(buf);
	if (strcmp(buf,"exit") == 0)
	    exit(0);
    }
    while (!scan_int(buf, &n) || n <= 0 || n > count);

    n--;
    new_screen();
    printf("method selected: %s - %s\n\n", menu[n].name,menu[n].description);
    strcpy (method, menu[n].name);
}
