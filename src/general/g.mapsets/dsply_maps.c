#include "externs.h"

display_available_mapsets(verbose)
{
    if (verbose)
	display1();
    else
	display2();
}
static
display1()
{
    int n;

    printf("Available mapsets:");
    for (n = 0; n < nmapsets; n++)
    {
        if (n%4)
            printf(" ");
        else
            printf("\n");
        printf("%2d %-15s", n+1, mapset_name[n]);
    }
    printf("\n");
    if (nmapsets == 0)
        printf("** no mapsets **\n");
    printf("\n");
}
static
display2()
{
    int nleft, len, n;
    char *name;

    nleft = 78;
    for (n = 0; n < nmapsets; n++)
    {
        len = strlen (name = mapset_name[n]);
        if (len > nleft)
        {
            printf("\n");
            nleft = 78;
        }
        printf("%s ", name);
        nleft -= (len + 1);
    }
    printf("\n");
}
