/* %W% %G% */
#include "externs.h"

display_available_mapsets()
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
