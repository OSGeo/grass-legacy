#include <stdio.h>

int hitreturn (void)
{
	fprintf (stdout,"Hit RETURN -->");
	while (getchar() != '\n')
		;

	return 0;
}
