#include <stdio.h>

int scan_int (char *buf, int *n)
{
	char temp[2];

	*temp = 0;
	if (sscanf (buf, "%d%1s", n, temp) != 1)
		return(0);
	return *temp == 0;
}
