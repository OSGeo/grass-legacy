#include <stdio.h>

int scan_double (char *buf, double *n)
{
	char temp[2];

	*temp = 0;
	if (sscanf (buf, "%lf%1s", n, temp) != 1)
		return(0);
	return *temp == 0;
}
