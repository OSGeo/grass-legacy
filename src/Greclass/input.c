#include <stdio.h>
input(buf) char *buf;
{
    char temp1[10], temp2[2];
    if (isatty(0))
	printf ("> ");
    if (gets(buf) == NULL)
	return 0;
    if (sscanf (buf, "%5s%1s",temp1,temp2)==1 &&
        strcmp (temp1,"end")==0) return 0;
    return 1;
}
