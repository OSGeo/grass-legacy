#include <stdio.h>

input(buf, prompt) 
    char *buf, *prompt;
{
    char temp1[10], temp2[2];
    if (isatty(0))
	printf ("%s> ", prompt);
    if (gets(buf) == NULL)
	return 0;
    if (sscanf (buf, "%5s%1s",temp1,temp2)==1)
    {
	G_tolcase (temp1);
        if (strcmp (temp1,"end")==0) return 0;
	if (strcmp (temp1, "exit")==0) exit (1);
    }
    return 1;
}
