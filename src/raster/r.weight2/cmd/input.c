#include <stdio.h>
#include <unistd.h>
#include "gis.h"

/* buf[512],prompt[50]  in main_loop.c */

int input(char *buf,char *prompt)
{
    char temp1[10], temp2[2];
    if (isatty(0))
	fprintf (stdout,"%s> ", prompt);
    if (fgets(buf,512,stdin) == NULL)
	return 0;
    if (sscanf (buf, "%5s%1s",temp1,temp2)==1)
    {
	G_tolcase (temp1);
        if (strcmp (temp1,"end")==0) return 0;
	if (strcmp (temp1, "exit")==0) exit (1);
    }
    return 1;
}
