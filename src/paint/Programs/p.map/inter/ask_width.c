#include <stdio.h>
#include "local_proto.h"

int 
ask_width (char *name, FILE *fd)
{
    char buf[100];
    char dummy[2];
    int width;

    while(1)
    {
	width = -1;
	fprintf (stdout,"enter line width for <<%s>>: ", name);
	input (buf);
	if (sscanf(buf,"%d%1s", &width, dummy) == 1 && width > 0)
	    break;
	fprintf (stdout,"\nwidth should be entered as a positive number 'n'\n\n");
    }

    fprintf (fd, "  width %d\n", width);

  return 0;
}
