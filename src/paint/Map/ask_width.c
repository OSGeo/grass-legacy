#include <stdio.h>
ask_width(name,fd)
    char *name;
    FILE *fd;
{
    char buf[100];
    char dummy[2];
    int width;

    while(1)
    {
	width = -1;
	printf("enter line width for <<%s>>: ", name);
	input (buf);
	if (sscanf(buf,"%d%1s", &width, dummy) == 1 && width > 0)
	    break;
	printf("\nwidth should be entered as a positive number 'n'\n\n");
    }

    fprintf (fd, "  width %d\n", width);
}
