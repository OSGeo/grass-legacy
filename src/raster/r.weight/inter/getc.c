#include <stdio.h>
int 
mygetc (FILE *fd)
{
    int c;

    c = getc(fd);
    if (c == EOF)
    {
        fprintf (stdout,"EOF\n");
        exit(0);
    }
    return c;
}
