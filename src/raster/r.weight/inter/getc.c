#include <stdio.h>
mygetc (fd)
    FILE *fd;
{
    int c;

    c = getc(fd);
    if (c == EOF)
    {
        printf ("EOF\n");
        exit(0);
    }
    return c;
}
