#include <stdio.h>

readline (fd, buf, len)
    FILE *fd;
    char *buf;
    int len;
{
    int n;
    int c;

    if (feof(fd)) return 0;
    if (len <= 0) return 0;

    len--;
    for (n = 0; n < len; n++)
    {
	c = fgetc(fd);
	if (c <= 0 || c == '\n')
	    break;
	*buf++ = c;
    }
    *buf = 0;
    while (c >= 0 && c != '\n')
	c = fgetc(fd);
    
    return 1;
}
