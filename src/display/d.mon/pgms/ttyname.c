#include <stdio.h>
char *ttyname(f)
{
    FILE *fd, *popen();
    static char name[50];
    char *n;

    *name = 0;
    fd = popen ("tty","r");
    if (fd)
    {
	if (fgets(name, sizeof name, fd))
	    for (n = name; *n; n++)
		if (*n == '\n') *n = 0;
	pclose (fd);
    }
    return name;
}
