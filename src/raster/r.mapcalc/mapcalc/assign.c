#include <stdio.h>
assignment (fd, name)
    FILE *fd;
    char *name;
{
    char *nm;
    int c;

/* assumes that pre-parsing has put something into fd */

    fseek (fd, 0L, 0);
    nm = name;
    while ((c = fgetc(fd)) > 0 && c != '=' && c != '\n')
	*nm++ = c;
    *nm = 0;

    if (c != '=')
    {
	fprintf (stderr, "You must say map = expression\n");
	return 0;
    }

    G_strip (name);
    if (G_legal_filename (name) < 0)
    {
	fprintf (stderr, "%s - illegal name\n", name);
	return 0;
    }

    return 1;
}
