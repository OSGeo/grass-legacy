#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int fontmap (char *mapfile, int map[])
{
    FILE *fd;
    int n;
    int a,b;
    char buf[200];


    if (strcmp (mapfile, "ls") == 0)
    {
	system ("cd ../fonts; ls *.hmp | sed 's/.hmp//' | fmt");
	return -1;
    }
    sprintf (buf, "../fonts/%s.hmp", mapfile);
	fprintf (stdout,"%s\n", buf) ;
    n = 0;
    fd = fopen (buf, "r");
    if (!fd)
    {
	perror (buf);
	return -1;
    }

    while (fscanf (fd, "%s", buf) == 1)
    {
	if (sscanf (buf, "%d-%d", &a, &b) == 2)
	    while (a <= b)
		map[n++] = a++;
	else if (sscanf (buf, "%d", &a) == 1)
	    map[n++] = a;
    }
    fclose (fd);
    return n;
}
