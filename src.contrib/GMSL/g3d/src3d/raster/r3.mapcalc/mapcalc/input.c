#include <stdio.h>
input (file)
    char *file;
{
    FILE *fd;
    int c;
    int any;

    fd = fopen (file, "w");
    if (fd == NULL)
    {
	fprintf (stderr, "can't open temp files\n");
	perror(file);
	exit(1);
    }

    if (isatty(0)) printf ("\nr3.mapcalc> ");

    any = 0;
    while ( (c = getchar()) > 0)
    {
	if (c != ' ' && c != '\t' && c != '\n' && c != '\\')
	    any = 1;
	if (c == '\n')
	    break;
	if (c == '\\')
	{
	    if ((c = getchar()) != '\n')
	    {
		if (c != ' ' && c != '\t' && c != '\n')
		    any=1;
		fputc ((char)c, fd);
	    }
	    else if (isatty(0))
		printf ("...> ");
	}
	else
	    fputc ((char)c, fd);
    }
    fputc ('\n', fd);
    fclose (fd);
    return any;
}
