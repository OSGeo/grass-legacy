
/*
**  clean_atts_file:  the Dig_atts library uses ascii files. and to allow
**     random access, we have to guarantee that all records are exactly
**	the same size.
**      so we will copy the file and re-write it using the library functions
*/

#include "digit.h"
#include "gis.h"

clean_atts_file (file)
    char *file;
{
    char *tmpfile;
    char command[1024];
    char buf[BUFSIZ];
    FILE *src, *dest;
    char code[100];
    double x, y;
    int cat;



    tmpfile = G_tempfile ();
    sprintf (command, "cp %s %s", file, tmpfile);
    system (command);

    if ((src = fopen (tmpfile, "r")) == NULL)
	    G_fatal_error ("Can't open temp file for read\n");
    if ((dest = fopen (file, "w")) == NULL)
	    G_fatal_error ("Can't open attribute file for write\n");

    while (fgets (buf, BUFSIZ, src) != NULL)
    {
	if (4 != sscanf (buf, " %s %lf %lf %d", code, &x, &y, &cat))
	{
	    fprintf (stderr, "Bad line '%s'\n", buf);
	    continue;
	}

	G_squeeze (code);
	write_att (dest, *code, x, y, cat);
    }
    fclose (src);
    fclose (dest);
    unlink (tmpfile);
}
