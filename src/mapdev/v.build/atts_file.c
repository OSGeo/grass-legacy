/*
**  clean_atts_file:  the Dig_atts library uses ascii files. and to allow
**     random access, we have to guarantee that all records are exactly
**	the same size.
**      so we will copy the file and re-write it using the library functions
*/

#include <unistd.h>
#include "gis.h"
#include "Vect.h"
#include "vbuildlib.h"

int clean_atts_file (char *file)
{
    char *tmpfile;
    char buf[BUFSIZ];
    FILE *src, *dest;
    char code[100];
    double x, y;
    int cat;



    tmpfile = G_tempfile ();
    /*
    sprintf (command, "cp %s %s", file, tmpfile);
    system (command);
    */
    if (0 > cp_file (file, tmpfile))
	return 0;

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

    return 0;
}

int cp_file (char *from, char *to)
{
    FILE *in, *out;
    char buf[BUFSIZ];
    int red;
    int no_file = 0;

    if (NULL == (in = fopen (from, "r")))
    {
	no_file = 1;
    }
    if (NULL == (out = fopen (to, "w")))
    {
	if (!no_file)
	    fclose (in);
	return (-1);
    }

    if (!no_file)
    {
	while (red = fread (buf, 1, BUFSIZ, in))
	    fwrite (buf, 1, red, out);
	fclose (in);
    }
    fclose (out);

    return (0);
}
