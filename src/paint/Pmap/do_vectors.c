#include "gis.h"
#include "vector.h"
#include "misc.h"
do_vectors (after_masking)
{
    FILE *fd;
    int n;

    n = vector.count;
    while (n-- > 0)
    {
	if (after_masking && vector.masked[n])
	    continue;
	if (!after_masking && !vector.masked[n])
	    continue;
	if (verbose > 1)
	{
	    printf ("PAINT: reading vector file <%s in %s> ...",
		vector.name[n], vector.mapset[n]);
	    fflush (stdout);
	}

        fd = G_fopen_vector_old (vector.name[n], vector.mapset[n]);
        if (fd == NULL)
        {
	    char name[100];
	    sprintf (name, "%s in %s",vector.name[n], vector.mapset[n]);
	    error ("vector file", name, "can't open");
            continue;
        }
	dig_init (fd); /* in digit library */

	set_line_style_solid();
	if (vector.hwidth[n])
	{
	    set_color (vector.hcolor[n]);
	    set_width (vector.width[n] + 2*vector.hwidth[n]);
	    vectdraw (fd);
	    dig_rewind (fd); /* in digit library */
	}

	set_width (vector.width[n]);
	if (vector.linestyle[n] != NULL)
	    set_line_style (vector.linestyle[n], vector.colors[n]);
	else
	    set_color (vector.colors[n][0]);
	vectdraw (fd);
        fclose (fd);

	set_line_style_solid();
	if (verbose > 1) printf ("\n");
    }
}
