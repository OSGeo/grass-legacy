#include "gis.h"
#include "vector.h"
#include "misc.h"
#include "Vect.h"

do_vectors (after_masking)
    int after_masking;
{
    int n;
    struct Map_info Map;

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

	Vect_set_open_level (1);	/* necessary? */
	if (0 >= Vect_open_old (&Map, vector.name[n], vector.mapset[n]))
	{
	    char name[100];
	    sprintf (name, "%s in %s",vector.name[n], vector.mapset[n]);
	    error ("vector file", name, "can't open");
	    continue;
	}

	set_line_style_solid();
	if (vector.hwidth[n])
	{
	    set_color (vector.hcolor[n]);
	    set_width (vector.width[n] + 2*vector.hwidth[n]);
	    vectdraw (&Map);
	    Vect_rewind (&Map); /* in digit library */
	}

	set_width (vector.width[n]);
	if (vector.linestyle[n] != NULL)
	    set_line_style (vector.linestyle[n], vector.colors[n]);
	else
	    set_color (vector.colors[n][0]);
	vectdraw (&Map);
	Vect_close (&Map);

	set_line_style_solid();
	if (verbose > 1) printf ("\n");
    }
}
