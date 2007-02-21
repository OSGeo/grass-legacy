#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include "list.h"

/*
 *  returns 0 - success
 *          1 - error
 */
int do_rename (int n, char *old, char *new)
{
    int i, ret;
    int len;
    int result = 0;

    /* verbosity: --quiet is completely quiet.
		   GRASS_VERBOSE=1 shows only map names.
		   normal and --verbose show map names and elements. */
    if (G_verbose() > G_verbose_min())
	fprintf (stdout,"RENAME [%s] to [%s]\n", old, new);

    if (strcmp (old,new) == 0) return 1;

    len = get_description_len(n);

    hold_signals(1);
    
    if ( strcmp(list[n].alias, "vect") == 0 ) { 
	ret = Vect_rename ( old, new, stderr );
	if ( ret == -1 ) {
	    G_warning ("Cannot rename %s to %s", old, new );
            result = 1;
	}
    } else {
	for (i = 0; i < list[n].nelem; i++)
	{
	    if (G_verbose() >= G_verbose_std()) {
		fprintf (stdout," %-*s ", len, list[n].desc[i]);
		fflush (stdout);
	    }

	    G_remove(list[n].element[i], new);
	    switch (G_rename (list[n].element[i], old, new))
	    {
	    case -1: 
		fprintf (stdout,"COULD NOT RENAME"); 
		result = 1;
		break;
	    case  0: 
		if (G_verbose() >= G_verbose_std())
		    fprintf (stdout,"MISSING"); 
		break;
	    }
	    if (G_verbose() >= G_verbose_std())
		fprintf (stdout,"\n");
	}
	if (strcmp (list[n].element[0], "cell") == 0)
	{
	    char colr2[50];
	    sprintf (colr2, "colr2/%s", G_mapset());
	    G_remove (colr2, new);
	    G_rename (colr2, old, new);
	}
    }
    hold_signals(0);

    return result;
}
