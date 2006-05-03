#include <string.h>
#include "list.h"
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/glocale.h>

/* 
 *  returns 0 - success
 *          1 - error
 */
int do_remove (int n, char *old)
{
    int i, ret;
    int len;
    char *mapset;
    int result = 0;

    fprintf (stdout,"REMOVE [%s]\n", old);

    len = get_description_len(n);

    hold_signals(1);
    if ( strcmp(list[n].alias, "vect") == 0 ) {
	if ((mapset = G_find_vector2 (old, "")) == NULL)
	    G_fatal_error(_("Vector map <%s> not found"), old);
	ret = Vect_delete ( old );
	if ( ret == -1 ) {
	    G_warning(_("Cannot delete vector %s"), old );
            result = 1;
	}
    } else {
	for (i = 0; i < list[n].nelem; i++)
	{
	    fprintf (stdout," %-*s ", len, list[n].desc[i]);
	    fflush (stdout);

	    switch (G_remove (list[n].element[i], old))
	    {
	    case -1: 
		fprintf (stdout,"COULD NOT REMOVE"); 
                result = 1;
		break;
	    case  0: 
		fprintf (stdout,"MISSING"); 
		break;
	    }
	    fprintf (stdout,"\n");
	}
    }
    if (strcmp (list[n].element[0], "cell") == 0)
    {
	char colr2[50];
	sprintf (colr2, "colr2/%s", G_mapset());
	G_remove (colr2, old);
    }
    hold_signals(0);

    return result;
}
