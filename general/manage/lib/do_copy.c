#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"
#include "list.h"

int do_copy (int n, char *old, char *mapset, char *new)
{
    int i, ret;
    int len;
    char command[2048];
    char path[1024];

    G_debug (3, "Copy %s", list[n].alias );
    fprintf (stdout,"COPY [%s] to current mapset as [%s]\n", G_fully_qualified_name(old, mapset), new);

    len = get_description_len(n);

    hold_signals(1);
    if ( strcmp(list[n].alias, "vect") == 0 ) {
	ret = Vect_copy ( old, mapset, new, stderr );
	if ( ret == -1 ) {
	    G_warning ("Cannot copy %s to current mapset as %s", G_fully_qualified_name(old, mapset), new );
	}
    } else {
	for (i = 0; i < list[n].nelem; i++)
	{
	    fprintf (stdout," %-*s ", len, list[n].desc[i]);
	    fflush (stdout);


	    G__make_mapset_element (list[n].element[i]);
	    G__file_name (path, list[n].element[i], old, mapset);
	    if (access (path, 0) != 0)
	    {
		G_remove (list[n].element[i], new);
		fprintf (stdout,"MISSING\n");
		continue;
	    }
	    sprintf (command, "%s/etc/copy '%s' '", G_gisbase(), path);
	    G__file_name (path, list[n].element[i], new, G_mapset());
	    strcat (command, path);
	    strcat (command, "'");
	    system (command);
	    fprintf (stdout,"\n");
	}
    }

/* special case: remove (yes, remove) the secondary color table, if it exists */
    if (strcmp (list[n].element[0], "cell") == 0)
    {
	char colr2[50];

	sprintf (colr2, "colr2/%s", G_mapset());
	G_remove (colr2, new);
    }
    hold_signals(0);

    return 0;
}
