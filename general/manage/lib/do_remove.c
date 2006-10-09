#include <string.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/glocale.h>
#include "list.h"

/* 
 *  returns 0 - success
 *          1 - error
 */
int do_remove (int n, char *old)
{
    int i, ret;
    /* int len; */
    char *mapset;
    int result = 0;
    int removed = 0;

    G_message ("Remove <%s>", old);

    /* len = get_description_len(n); */

    hold_signals(1);
    if ( G_strcasecmp(list[n].alias, "vect") == 0 ) {
	ret = Vect_delete ( old );
	if ( ret == -1 ) {
	    result = 1;
	}
	else {
	    removed = 1;
	}
    } else {
	removed = 0;
        for (i = 0; i < list[n].nelem; i++) {

	    switch (G_remove (list[n].element[i], old))
	    {
	    case -1: 
                G_warning ("%s: %s", list[n].desc[i],_("couldn't be removed"));
                result = 1;
		break;
	    case  0: 
		G_debug (1, "%s: %s", list[n].desc[i],_("missing"));
		break;
            case 1:
                G_debug (1, "%s: %s", list[n].desc[i],_("removed"));
		removed = 1;
		break;
	    }
	}
    }

    if (G_strcasecmp (list[n].element[0], "cell") == 0)
    {
	char colr2[50];
	sprintf (colr2, "colr2/%s", G_mapset());
	switch (G_remove (colr2, old))
	{
	case -1: 
	    G_warning ("%s: %s", colr2, _("couldn't be removed"));
	    result = 1;
	    break;
	case  0: 
	    G_debug (1, "%s: %s", colr2, _("missing"));
	    break;
	case 1:
	    G_debug (1, "%s: %s", colr2, _("removed"));
	    removed = 1;
	    break;
	}
    }

    hold_signals(0);

    if (!removed)
	G_warning (_("<%s> nothing removed"), old);
    
    return result;
}
