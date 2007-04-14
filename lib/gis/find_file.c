#include <string.h>
#include <unistd.h>
#include <grass/gis.h>

static char *G__find_file (
    const char *element,
    const char *name,
    const char *mapset)
{
    char path[1000];
    char xname[512], xmapset[512];
    const char *pname, *pmapset;
    int n;

    if (*name == 0)
	return NULL;
    *path = 0;

/*
 * if name is in the fully qualified format, split it into
 * name, mapset (overrides what was in mapset)
 */
    if (G__name_is_fully_qualified(name, xname, xmapset))
    {
	pname = xname;
	pmapset = xmapset;
    }
    else
    {
	pname = name;
        pmapset = mapset;
    }

/*
 * reject illegal names and mapsets
 */
    if (G_legal_filename (pname) == -1)
	    return NULL;

    if (pmapset && *pmapset && G_legal_filename (pmapset) == -1)
	    return NULL;

/*
* if no specific mapset is to be searched
* then search all mapsets in the mapset search list
*/
    if (pmapset == NULL || *pmapset == 0)
    {
	int cnt = 0;
	const char *pselmapset = NULL;
	for (n = 0; (pmapset = G__mapset_name(n)); n++) {
	    if (access(G__file_name (path, element, pname, pmapset), 0) == 0) {
		if ( !pselmapset )
		    pselmapset = pmapset;
		else
		    G_warning ("'%s/%s' was found in more mapsets (also found in %s).", element, pname, pmapset);
		cnt++;
	    }
	}
	if ( cnt > 0 ) {
	    /* If the same name exists in more mapsets and print a warning */
	    if ( cnt > 1 ) 
		G_warning ("using '%s@%s'.", pname, pselmapset);
	    return (char *) pselmapset;
	}
    }
/*
 * otherwise just look for the file in the specified mapset.
 * since the name may have been qualified, mapset may point
 * to the xmapset, so we must should it to
 * permanent storage via G_store().
 */
    else
    {
	if (access(G__file_name (path, element, pname, pmapset),0) == 0)
		return G_store (pmapset);
    }
    return NULL;
}



/*!
 * \brief searches for a file from the mapset search list
 *      or in a specified mapset.
 *	returns the mapset name where the file was found.
 *
 *  notes:
 *
 *      If the user specifies a fully qualified element (<i>name@mapset</i>)
 *      which exists, then <i>G_find_file()</i> modifies <b>name</b>
 *      by removing the "@<i>mapset</i>" part.
 *
 *      Rejects all names that begin with "."
 *
 *      If <b>name</b> is of the form nnn in ppp then only mapset ppp
 *      is searched.
 *
 *  \param const char *element   database element (eg, "cell", "cellhd", "colr", etc)
 *  \param char *name            file name to look for
 *  \param const char *mapset    mapset to search. if mapset is ""
 *                               will search in mapset search list
 *
 *  \return char *  pointer to a string with name of mapset
 *              where file was found, or NULL if not found
*/

char *G_find_file (
    const char *element,
    char *name,
    const char *mapset)
{
    char xname[512], xmapset[512];
    const char *pname, *pmapset;
    char *mp;

    if (G__name_is_fully_qualified(name, xname, xmapset))
    {
	pname = xname;
	pmapset = xmapset;
    }
    else
    {
	pname = name;
	pmapset = mapset;
    }

    mp = G__find_file (element, pname, pmapset);

    if (mp && name != pname)
	strcpy(name, pname);

    return mp;
}



/*!
 * \brief searches for a file from the mapset search list
 *      or in a specified mapset. (look but don't touch)
 *	returns the mapset name where the file was found.
 *
 *      Exactly the same as G_find_file() except that if <b>name</b> is in
 *      the form "<i>name@mapset</i>", and is found, G_find_file2() will not
 *      alter <b>name</b> by removing the "@<i>mapset</i>" part.
 *
 *  note:
 *      rejects all names that begin with "."
 *
 *  \param char *element    database element (eg, "cell", "cellhd", "colr", etc)
 *  \param char *name       file name to look for
 *  \param char *mapset     mapset to search. if mapset is ""
 *                         will search in mapset search list
 *
 *  \return char *  pointer to a string with name of mapset
 *              where file was found, or NULL if not found
*/
char *G_find_file2 (
    const char *element,
    const char *name,
    const char *mapset)
{
    return G__find_file (element, name, mapset);
}
