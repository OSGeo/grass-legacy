/*
 **********************************************************************
 *  char *
 *  G_find_file (element, name, mapset)
 *        char *element    database element (eg, "cell", "cellhd", etc)
 *        char *name       file name to look for
 *        char *mapset     mapset to search. if mapset is ""
 *                         will search in mapset search list
 *
 *	searches for a file from the mapset search list
 *      or in a specified mapset.
 *	returns the mapset name where the file was found.
 *
 *  returns:
 *      char *  pointer to a string with name of mapset
 *              where file was found, or NULL if not found
 *  note:
 *      rejects all names that begin with .
 *
 *      if name is of the form nnn in ppp then only mapset ppp
 *      is searched
 *
 *  G_find_file2 (element, name, mapset)
 *
 *      exactly the same as G_find_file() except that if name is in the
 *      form nnn in ppp, and is found, name is changed to nnn.
 **********************************************************************/

#include <string.h>
#include <unistd.h>
#include "gis.h"

static char *G__find_file (
    char *element,
    char *name,
    char *mapset)
{
    char path[1000];
    char xname[512];
    char xmapset[512];
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
	if(name && *name)
	    strcpy(name, xname);
	if(mapset && *mapset)
	    strcpy(mapset, xmapset);
    }

/*
 * reject illegal names and mapsets
 */
    if (G_legal_filename (name) == -1)
	    return NULL;

    if (mapset && *mapset && G_legal_filename (mapset) == -1)
	    return NULL;

/*
* if no specific mapset is to be searched
* then search all mapsets in the mapset search list
*/
    if (mapset == NULL || *mapset == 0)
    {
	for (n = 0; mapset = G__mapset_name(n); n++)
	    if (access(G__file_name (path, element, name, mapset), 0) == 0)
		    return mapset;
    }
/*
 * otherwise just look for the file in the specified mapset.
 * since the name may have been qualified, mapset may point
 * to the xmapset, so we must should it to
 * permanent storage via G_store().
 */
    else
    {
	if (access(G__file_name (path, element, name, mapset),0) == 0)
		return G_store (mapset);
    }
    return NULL;
}

char *G_find_file (
    char *element,
    char *name,
    char *mapset)
{
    char *mp;
    char xname[512], xmapset[512];

    mp = G__find_file (element, name, mapset);
    if (mp)
    {
	if (G__name_is_fully_qualified(name, xname, xmapset)){
	    if(name && *name)
	        strcpy (name, xname);
	}
    }

    return mp;
}
char *G_find_file2 (
    char *element,
    char *name,
    char *mapset)
{
    return G__find_file (element, name, mapset);
}
