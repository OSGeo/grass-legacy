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
 *      form nnn in ppp, and is found, name is changed to nnn by G_find_file().
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
    char xname[512], xmapset[512];
    char *pname, *pmapset;
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
	for (n = 0; (pmapset = G__mapset_name(n)); n++)
	    if (access(G__file_name (path, element, pname, pmapset), 0) == 0)
		    return pmapset;
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
 * \brief find a database file
 *
 * Look for the file <b>name</b> under the specified
 * <b>element</b> in the database. The <b>mapset</b> parameter can either be
 * the empty string "", which means search all the mapsets in the user's current
 * mapset search path,\remarks{See Mapset_Search_Path for more details
 * about the search path} or it can be a specific mapset, which means
 * <i>.</i> look for the file only in this one mapset (for example, in the
 * current mapset).
 * If found, the mapset where the file lives is returned. If not found, the NULL
 * pointer is returned.
 * If the user specifies a fully qualified file name, (i.e, a name that also
 * contains the mapset; see Fully_Qualified_File_Names) then
 * <i>G_find_file( )</i> modifies <b>name</b> by eliminating the mapset
 * from the <b>name</b>
 * For example, to find a "paint/labels" file anywhere in the database:
\code
  char name[50];
  char *mapset;
  if ((mapset = G_find_file("paint/labels",name,"")) == NULL)
  // not found
\endcode
 * To check that the file exists in the current mapset:
\code
  char name[50];
  if (G_find_file("paint/labels",name,G_mapset( )) == NULL)
  // not found
\endcode
 *
 *  \param element
 *  \param name
 *  \param mapset
 *  \return char * 
 */

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
	if (G__name_is_fully_qualified(name, xname, xmapset))
            strcpy (name, xname);
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
