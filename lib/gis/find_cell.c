/*
 **********************************************************************
 *  char *
 *  G_find_cell (name, mapset)
 *        char *name       file name to look for
 *        char *mapset     mapset to search. if mapset is ""
 *                         will search in mapset search list
 *
 *	searches for a cell file from the mapset search list
 *      or in a specified mapset.
 *	returns the mapset name where the cell file was found.
 *
 *  returns:
 *      char *  pointer to a string with name of mapset
 *              where cell file was found, or NULL if not found
 *  note:
 *      rejects all names that begin with .
 *
 *      if name is of the form nnn in ppp then 
 *      name = nnn and mapset = ppp
 **********************************************************************/

#include "gis.h"


/*!
 * \brief find a raster file
 *
 * Looks for the raster file <b>name</b> in the database. The
 * <b>mapset</b> parameter can either be the empty string "", which means
 * search all the mapsets in the user's current mapset search path,\remarks{See
 * Mapset_Search_Path for more details about the search path.} or it
 * can be a specific mapset name, which means look for the raster file only in
 * this one mapset (for example, in the current mapset). If found, the mapset
 * where the raster file lives is returned. If not found, the NULL pointer is
 * returned.
 * If the user specifies a fully qualified raster file which exists, then
 * <i>G_find_cell(~)</i> modifies <b>name</b> by removing the
 * "@<i>mapset</i>".
 * For example, to find a raster file anywhere in the database:
\code
  char name[GNAME_MAX];
  char *mapset;
  if ((mapset = G_find_cell(name,"")) = = NULL)
  // not found
\endcode
 * To check that the raster file exists in the current mapset:
\code
  char name[GNAME_MAX];
  if (G_find_cell(name,G_mapset( )) = = NULL)
  // not found
\endcode
 *
 *  \param name
 *  \param mapset
 *  \return char * 
 */

char *
G_find_cell (name, mapset)
	char *name;
	char *mapset;
{
	return G_find_file ("cell", name, mapset);
}

char *
G_find_cell2 (name, mapset)
	char *name;
	char *mapset;
{
	return G_find_file2 ("cell", name, mapset);
}
