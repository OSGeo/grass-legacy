/*****************************************************************
 * G__name_in_mapset (name_in, name_out, mapset)
 *
 * checks to see if 'name_in' is in the format: <name> in <mapset>
 *
 * returns
 *    1 (TRUE)  name_in is in this format.
 *              name_out will contain the simple <name>
 *              mapset will contain <mapset>
 *    0 (FALSE) name_in is not in this format
 *              name_out and mapset are undefined (changed)
 ****************************************************************/

#include <string.h>
#include "gis.h"

#ifndef COMMENTED_OUT
int G__name_in_mapset (
    char *name_in,
    char *name_out,
    char *mapset)
{
    char in[1024];

    *in = 0;
    return (sscanf (name_in,"%s %s %s", name_out, in, mapset) == 3 &&
	    strcmp (in,"in") == 0);
}
#endif

int G__name_is_fully_qualified (
    char *fullname,
    char *name,char *mapset)
{
    char *p,*q;

/* search for name@mapset */

    *name = *mapset = 0;

    for (p = fullname; *p ; p++)
	if (*p == '@')
	    break;

    if (*p == 0)
	return 0;

/* copy the name part */
    q = name;
    while (fullname != p)
	*q++ = *fullname++;
    *q = 0;

/* copy the mapset part */
    p++;	/* skip the @ */
    q = mapset;
    while ((*q++ = *p++))
        ;

    return (*name && *mapset);
}


/*!
 * \brief fully qualified file name
 *
 * Returns a fully qualified name for the file <b>name</b> in 
 * <b>mapset.</b> Currently this string is in the form <i>name@mapset</i>, 
 * but the programmer should pretend not to know this and always call this 
 * routine to get the fully qualified name.
 * The following example shows how an interactive version of <i>d.rast</i>
 * interfaces with the command-line version of <i>d.rast</i>:
 \code
  #include "gis.h"
  int main(char *argc, char **argv)
  {
  char name[GNAME_MAX], *mapset, *fqn;
  char command[1024];
  G_gisinit(argv[0]);
  mapset = G_ask_cell_old ("", name, "");
  if (mapset = = NULL) exit(0);
  fqn = G_fully_qualified_name (name, mapset);
  sprintf (command, "d.rast map='%s'", fqn);
  system(command);
  }
 \endcode
 *
 *  \param name
 *  \param mapset
 *  \return char * 
 */

char *
G_fully_qualified_name (name, mapset)
    char *name;
    char *mapset;
{
    char fullname[1024];
    char *G_store();

    if(strchr(name, '@'))
    	sprintf (fullname, "%s", name);
    else
    	sprintf (fullname, "%s@%s", name, mapset);

    return G_store(fullname);
}
