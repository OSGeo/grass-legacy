/***********************************************************
 * char *
 * G_tempfile()
 *
 *   returns a unique temporary (full path) file name
 *   these files are in a temp directory under the
 *   current mapset.
 *
 *   successive calls will generate different names
 *   the names are of the form pid.n where pid is the
 *   programs process id number and n is a unique identifier
 *
 * returns:
 *   pointer to a character string containing the name.
 *   the name is copied to allocated memory and may be
 *   released by the unix free() routine.
 *
 * note:
 *   GIS mapset manangement will clean up the temp directory
 ***********************************************************/

#include "gis.h"

char *
G_tempfile ()
{
    char path[300];
    char name[20];
    static int uniq = 0;
    char *G_store();

    G__make_mapset_element (".tmp");
    do
    {
	sprintf (name, "%d.%d", getpid(), uniq++) ;
	G__file_name (path, ".tmp", name, G_mapset()) ;
    }
    while (access (path, 0) == 0) ;

    return G_store (path);
}
