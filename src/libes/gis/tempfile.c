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
 *   It is recommended to unlink() the tempfile on exit/error.
 *   Only if GRASS is left with 'exit', the GIS mapset manangement 
 *   will clean up the temp directory (ETC/clean_temp)
 ***********************************************************/

#include <string.h>
#include <unistd.h>
#include "gis.h"

char *G_tempfile()
{
    return G__tempfile(getpid());
}

char *G__tempfile (int pid)
{
    char path[1024];
    char name[20];
    char element[100];
    static int uniq = 0;

    if (pid <= 0)
	pid = getpid();
    G__temp_element(element);
    do
    {
	sprintf (name, "%d.%d", pid, uniq++) ;
	G__file_name (path, element, name, G_mapset()) ;
    }
    while (access (path, 0) == 0) ;

    return G_store (path);
}

int G__temp_element(char *element)
{
    char *machine;

    strcpy (element, ".tmp");
    machine = G__machine_name();
    if (machine != NULL && *machine != 0)
    {
	strcat (element, "/");
	strcat (element, machine);
    }
    G__make_mapset_element (element);

    return 0;
}
