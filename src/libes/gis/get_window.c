/*
 *************************************************************************
 * G_get_window (window)
 *     struct Cell_head *window
 *
 *      read the current mapset window
 *      dies if error
 *
 *************************************************************************
 * G_get_default_window (window)
 *     struct Cell_head *window
 *
 *      read the default window for the location
 *      dies if error
 *
 *************************************************************************
 * char *
 * G__get_window (window, element, name, mapset)
 *      read the window 'name' in 'element' in 'mapset'
 *      returns NULL if ok, error message if not
 ************************************************************************/

#include <stdlib.h>
#include "G.h"
#include "gis.h"

int G_get_window (struct Cell_head *window )
{
    static int first = 1;
    static struct Cell_head dbwindow ;

    if (first)
    {
	char *err;

	if(err = G__get_window (&dbwindow,"","WIND",G_mapset()))
	{
	    G_free (err);
	    G_fatal_error ("region for current mapset %s\nrun \"g.region\"", err);
	}
    }

    first = 0;
    G_copy ((char *) window, (char *) &dbwindow, sizeof(dbwindow) ) ;

    if (!G__.window_set)
    {
	G__.window_set = 1;
	G_copy((char *) &G__.window, (char *) &dbwindow, sizeof(dbwindow) ) ;
    }

    return 1;
}

int G_get_default_window ( struct Cell_head *window )
{
    char *err;

    if (err = G__get_window (window,"","DEFAULT_WIND","PERMANENT"))
    {
	G_free (err);
	G_fatal_error ("default region %s", err);
    }
    return 1;
}

char *G__get_window ( struct Cell_head *window,
       char *element, char *name, char *mapset)
{
    FILE *fd ;
    char *err;
    char *G__read_Cell_head();

    G_zero ((char *) window, sizeof (struct Cell_head));
    if (!(fd = G_fopen_old (element, name, mapset) ))
    {
/*
char path[1024];
G__file_name (path,element,name,mapset);
fprintf (stderr, "G__get_window(%s)\n",path);
*/
	return G_store ("is not set");
    }

    err = G__read_Cell_head(fd, window, 0);
    fclose (fd);

    if (err)
    {
	char msg[1024];

	sprintf (msg, "is invalid\n%s", err);
	G_free (err);
	return G_store (msg);
    }

    return NULL;
}
