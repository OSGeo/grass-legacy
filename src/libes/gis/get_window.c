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

#include "G.h"

char *G__get_window();

G_get_window (window)
    struct Cell_head *window ;
{
    static int first = 1;
    static struct Cell_head dbwindow ;

    if (first)
    {
	char msg[1024];
	char *err;

	if(err = G__get_window (&dbwindow,"","WIND",G_mapset()))
	{
	    sprintf (msg, "mapset window %s\nrun \"window\"", err);
	    free (err);
	    G_fatal_error (msg);
	}
    }

    first = 0;
    G_copy (window, &dbwindow, sizeof(dbwindow) ) ;

    if (!G__.window_set)
    {
	G__.window_set = 1;
	G_copy(&G__.window, &dbwindow, sizeof(dbwindow) ) ;
    }

    return 1;
}

G_get_default_window (window)
    struct Cell_head *window ;
{
    char msg[1024];
    char *err;

    if (err = G__get_window (window,"","DEFAULT_WIND","PERMANENT"))
    {
	sprintf (msg, "default window %s", err);
	free (err);
	G_fatal_error (msg);
    }
    return 1;
}

char *
G__get_window (window, element, name, mapset)
    struct Cell_head *window ;
    char *element ;
    char *name ;
    char *mapset ;
{
    FILE *fd ;
    char *err;
    char *G__read_Cell_head();

    G_zero (window, sizeof (struct Cell_head));
    if (!(fd = G_fopen_old (element, name, mapset) ))
    {
/*
char path[1024];
G__file_name (path,element,name,mapset);
fprintf (stderr, "G__get_window(%s)\n",path);
*/
	return G_store ("is missing");
    }

    err = G__read_Cell_head(fd, window, 0);
    fclose (fd);

    if (err)
    {
	char msg[1024];

	sprintf (msg, "is invalid\n%s", err);
	free (err);
	return G_store (msg);
    }

    return NULL;
}
