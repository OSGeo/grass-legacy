/* %W% %G%
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
 * G__get_window (window, element, name, mapset)
 *      read the window 'name' in 'element' in 'mapset'
 *      returns -2  window not found
 *              -1  window format invalid
 *               1  window ok
 ************************************************************************/

#include "G.h"
#include "windround.h"
G_get_window (window)
    struct Cell_head *window ;
{
    static int first = 1;
    static struct Cell_head dbwindow ;

    if (first)
    {
	switch (G__get_window (&dbwindow,"","WIND",G_mapset()))
	{
	case -2:
		G_fatal_error ("mapset window missing. run WINDOW");
		exit (-1);
	case -1:
		G_fatal_error ("mapset window invalid. run WINDOW");
		exit (-1);
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
    switch (G__get_window (window,"","DEFAULT_WIND","PERMANENT"))
    {
    case -2:
	    G_fatal_error ("default window missing");
	    exit (-1);
    case -1:
	    G_fatal_error ("default window invalid");
	    exit (-1);
    }
    return 1;
}

G__get_window (window, element, name, mapset)
    struct Cell_head *window ;
    char *element ;
    char *name ;
    char *mapset ;
{
    FILE *fd ;
    char buff[128] ;
    char label[20] ;
    double value ;


    G_zero (window, sizeof (struct Cell_head));
    if (!( fd = G_fopen_old (element , name, mapset) ))
	    return -2 ;
    
/* Zero the window */
    window->compressed  = -1 ;
    window->format  = 0 ;
    window->rows    = 0 ;
    window->cols    = 0 ;
    window->proj    = -1 ;
    window->zone    = -1 ;
    window->ew_res  = 0.0 ;
    window->ns_res  = 0.0 ;
    window->north   = 0.0 ;
    window->south   = 0.0 ;
    window->east    = 0.0 ;
    window->west    = 0.0 ;

    while (fgets(buff,sizeof buff,fd))
    {
	*label = 0;
	sscanf(buff,"%[^:]:%lf", label, &value) ;
/* truncate value to 2 decimal places */
	sprintf (buff,"%.2lf", value);
	sscanf  (buff, "%lf", &value);
	if (! strncmp(label, "proj", 4))
		window->proj    = (int) value ;
	else if (! strncmp(label, "zone", 4))
		window->zone    = (int) value ;
	else if (! strncmp(label, "nort", 4))
		window->north   = value ;
	else if (! strncmp(label, "sout", 4))
		window->south   = value ;
	else if (! strncmp(label, "west", 4))
		window->west    = value ;
	else if (! strncmp(label, "east", 4))
		window->east    = value ;
	else if (! strncmp(label, "e-w ", 4))
		window->ew_res  = value ;
	else if (! strncmp(label, "n-s ", 4))
		window->ns_res  = value ;
    }

    fclose(fd) ;

/* Check for reasonableness */

    if (window->ew_res > 0)
	window->cols = (window->east - window->west) / window->ew_res + WINDOW_ROUND ;
    else
	return -1;

    if (window->cols <= 0)
	return -1;

    if (window->ns_res > 0)
	window->rows = (window->north - window->south) / window->ns_res + WINDOW_ROUND ;
    else
	return -1;

    if (window->rows <= 0)
	return -1;
    
    return(1) ;
}
