/* %W% %G%
 **********************************************************************
 *
 * G_put_window (window)
 *      write the current mapset window
 **********************************************************************
 *
 * G__put_window (window, dir, name)
 *      write the window 'name' in 'mapset'
 *      returns -1  error
 *               1  ok
 *********************************************************************/

#include "gis.h"
G_put_window (window)
    struct Cell_head *window ;
{
    return G__put_window (window,"", "WIND");
}

G__put_window (window, dir, name)
    struct Cell_head *window ;
    char *dir;
    char *name;
{
    FILE *fd ;

    if (!(fd = G_fopen_new(dir, name)))
	return -1 ;

    fprintf(fd,"proj:       %d\n", window->proj);
    fprintf(fd,"zone:       %d\n", window->zone);
    fprintf(fd,"north:      %.2lf\n", window->north);
    fprintf(fd,"south:      %.2lf\n", window->south);
    fprintf(fd,"east:       %.2lf\n", window->east);
    fprintf(fd,"west:       %.2lf\n", window->west);
    fprintf(fd,"e-w res:    %.2lf\n", window->ew_res);
    fprintf(fd,"n-s res:    %.2lf\n", window->ns_res);

    fclose(fd) ;
    return(1) ;
}
