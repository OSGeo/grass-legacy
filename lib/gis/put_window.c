/*
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
int G_put_window (struct Cell_head *window )
{
    return G__put_window (window,"", "WIND");
}

int G__put_window ( struct Cell_head *window , char *dir, char *name)
{
    FILE *fd ;

    if (!(fd = G_fopen_new(dir, name)))
	return -1 ;

    G__write_Cell_head (fd, window, 0);
    fclose (fd);

    return 1;
}
