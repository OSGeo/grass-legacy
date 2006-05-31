#include <stdlib.h>
#include <grass/gis.h>
#include <grass/glocale.h>


void *read_raster(void *buf, const int fd, const RASTER_MAP_TYPE rtype, const int quiet)
{
    void *tmpbuf = buf;
    int rows = G_window_rows();
    int i;

    if (!quiet)
        fprintf(stderr, _("\nReading raster file... "));

    for (i = 0; i < rows; i++) {
        if (!quiet)
            G_percent(i + 1, rows, 10);

        G_get_raster_row(fd, tmpbuf, i, rtype);
        tmpbuf = G_incr_void_ptr(tmpbuf, G_raster_size(rtype) * G_window_cols());
    }

    return tmpbuf;
}


void *write_raster(void *buf, const int fd, const RASTER_MAP_TYPE rtype, const int quiet)
{
    void *tmpbuf = buf;
    int rows = G_window_rows();
    int i;

    if (!quiet)
        fprintf(stderr, _("\nWriting raster file... "));

    for (i = 0; i < rows; i++) {
        if (!quiet)
            G_percent(i, rows, 10);

        G_put_raster_row(fd, tmpbuf, rtype);
        tmpbuf = G_incr_void_ptr(tmpbuf, G_raster_size(rtype) * G_window_cols());
    }

    return tmpbuf;
}

