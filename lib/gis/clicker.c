
/*-
 * G_clicker()
 * 
 * Print a clock hand (one of '|', '/', '-', '\') to stderr.
 * Used in place of G_percent for unknown number of iterations
 * 
 */
#include <stdio.h>
#include <grass/gis.h>

static int G_clicker_prev = 0;

int G_clicker(void)
{
    int x, format;
    static char clicks[] = "|/-\\";

    /* be verbose only 1> */
    format = G_info_format();
    if (format == G_INFO_FORMAT_SILENT || G_verbose() < 1)
        return 0;

    if (G_clicker_prev == -1 || G_clicker_prev == 3)
	x = 0;
    else
	x = G_clicker_prev + 1;

    fprintf(stderr, "%1c\b", clicks[x]);
    fflush(stderr);
    G_clicker_prev = x;

    return 0;
}
