#include "gis.h"

trailer (window, grid, scale_text)
    struct Cell_head *window;
    int grid;
    char *scale_text;
{
    char buf[400];
    char temp[100];
    char east[50], west[50], north[50], south[50];
    char *G_unit_name();
    int len1, len2;

    Ptext("");
    sprintf (buf, "%-7s %s", "SCALE:", scale_text);
    Ptext(buf);
    Ptext("");

    G_format_northing (window->north, north, window->proj);
    G_format_northing (window->south, south, window->proj);
    G_format_easting  (window->east,  east,  window->proj);
    G_format_easting  (window->west,  west,  window->proj);

    len1 = strlen (west);
    len2 = max (strlen(north), strlen(south));

    sprintf (buf, "%-7s %*s %*s", "", len1, "", len2, north);
    Ptext(buf);

    sprintf (buf, "%-7s %*s %*s %s", "REGION:",  len1, west, len2, "", east);
    if (grid)
    {
        sprintf(temp, "     (grid: %d %s)", grid, G_database_unit_name(grid!=1));
	strcat (buf, temp);
    }
    Ptext(buf);

    sprintf (buf, "%-7s %*s %*s", "", len1, "", len2, south);
    Ptext(buf);
}

static max (a,b) { return a>b?a:b; }
