#include <string.h>
#include "gis.h"

int main(int argc, char *argv[])
{
    int lister();
    G_gisinit (argv[0]);

    G_list_element ("site_lists", "sites", argc > 1 ? argv[1] : "", lister);
    exit(0);
}

int lister (char *name,char *mapset,char *title)
{
    *title = 0;
    if (*name)
	strcpy (title, G_get_sites_title (name, mapset));

    return 0;
}
