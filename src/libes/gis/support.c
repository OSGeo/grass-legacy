#include "gis.h"
/* open an existing support file.
 * look first in G_mapset()/support/element/mapset/name
 * then in mapset/element/name
 */
G_open_support_old (element, name, mapset)
    char *element ;
    char *name ;
    char *mapset ;
{
    char support[500];
    char rname[256], rmapset[256];

    if (G__name_in_mapset (name, rname, rmapset))
    {
	if (strcmp (rmapset, mapset) != 0)
	    return -1;
	name = rname;
    }

/* look first in G_mapset()/support/element/mapset/name */

    sprintf (support,"support/%s/%s", element, mapset);
    if (G_find_file(support, name, G_mapset()))
	return G_open_old (support, name, G_mapset());

/* look in mapset/element/name */

    return G_open_old (element, name, mapset);
}

FILE *
G_fopen_support_old (element, name, mapset)
    char *element ;
    char *name ;
    char *mapset ;
{
    int fd;

    fd = G_open_support_old (element, name, mapset);
    if (fd < 0)
	return (FILE *) NULL ;
    return fdopen (fd, "r");
}

/* open a new support file.
 * if mapset != G_mapset(), create G_mapset()/support/element/mapset/name
 * else create mapset/element/name
 */
G_open_support_new (element, name, mapset)
    char *element ;
    char *name ;
    char *mapset ;
{
    char support[500];
    char rname[256], rmapset[256];

    if (G__name_in_mapset (name, rname, rmapset))
    {
	if (strcmp (rmapset, mapset) != 0)
	    return -1;
	name = rname;
    }

/* if mapset isn't current mapset, put into support */
    if (strcmp (mapset, G_mapset()) != 0)
    {
	sprintf (support,"support/%s/%s", element, mapset);
	element = support;
    }
    return (G_open_new (element, name));
}

FILE *
G_fopen_support_new (element, name, mapset)
    char *element ;
    char *name ;
    char *mapset ;
{
    int fd;

    fd = G_open_support_new (element, name, mapset);
    if (fd < 0)
	return (FILE *) NULL ;
    return fdopen (fd, "w");
}
