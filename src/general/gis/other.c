#include "gis.h"
static char *
filename(name, mapset)
	char *name, *mapset;
{
    static char path[1024];
    G__file_name (path, "", name, mapset);
    return path;
}

mapset_permissions(mapset)
    char *mapset;
{
    int stat;

    stat = G__mapset_permissions (mapset);
    if (stat == 1)
    {
	if(access(filename (".lock", mapset), 0) == 0)
	    stat = 0;
    }
    return stat;
}

mapset_message(mapset)
    char *mapset;
{
    if(printfile(filename (".message", mapset)))
	hit_return();
}

mapset_question(mapset)
    char *mapset;
{
    if(printfile(filename(".question", mapset)))
	return G_yes("Select this mapset? ", -1);
    return 1;
}

printfile(name)
    char *name;
{
    int fd;
    int n;
    char buf[1024];

    fd = open (name, 0);
    if (fd < 0) return 0;
    while ((n = read (fd, buf, sizeof buf)) > 0)
	write (1, buf, n);
    close (fd);
    return 1;
}
