/*
 *****************************************************************
 * open routines
 *
 * G__open (element, name, mapset, mode)
 *      char *element         database element name
 *      char *name            map file name
 *      char *mapset          mapset containing map "name"
 *      int mode              0=read, 1=write, 2=read/write
 * 
 *      this is the lowest level open routine.
 *      opens the file 'name' in 'element' ("cell", etc)
 *      in mapset 'mapset' according to the i/o 'mode'
 *
 *      mode = 0 (read) will look for 'name' in 'mapset'
 *               and open the file for read only
 *               the file must exist
 *
 *      mode = 1 (write) will create an empty file 'name' in the
 *               current mapset and open the file for write only
 *               'mapset' ignored
 *
 *      mode = 2 (read and write) will open a file in the
 *               current mapset for reading and writing
 *               creating a new file if necessary
 *               'mapset' ignored
 *
 *      returns: open file descriptor (int)
 *               or -1 could not open
 *
 *******************************************************************
 * G_open_new (element, name)
 *      char *element         database element name
 *      char *name            map file name
 *
 *      creates 'name' in the current mapset and opens it
 *      for write only.
 *
 *      returns: open file descriptor (int)
 *               or -1 could not open
 *
 *******************************************************************
 * G_open_old (element, name, mapset)
 *      char *element         database element name
 *      char *name            map file name
 *      char *mapset          mapset containing map "name"
 *
 *      opens 'name' in 'mapset' for read only.
 *
 *      returns: open file descriptor (int)
 *               or -1 could not open
 *
 *******************************************************************
 * G_fopen_new (element, name)
 *      char *element         database element name
 *      char *name            map file name
 *
 *      creates 'name' in the current mapset and opens it
 *      for write only.
 *
 *      returns: open file descriptor (FILE *)
 *               or NULL could not open
 *
 *******************************************************************
 * G_fopen_old (element, name, mapset)
 *      char *element         database element name
 *      char *name            map file name
 *
 *      opens 'name' in 'mapset' for read only.
 *
 *      returns: open file descriptor (FILE *)
 *               or NULL could not open
 *******************************************************************/

#include "gis.h"

G__open (element, name, mapset, mode)
    char *element;
    char *name;
    char *mapset;
{
    char path[1024];
    char xname[512], xmapset[512];


    G__check_gisinit();

/* READ */
    if (mode == 0)
    {
	if (G__name_is_fully_qualified (name, xname, xmapset))
	{
	    if (strcmp (xmapset, mapset) != 0)
		    return -1;
	    name = xname;
	}
	if (! G_find_file (element, name, mapset))
	    return -1;
	G__file_name (path, element, name, mapset);
	return open (path, 0);
    }
/* WRITE */
    if (mode == 1 || mode == 2)
    {
	if (G__name_is_fully_qualified (name, xname, xmapset))
	{
	    if (strcmp (xmapset, G_mapset()) != 0)
		return -1;
	    name = xname;
	}

	if (G_legal_filename(name) == -1)
	    return -1;

	G__file_name (path, element, name, G_mapset());
	if(mode == 1 || access(path,0) != 0)
	{
	    G__make_mapset_element (element);
	    close (creat (path, 0666));
	}

	return open (path, mode);
    }
    return -1;
}

G_open_new (element, name)
    char *element, *name;
{
    return G__open (element, name, G_mapset(), 1);
}

G_open_old (element, name, mapset)
    char *element, *name, *mapset;
{
    return G__open (element, name, mapset, 0);
}

G_open_update (element, name)
    char *element, *name;
{
    int fd;
    fd = G__open (element, name, G_mapset(), 2);
    if (fd >= 0) lseek (fd, 0L, 2);
    return fd;
}

FILE *
G_fopen_new (element, name)
    char *element, *name;
{
    int fd;

    fd = G__open (element, name, G_mapset(), 1);
    if (fd < 0)
	return (FILE *) 0;

    return fdopen (fd, "w");
}

FILE *
G_fopen_old (element, name, mapset)
    char *element, *name, *mapset;
{
    int fd;

    fd = G__open (element, name, mapset, 0);
    if (fd < 0)
	return (FILE *) 0;

    return fdopen (fd, "r");
}

FILE *
G_fopen_append (element, name)
    char *element, *name;
{
    int fd;

    fd = G__open (element, name, G_mapset(), 2);
    if (fd < 0)
	return (FILE *) 0;
    lseek (fd, 0L, 2);

    return fdopen (fd, "a");
}

FILE *
G_fopen_modify (element, name)
    char *element, *name;
{
    int fd;

    fd = G__open (element, name, G_mapset(), 2);
    if (fd < 0)
	return (FILE *) 0;
    lseek (fd, 0L, 0);

    return fdopen (fd, "r+");
}
