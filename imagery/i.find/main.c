/************************************************************************
 * usage: i.find location mapset element file [element2 file2]...
 *
 * produces a file containing the names of files of type
 * element (cell, dig, etc) in the search path for the mapset in location
 *
 * the output file is in the format used by i.ask, which does a popup menu
 * of the files, and lets the user select one using the mouse
 *
 * at present this routine requires that both the current location/mapset
 * and the specified location/mapset be valid for the user.
 * I hope to remove this requirement some time.
 *
 * note: the list is created in other file, and when complete it is moved
 *       to the one specified on the command line. This allows programs
 *       to run this command in background and check for completion by
 *       looking for the file.
 *
 *       if there are no files, the list file is not created
 *
 ***********************************************************************/

#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include "gis.h"
#include "glocale.h"
#include "local_proto.h"

char command[1024];


int 
main (int argc, char *argv[])
{
    char *tempfile;
    FILE *fd;
    int n;
    int ok;

    if (argc < 5 || argc%2 == 0)
    {
	G_message(_("usage: %s location mapset element file\n"), argv[0]);
 	exit(1);
    }
    G_gisinit (argv[0]);

/*
 * this code assumes that the SEARCH PATH is not read
 * until we call G__mapset_name() in find()
 */
    tempfile = G_tempfile();

    G__setenv ("LOCATION_NAME", argv[1]);
    G__setenv ("MAPSET", argv[2]);

    for (n = 3; n < argc; n += 2)
    {
/* get this list into a temp file first */
	fd = fopen (tempfile, "w");
	if (fd == NULL)
	{
	    perror (tempfile);
	    exit(1);
	}
	unlink (argv[n+1]);
	ok = find (fd, argv[n]);
	fclose (fd);

/* move the temp file to the real file
 * this allows programs to run i.find in the background
 * and check for completion by looking for the file
 */
	if (ok)
	{
	    sprintf (command, "mv %s %s", tempfile, argv[n+1]);
	    G_system(command);
	}
	unlink (tempfile);
    }

    return 0;
}


int 
find (FILE *fd, char *element)
{
    int len1, len2;
    char *mapset;
    char name[100];
    char *dir;
    int len;
    int n;
    FILE *ls;

    strcpy (command, "ls ");
    dir = command + strlen (command);

    len1 = len2 = 0;
    fseek (fd, 0L, 0);
    fwrite (&len1, sizeof(len1), (size_t)1, fd);
    fwrite (&len2, sizeof(len2), (size_t)1, fd);
    for (n=0; (mapset = G__mapset_name(n)) != NULL; n++)
    {
	G__file_name (dir, element, "", mapset);
	if (access (dir,0) != 0)
	    continue;
	ls = popen (command, "r");
	if (ls == NULL) continue;

	len = strlen (mapset);
	if (len > len2)
	    len2 = len;
	while (fscanf (ls, "%s", name) == 1)
	{
	    fprintf (fd, "%s %s\n", name, mapset);
	    len = strlen (name);
	    if (len > len1)
		len1 = len;
	}
	pclose (ls);
    }
    if (len1 == 0 || len2 == 0)
	return 0;
    fflush (fd);
    fseek (fd, 0L, 0);
    fwrite (&len1, sizeof(len1), (size_t)1, fd);
    fwrite (&len2, sizeof(len2), (size_t)1, fd);
    return 1;
}
