#include "include.h"
#include <unistd.h>
#include <stdlib.h>

int list_cats (char *name)
{
    char *mapset ;
    struct Categories pcats ;
    int i ;
    CELL cat;
    struct Histogram histo;
    FILE *fd;
    static char *tempfile = NULL;
    char command[512];

    if (tempfile == NULL)
	tempfile = G_tempfile();

    mapset = G_find_cell (name, "") ;
    if (mapset == NULL)
    {
	fprintf (stdout,"    error: %s not found\n", name) ;
	return(-1) ;
    }

    if (G_read_cats(name, mapset, &pcats) == -1)
    {
	fprintf (stdout,"    error: %s not available\n", name) ;
	return(-1) ;
    }

    fd = fopen (tempfile, "w");
    if (fd == NULL)
    {
	fprintf (stdout,"    error: can't open any temp files\n");
	G_free_cats (&pcats);
	return(-1);
    }
    fprintf (fd, "\nCategories for map: <%s>\n", name) ;
    fprintf (fd, "Title: %s\n", G_get_cats_title (&pcats)) ;

    get_histo (name, mapset, &histo);

    for (i = 0; i < histo.num; i++)
    {
	cat = histo.list[i].cat;
	fprintf (fd, "%4ld  %s\n", (long)cat, G_get_cat (cat, &pcats));
    }
    fclose (fd);

    G_free_histogram (&histo);
    G_free_cats (&pcats);

    sprintf (command, "$GRASS_PAGER %s", tempfile);
    system (command);
    unlink (tempfile);
    return 0;
}
