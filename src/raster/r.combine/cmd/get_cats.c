#include "gis.h"
#include <unistd.h>
#include <stdlib.h>

int get_cats (char *name)
{
    char *mapset ;
    struct Categories cats ;
    CELL cat;
    FILE *fd;
    static char *tempfile = NULL;


    mapset = G_find_file("cell", name, "") ;
    if (!mapset)
    {
	fprintf (stderr, "Cell file [%s] not found\n", name) ;
	return -1 ;
    }

/* read the category data */
    if(G_read_cats(name, mapset, &cats) < 0)
	return 0;

/* run this thru "more" if input is the keyboard */
    fd = NULL;
    if (isatty(0))
    {
	if (tempfile == NULL)
	    tempfile = G_tempfile();
	fd = fopen (tempfile, "w");
	if (fd == NULL)
	    G_warning ("No temp files available");
    }
    if (fd == NULL)
	fd = stdout;

/* write the cat data structure */
    fprintf (fd, "Categories for raster file [%s in %s]\n", name, mapset) ;

    fprintf (fd, "TITLE: %s\n", G_get_cats_title (&cats));

    for (cat = 0; cat <= cats.num; cat++)
	fprintf (fd, "%4ld %s\n", (long)cat, G_get_cat (cat, &cats));

    G_free_cats (&cats);

    if (fd != stdout)
    {
	char command[1024];

	fclose (fd);
	sprintf (command, "$GRASS_PAGER %s", tempfile);
	system(command);
	unlink (tempfile);
    }

/* everything went okay if it got this far */
    return 1 ;
}
