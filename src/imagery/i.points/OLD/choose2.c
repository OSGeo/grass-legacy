#include "globals.h"
/* show user all group files and ask him/her to choose one
 * returns index into group.ref
 */
choose_groupfile()
{
    FILE *fd;
    int n;

    fd = fopen (tempfile1, "w");
    if (fd == NULL)
	G_fatal_error ("Can't open any tempfiles");
    for (n = 0; n < group.ref.nfiles; n++)
	fprintf (fd, "%s in %s\n", group.ref.file[n].name, group.ref.file[n].mapset);
    fclose (fd);

    fd = fopen (tempfile1, "r");
    n = choose_from_list (fd,n, "to reference");
    fclose (fd);

    return n-1;
}
