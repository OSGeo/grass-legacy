#include "imagery.h"
#include "local_proto.h"

int menu (char *group, int new)
{
    char title[80];
    char buf[128];
    int nfiles;
    struct Ref ref;

    if (new)
	run ("editgroup", group);
    
    if (!I_find_group (group))
	return 1;

    sprintf (buf, "GROUP: %s", group);
    I_location_info (title, buf);
    while (1)
    {
	I_get_group_ref (group, &ref);
	nfiles = ref.nfiles;
	I_free_group_ref (&ref);

	G_clear_screen();
	fprintf (stderr, "%s\n\n", title);

	fprintf (stderr, "       1.     Select a different group\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "       2.     Edit group title\n");
	fprintf (stderr, "\n");
	fprintf (stderr, "       3.     Include new raster files in the group\n");
	if (nfiles > 0)
	{
	    fprintf (stderr, "              or remove raster files from the group\n");
	    fprintf (stderr, "\n");
	    fprintf (stderr, "       4.     Assign colors to the group\n");
	    fprintf (stderr, "\n");
	    fprintf (stderr, "       5.     Create a new subgroup within the group\n");
	}
	fprintf (stderr, "\n");
	fprintf (stderr, "     RETURN   exit\n");
	fprintf (stderr, "\n> ");

	if (!G_gets(buf))
	    continue;
	if (*buf == 0)    /* exit */
	    exit(0);

	G_strip (buf);
	fprintf (stderr, "<%s>\n",buf);

	if (strcmp (buf, "1") == 0)
	    break;
	if (strcmp (buf, "2") == 0)
	    run ("title", group);
	else if (strcmp (buf, "3") == 0)
	    run ("editgroup", group);
	else if (nfiles > 0 && strcmp (buf, "4") == 0)
	    run ("colors", group);
	else if (nfiles > 0 && strcmp (buf, "5") == 0)
	    run ("subgroup", group);
    }

    return 0;
}
