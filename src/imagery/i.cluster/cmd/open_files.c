#include "global.h"

open_files () 
{
    char *name, *mapset;
    FILE *fd;
    int n, missing;

    I_init_group_ref (&ref);

    G_strip (group);
    if (!I_find_group (group)) {
      fprintf (stderr, "\nWARNING: group <%s> not found\n", group);
      exit(1);
    }
    G_strip (subgroup);
    if (!I_find_subgroup (group, subgroup)) {
      fprintf(stderr,"\nWARNING: subgroup <%s> not found\n",subgroup);
      exit(1);
    }
    I_free_group_ref (&ref);
    I_get_subgroup_ref (group, subgroup, &ref);

    missing = 0;
    for (n = 0; n < ref.nfiles; n++)
    {
	name = ref.file[n].name;
	mapset = ref.file[n].mapset;
	if (G_find_cell (name, mapset) == NULL)
	{
	    if (!missing)
		fprintf (stderr, "\7\n** The following raster files in subgroup [%s] do not exist\n", subgroup);
	    missing = 1;
	    fprintf (stderr, "       %s\n", G_fully_qualified_name(name, mapset));
	}
    }
    if (missing) exit(1);
    if (ref.nfiles <= 1)
    {
	fprintf (stderr, "Subgroup [%s] ", subgroup);
	if (ref.nfiles <= 0)
	    fprintf (stderr, "doesn't have any files\n");
	else
	    fprintf (stderr, "only has 1 file\n");
	printf ("The subgroup must have at least 2 files to run %s\n", G_program_name());
	exit(1);
    }

    cell = (CELL **) G_malloc (ref.nfiles * sizeof (CELL *));
    cellfd = (int *) G_malloc (ref.nfiles * sizeof (int));
    for (n=0; n < ref.nfiles; n++)
    {
	cell[n] = G_allocate_cell_buf();
	name   = ref.file[n].name;
	mapset = ref.file[n].mapset;
	if ((cellfd[n] = G_open_cell_old (name, mapset)) < 0)
	{
	    fprintf (stderr, "Unable to proceed\n");
	    exit(1);
	}
    }

    I_init_signatures (&in_sig, ref.nfiles);
    if (insigfile) {
      fd = I_fopen_signature_file_old (group, subgroup, insigfile);
      if (fd == NULL)
      {
	fprintf (stderr, "** Can't open seed sigature file <%s> **\n", insigfile);
	exit(1);
      }
      n = I_read_signatures (fd, &in_sig);
      fclose (fd);
      if (n < 0)
      {
	fprintf (stderr, "** Can't read signature file <%s> **\n", insigfile);
	exit(1);
      }
      if (in_sig.nsigs > 255)
      {
	  fprintf (stderr, "** <%s> has too many signatures (limit is 255)\n", insigfile);
	  exit(1);
      }
      maxclass = in_sig.nsigs;
    }
}
