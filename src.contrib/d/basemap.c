/* manage script files in current mapset under d.scripts */

#include "gis.h"
#define SCRIPTS "d.scripts"

save_script (argc, argv) char *argv[];
{
    char file[128];
    char buf[1024];
    FILE *fd;
    char **list;
    int i,count;
    int overwrite;
    char *av;

    overwrite = 0;

    av = argv[0];
    av++;	/* skip first char of command */

    if (*av == 0)
    {
	argc--;
	argv++;
	av = (argc>0)?argv[0]:NULL;
    }
    if (argc > 0 && *av == '!')
    {
	overwrite = 1;
	av++;
    }
    if (argc > 0 && *av == 0)
    {
	argc--;
	argv++;
	av = (argc>0)?argv[0]:NULL;
    }

    if (argc > 0)
    {
	strcpy (file, av);
	if (G_legal_filename (file) < 0)
	{
	    printf ("%s - illegal file name\n", file);
	    argc = 0;
	}
	else if (!overwrite && G_find_file (SCRIPTS, file, G_mapset()))
	{
	    sprintf (buf, "%s - script already exists.", file);
	    if (!isatty(0))
	    {
		printf ("%s\n", buf);
		return 0;
	    }
	    strcat (buf, " Ok to overwrite? ");
	    if (!G_yes (buf, 0))
		argc = 0;
	}
    }
    if (argc == 0)
    {
	if (!isatty(0)) return 0;
	if (!G_ask_any ("Please enter a name for the script",
		file, SCRIPTS, "script", 1))
	    return 0;
    }

    fd = G_fopen_new (SCRIPTS, file);
    if (fd == NULL)
    {
	printf ("%s - can't create\n", file);
	perror("");
	return 0;
    }

    get_key ("bg", &list, &count);
    if (count > 0)
	fprintf (fd, "bg %s\n", list[0]);
    R_pad_freelist (list, count);

    get_key ("cell", &list, &count);
    if (count > 0)
	fprintf (fd, "cell %s\n", list[0]);
    R_pad_freelist (list, count);

    get_key ("script", &list, &count);
    for (i=0; i < count; i++)
	fprintf (fd, "script %s\n", list[i]);
    R_pad_freelist (list, count);

    fclose (fd);
    return 1;
}

restore_script (argc, argv) char *argv[];
{
    char file[128];
    char *av;
    char *mapset;
    int i;

    av = argv[0];
    av++;	/* skip first char of command */

    if (*av == 0)
    {
	argc--;
	argv++;
	av = (argc>0)?argv[0]:NULL;
    }

    if (argc > 0)
    {
	strcpy (file, av);
	for (i = 1; i < argc; i++)
	{
	    strcat (file, " ");
	    strcat (file, argv[i]);
	}
	mapset = G_find_file (SCRIPTS, file, "");
	if (mapset == NULL)
	{
	    printf ("%s - script not found\n", file);
	    if (!isatty(0)) return 0;
	    argc = 0;
	}
    }
    if (argc == 0)
    {
	if (!isatty(0)) return 0;
	mapset = G_ask_old ("Select a script", file, SCRIPTS, "script");
	if (mapset == NULL)
	    return 0;
    }

    if(!read_script (file, mapset, 0)) return 0;
    run_script(0,0);
    return 1;
}

/* read_script. reads script file and sets current script
 * check = 1 check for file existence. if doesn't exist do nothing
 *       = 0 skip this check
 *
 * does NOT run the script 
 */

read_script (file, mapset, check)
    char *file;
    char *mapset;
{
    char buf[1024];
    char key[100];
    char value[1000];
    FILE *fd;

    if (check && !G_find_file (SCRIPTS, file, mapset))
	return 0;
    fd = G_fopen_old (SCRIPTS, file, mapset);
    if (fd == NULL)
    {
	printf ("WARNING - can't open script file [%s]\n", file);
	return 0;
    }
    while (fgets (buf, sizeof buf, fd))
    {
	if (sscanf (buf, "%s %[^\n]", key, value) != 2)
	    continue;
	G_strip (key);
	G_strip (value);
	if (!strcmp (key, "bg"))
	    set_background_color (value);
	else if (!strcmp (key, "cell"))
	    set_cell (value);
	else
	    append_key (key, value);
    }
    fclose (fd);
    return 1;
}

read_basemap()
{
    return (read_script ("basemap", G_mapset(), 1));
}

basemap()
{
    if (read_basemap())
	run_script(0,0);
}
