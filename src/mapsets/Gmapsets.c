/* %W% %G% */
/*  Gmapsets	set current mapset path to   argv[1] ...
 *
 */
#include "gis.h"

struct Command_keys vars[] = {
 { "mapset", 1 },
 { "map",    1 },
 { "m",      1 },
 { NULL,     0 }
};

static int stuffem ();
static char Path[2048];
static nchoices;
int stuffem ();
int myhelp ();

main (argc, argv)
    int argc;
    char *argv[];
{
    char path[1024];
    int n;
    int i;
    int ret;
    int skip;
    char *cur_mapset;
    char **tokens, **p;
    FILE *fp;

    Path[0] = '\0';
    nchoices = 0;

    G_gisinit ("Gmapsets");

    G_set_parse_command_usage (myhelp);
    if ((ret = G_parse_command (argc, argv, vars, stuffem)) < 0)
    {
	fprintf (stderr, "Command line error\n");
	myhelp (argv[0], vars);
	exit (-1);
    }
    if (ret > 0)	/* Help was requested */
	exit (1);

    /* stuffem sets nchoices*/

    if (nchoices == 0)
    {
	goto DISPLAY;
    }

    /* note I'm assuming that mapsets cannot have ' 's in them */
    tokens = G_tokenize (Path, " ");

    fp = G_fopen_new ("", "SEARCH_PATH");
    if (!fp)
    {
	G_fatal_error ("Cannot open SEARCH_PATH for write");
	return (-1);
    }

    cur_mapset = G_mapset();

/*
* make sure current mapset is specified in the list
* if not add it to the head of the list
*/

    skip = 0;
    for (n = 0; n < nchoices; n++)
	if (strcmp (cur_mapset, tokens[n]) == 0)
	{
	    skip = 1;
	    break;
	}
    if (!skip)
    {
	fprintf (fp, "%s\n", cur_mapset);
    }

/*
* output the list, removing duplicates
*/
    for (n = 0; tokens[n] != NULL ; n++)
    {
	skip = 0;
	for (i = 0; i < n; i++)
	    if (strcmp (tokens[i], tokens[n]) == 0)
	    {
		skip = 1;
		break;
	    }
	if (!skip)
	{
	    fprintf (fp,"%s\n", tokens[n]);
	}
    }

    fclose (fp);
    G_free_tokens (tokens);

DISPLAY:
    _display_mapset_path ();
    return (0);
}

static
stuffem (position, mapset)
    int position;
    char *mapset;
{
    if (G__mapset_permissions (mapset) < 0)
    {
	char command[1024];
	fprintf (stderr, "\7ERROR: [%s] - no such mapset\n", mapset);
	fprintf (stderr, "\nAvailable mapsets:\n\n");
	sprintf (command, "ls -C %s/%s 1>&2", G_gisdbase(), G_location());
	system (command);
	sleep (3);
	exit(1);
    }
    nchoices++;
    strcat (Path, mapset);
    strcat (Path, " ");
    return (0);
}

myhelp (prog, vars)
    char *prog, **vars;
{
    fprintf (stderr, "\nUsage: %s [mapset] [mapset] [...]\n\n", prog);
}
