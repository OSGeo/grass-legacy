#define GLOBAL
#include "table.h"

char topline[] = "+-----------------------------------------------------------------------------+";
char midline[] = "|-----------------------------------------------------------------------------|";
char fill[] = "";

char *paramname[7] = 
{
    " # cells  ",
    "% w/ cat 0",
    "% no cat 0",
    "   acres  ",
    " hectares ",
    "   sq kms ",
    " sq miles "
};

int pagelen = 66;
int threshold = 60;
static char Cellfile[30];
static int Gotfile = 0;

static int stash_params ();
static int my_help ();

struct Command_keys keys[] = {
    { "layer", 1 },
    { "units",  2 },
    { "cellfile", 1 },
    { "unit",  2 },
    { NULL, 0 }
};

struct Command_keys real_keys[] = {
    { "number", 0 },
    { "percent", 1 },
    { "percent-0", 2 },
    { "acres", 3 },
    { "hectares", 4 },
    { "kilometers", 5 },
    { "miles", 6 },
};

main(argc, argv) 
    int argc;
    char *argv[];
{
    FILE *fd;
    char mapname[50];
    char *mapset;
    char *tempfile;
    char buf[128];
    int ret;

    G_gisinit (argv[0]);

    G_get_window(&window);

    Gotfile = 0;
    G_set_parse_command_usage (my_help);


    if ((ret = G_parse_command (argc, argv, keys, stash_params))<0)
	my_help (argv[0], keys), exit (-1);
    if (ret > 0)		/* help was requested */
	exit (1);
    if (!Gotfile)
	my_help (argv[0], keys), exit (-1);


    if((pnum = get_params()) > 3)
	tables = (pnum + 2) / 3;
    else
	tables = 1;

    tempfile = G_tempfile () ;

    strcpy (mapname, Cellfile);
    mapset = G_find_cell2 (mapname, "");
    if(mapset == NULL)
    {
	sprintf (buf, "%s - cell file not found", mapname);
	G_fatal_error (buf);
    }
    if(G_read_cats(mapname,mapset,&cats) < 0)
	exit (-1);

    sprintf (buf, "Gstats -C '%s' > %s", Cellfile, tempfile);
    system(buf);
    fd = fopen (tempfile, "r");
    if (fd == NULL)
	G_fatal_error ("unable to create a tempfile");

    page = 1;

    print_cats_report (fd,mapname,mapset);
    fclose (fd);
    unlink (tempfile);
    exit(0);
}

static
my_help (prog, vars)
    char *prog, *vars[];
{
    register int i;

    G_parse_command_usage (prog, vars, USAGE_SHORT);

    fprintf (stderr, "\nunits={ any of :\n");
    for (i = 0 ; i < 7 ; i++)
	fprintf (stderr, "\t\t%-8s\n", real_keys[i]);
    fprintf (stderr, "      }\n");
    return (0);
}

static
stash_params (position, alias)
    int position;
    char *alias;
{
    register int i;

    if (!strcmp (alias, "-"))	/* default is NOTHING */
	return (0);
    if (!strlen (alias))
    {
	return (-1);
    }

    if (position == 1)
    {
	Gotfile = 1;
	strcpy (Cellfile, alias);
    }
    else
    {
	for (i = 0 ; i < 7 ; i++)
	    if (strcmp (alias, real_keys[i].alias) == 0)
	    {
		param[i][0] = 'x';
		break;
	    }
	if (i == 7)
	{
	    return (-1);
	}
    }

    return (0);
}
