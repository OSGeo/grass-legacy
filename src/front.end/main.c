#include <stdio.h>

static char *bin    = "etc/bin";
static char *real_path[] = {
	"main","alpha","contrib",NULL};
static char *debug_path[] = {
	"debug",NULL};
static char **path;
static char *COMMAND = "cmd";
static char *INTERACTIVE = "inter";

main(argc,argv) char *argv[];
{
	char *me;
	char pgm[1024];
	int i;
	int cmd;
	int exists;
	char *getenv ();
	int Debug_on = 0;

	i = strlen(me = argv[0]);
	while (--i >= 0)
		if (me[i] == '/')
		{
			me += i+1;
			break;
		}
	G_no_gisinit(argv[0]) ;

/* change argv[0] a little bit */
	argv[0] = me;

/* if args on command-line, run command-line version,
 * else interactive version.
 * searching the grass path
 */
	cmd = argc > 1;

/* if input isn't a tty, run command-line version, even if no args */
	if (!isatty(0))
		cmd = 1;


	if (getenv ("GRASS_DEBUG"))
	{
		path = debug_path;
		Debug_on = 1;
	}
	else
		path = real_path;

	/* if there isn't a command-line version, complain */
	exists = 0;
	if (cmd)
	{
		for (i=0; path[i]; i++)
		{
			pgm_name (pgm, path[i], COMMAND, me);
			if (access(pgm, 0) == 0)
			{
				warn(argv[0],path[i]) ;
				execvp (pgm, argv);
				fprintf (stderr, "ERROR: unable to run %s\n", pgm);
				exit(1);
			}

			/* while we are here, check to see if interactive exists -dpg */
			pgm_name(pgm, path[i], INTERACTIVE, me);
			if (access(pgm, 0) == 0)
			{
				exists = 1;
			}
		}
		if (exists)
		{
			fprintf (stderr, "Usage:\n");
			fprintf (stderr, "  %s\n\n", me);
			fprintf (stderr, "    (This command must be run interactively)\n");
			exit(1);
		}
		else
			goto none_found;
	}

	/* interactive, look for interactive or command version */
	for (i=0; path[i]; i++)
	{
		pgm_name(pgm, path[i], INTERACTIVE, me);
		warn(argv[0],path[i]) ;
		execvp (pgm, argv);
		if (access(pgm, 0) == 0)
		{
			fprintf (stderr, "ERROR: unable to run %s\n", pgm);
			exit(1);
		}

		/* if that fails, try the other one */
		pgm_name(pgm, path[i], COMMAND, me);
		warn(argv[0],path[i]) ;
		execvp (pgm, argv);
		if (access(pgm, 0) == 0)
		{
			fprintf (stderr, "ERROR: unable to run %s\n", pgm);
			exit(2);
		}
	}

	/* give up */

none_found:

	/*
    fprintf (stderr, "ERROR: Unable to execute %s\n", me);
    fprintf (stderr, "None of the following programs were found:\n");
    for (i=0; path[i]; i++)
    {
	pgm_name(pgm, path[i], COMMAND, me);
	fprintf (stderr, " %s\n", pgm);
	pgm_name(pgm, path[i], INTERACTIVE, me);
	fprintf (stderr, " %s\n", pgm);
    }
*/
	if (!Debug_on)
	{
		fprintf (stderr, "\n");
		fprintf (stderr, "ERROR: program '%s' cannot be executed because:\n", me);
		fprintf (stderr, "\n");
		fprintf (stderr, "Neither a command line (cmd) or interactive (inter) version was found\n");
		fprintf (stderr, "   in any of the main, alpha, or contrib directories.\n");
		fprintf (stderr, "\n");
	}
	else
	{
		fprintf (stderr, "\n");
		fprintf (stderr, "ERROR: program '%s' cannot be executed because:\n", me);
		fprintf (stderr, "\n");
		fprintf (stderr, "GRASS_DEBUG environment variable is set, and a DEBUG version of\n");
		fprintf (stderr, "   the specified program cannot be found.\n");
		fprintf (stderr, "\n");
	}

	sleep (3);


	exit(3);
}

pgm_name (pgm, dir, subdir, name)
char *pgm, *dir, *subdir, *name;
{
	char *G_gisbase();

	sprintf (pgm, "%s/%s/%s/%s/%s", G_gisbase(), bin, dir, subdir, name);
}

static
warn(cmd, type)
	char *cmd ;
	char *type ;
{
	if (!isatty(0))
		return ;

	if (! strcmp(type,"main"))
		return ;

/*  Can this cuz it is too noisy.  DPG
	if (! strcmp(type,"alpha"))
	{
		fprintf(stderr, "\n### %s is an ALPHA TEST program ###\n\n", cmd) ;
		return ;
	}

	fprintf(stderr, "\n### %s is a CONTRIBUTED program ###\n\n", cmd) ;
	return ;
*/
}
