#include "gis.h"


main (argc, argv)
    char *argv[];
{
    char buf[BUFSIZ];

/*DEBUG*/ fprintf (stderr, "Checkpoint 01\n");
    G_gisinit (argv[0]);

    if (argc != 2)
	fprintf (stderr, "Usage: %d filename\n", argv[0]), exit (1);
/*DEBUG*/ fprintf (stderr, "Checkpoint 02\n");

    if (*argv[1] == '/')
    {
	strcpy (buf, argv[1]);
/*DEBUG*/ fprintf (stderr, "Checkpoint 03 '%s'\n", buf);
    }
    else
    {
	if (NULL == getwd (buf))
	    fprintf (stderr, "Fatal error in getwd()\n"), exit (1);
	strcat (buf, "/");
	strcat (buf, argv[1]);
/*DEBUG*/ fprintf (stderr, "Checkpoint 04 '%s'\n", buf);
    }

/*DEBUG*/ fprintf (stderr, "Checkpoint 05 '%s'\n", buf);
    R_open_driver();

/*DEBUG*/ fprintf (stderr, "Top: %d  ", R_screen_top ());
/*DEBUG*/ fprintf (stderr, "Bot: %d  ", R_screen_bot());
/*DEBUG*/ fprintf (stderr, "Left: %d  ", R_screen_left());
/*DEBUG*/ fprintf (stderr, "Right: %d\n", R_screen_rite ());

    R_panel_save (buf, R_screen_top (), R_screen_bot(), 
		       R_screen_left(), R_screen_rite());
    R_close_driver();

/*DEBUG*/ fprintf (stderr, "Checkpoint 06\n");
}
