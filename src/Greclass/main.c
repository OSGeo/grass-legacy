#include "rule.h"

main(argc, argv) char *argv[];
{
    struct Categories cats;
    char title[256];
    char buf[1024];
    RULE *rules, *tail;
    int i;
    int any;
    char old_name[100], *old_mapset;
    char *new_name;

    if (argc < 3)
    {
	fprintf (stderr, "usage: %s input-layer output-layer [title]\n", argv[0]);
	exit(1);
    }

    G_gisinit (argv[0]);


    strcpy (old_name, argv[1]);
    new_name = argv[2];

    old_mapset = G_find_cell2 (old_name, "");
    if (old_mapset == NULL)
    {
	sprintf (buf, "%s - not found", argv[1]);
	G_fatal_error (buf);
	exit(1);
    }
    if (G_legal_filename(new_name) < 0)
    {
	sprintf (buf, "%s - illegal name", new_name);
	G_fatal_error (buf);
	exit(1);
    }
    if (strcmp(old_name,new_name)==0 && strcmp(old_mapset,G_mapset())== 0)
    {
	G_fatal_error ("input-layer can NOT be the same as output-layer");
	exit(1);
    }

    *title = 0;
    for (i = 3; i < argc; i++)
    {
	if (*title) strcat (title, " ");
	strcat (title, argv[i]);
    }
    G_init_cats (0, "", &cats);
    rules = tail = NULL;
    any = 0;

    while (input(buf))
    {
	switch (parse (buf, &rules, &tail, &cats))
	{
	case -1:
	    if (isatty(0))
	    {
		fprintf (stderr, "illegal reclass rule.");
		fprintf (stderr, " ignored\n");
	    }
	    else
	    {
		strcat (buf, " - invalid reclass rule");
		G_fatal_error (buf);
		exit(1);
	    }
	    break;

	case 0: break;

	default: any = 1; break;
	}
    }

    if (!any)
    {
	if (isatty(0))
	    fprintf (stderr, "no rules specified. %s not created\n", new_name);
	else
	    G_fatal_error ("no rules specified");
	exit(1);
    }

    reclass (old_name, old_mapset, new_name, rules, &cats, title);

    exit(0);
}
