#define MAIN
#include "list.h"

main (argc,argv) char *argv[];
{
    int n;
    struct Option *new, *old;

    init (argv[0]);

    old = G_define_option();
    old->key = "old";
    old->type = TYPE_STRING;
    old->required = YES;
    old->description = "item to be renamed";

    new = G_define_option();
    new->key = "new";
    new->type = TYPE_STRING;
    new->required = YES;
    new->description = "new name for item";

    n = parse (argc, argv);
    if(!find (n, old->answer, G_mapset()))
    {
	fprintf (stderr, "<%s> not found\n", old->answer);
	exit(1);
    }
    if (G_legal_filename (new->answer) < 0)
    {
	fprintf (stderr, "<%s> illegal name\n", new->answer);
	exit(1);
    }
    if (strcmp (old->answer,new->answer) == 0)
    {
	fprintf (stderr, "names are the same, no rename required\n");
	exit(0);
    }
    do_rename (n, old->answer, new->answer);
    exit(0);
}
