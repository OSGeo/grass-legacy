#define MAIN
#include "list.h"

main (argc,argv) char *argv[];
{
    int n;
    char *mapset;
    struct
    {
	struct Option *to, *from;
    } parm;

    init (argv[0]);

    parm.from = G_define_option();
    parm.from->key =      "from";
    parm.from->type     = TYPE_STRING;
    parm.from->required = YES;
    parm.from->multiple = NO;
    parm.from->description = "item to be copied";
#define FROM parm.from->answer

    parm.to = G_define_option();
    parm.to->key =      "to";
    parm.to->type     = TYPE_STRING;
    parm.to->required = YES;
    parm.to->multiple = NO;
    parm.to->description = "name to be given to the copy";
#define TO parm.to->answer

    n = parse(argc, argv);

    mapset = find (n, FROM, "");
    if (!mapset)
    {
	fprintf (stderr, "<%s> not found\n", FROM);
	exit(1);
    }
    if (G_legal_filename (TO) < 0)
    {
	fprintf (stderr, "<%s> illegal name\n", TO);
	exit(1);
    }
    if (strcmp (mapset, G_mapset()) == 0 && strcmp (FROM, TO) == 0)
    {
	fprintf (stderr, "%s: files are the same, no copy required\n",
	    G_program_name());
	exit(0);
    }
    do_copy (n, FROM, mapset, TO);
    exit(0);
}
