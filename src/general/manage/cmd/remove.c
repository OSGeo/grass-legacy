#define MAIN
#include "list.h"

main (argc,argv) char *argv[];
{
    int i,n;
    char *mapset;
    struct Option **parm, *p;
    char *name;

    init (argv[0]);
    parm = (struct Option **) G_calloc (nlist, sizeof(struct Option *));

    for (n = 0; n < nlist; n++)
    {
	p = parm[n] = G_define_option();
	p->key = list[n].alias;
	p->type = TYPE_STRING;
	p->required = NO;
	p->multiple = YES;
	p->description = G_malloc (64);
	sprintf (p->description, "%s file(s) to be removed", list[n].alias);
    }

    if (G_parser(argc, argv))
	exit(1);

    for (n = 0; n < nlist; n++)
    {
	if (parm[n]->answers)
	    for (i = 0; name = parm[n]->answers[i]; i++)
		do_remove (n, name);
    }
    exit(0);
}
