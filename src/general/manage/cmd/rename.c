#define MAIN
#include "list.h"
#include "local_proto.h"

int 
main (int argc, char *argv[])
{
    int i,n;
    struct GModule *module;
    struct Option **parm, *p;
    struct Flag *overwr;
    char *old, *new;

    init (argv[0]);

	module = G_define_module();
	module->description =
		"To rename data base element files in "
		"the user's current mapset.";

    parm = (struct Option **) G_calloc (nlist, sizeof(struct Option *));

    for (n = 0; n < nlist; n++)
    {
	p = parm[n] = G_define_option();
	p->key = list[n].alias;
	p->key_desc="old,new";
	p->type = TYPE_STRING;
	p->required = NO;
	p->multiple = NO;
	p->description = G_malloc (64);
	sprintf (p->description, "%s file(s) to be renamed", list[n].alias);
    }

    overwr		= G_define_flag();
    overwr->key		= 'o';
    overwr->description	= "Overwrite <new> file(s)";

    if (G_parser(argc, argv))
	exit(1);

/* check for even number of names for each element */
    for (n = 0; n < nlist; n++)
    {
	i = 0;
	if (parm[n]->answers)
	    while (parm[n]->answers[i])
		i++;
	if (i%2) /* must be even number of names */
	{
	    G_usage();
	    exit(1);
	}
    }
    for (n = 0; n < nlist; n++)
    {
	if (parm[n]->answers == NULL)
	    continue;
	i = 0;
	while (parm[n]->answers[i])
	{
	    old = parm[n]->answers[i++];
	    new = parm[n]->answers[i++];
	    if(!find (n, old, G_mapset()))
	    {
		fprintf (stderr, "<%s> not found\n", old);
		continue;
	    }
	    if (!overwr->answer && find (n, new, ""))
	    {
		fprintf (stderr, "<%s> already exists\n", new);
		continue;
	    }
	    if (G_legal_filename (new) < 0)
	    {
		fprintf (stderr, "<%s> illegal name\n", new);
		continue;
	    }
	    if (strcmp (old, new) == 0)
	    {
		fprintf (stderr, "%s=%s,%s: files are the same, no rename required\n",
		    parm[n]->key,old,new);
		continue;
	    }
	    do_rename (n, old, new);
	}
    }
    exit(0);
}
