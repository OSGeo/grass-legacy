#define MAIN
#include "list.h"
#include "local_proto.h"

int 
main (int argc, char *argv[])
{
    int i,n;
	struct GModule *module;
    struct Option **parm, *p;
    char *name;

	module = G_define_module();
	module->description =
		"Removes data base element files from "
		"the user's current mapset.";

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
