#define MAIN
#include "list.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    int i,n;
    char *mapset;
	struct GModule *module;
    struct Option **parm, *p;
    char *from, *to;

    init (argv[0]);

	module = G_define_module();
	module->description =
		"Copies available data files in the user's current mapset "
		"search path and location to the appropriate element "
		"directories under the user's current mapset.";

    parm = (struct Option **) G_calloc (nlist, sizeof(struct Option *));

    for (n = 0; n < nlist; n++)
    {
        p = parm[n] = G_define_option();
        p->key = list[n].alias;
        p->key_desc="from,to";
        p->type = TYPE_STRING;
        p->required = NO;
        p->multiple = NO;
        p->description = G_malloc (64);
        sprintf (p->description, "%s file(s) to be copied", list[n].alias);
    }

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
            from = parm[n]->answers[i++];
            to   = parm[n]->answers[i++];
            mapset = find (n, from, "");
            if (!mapset)
            {
                fprintf (stderr, "<%s> not found\n", from);
                continue;
            }
            if (G_legal_filename (to) < 0)
            {
                fprintf (stderr, "<%s> illegal name\n", to);
                continue;
            }
            if (strcmp (mapset, G_mapset()) == 0 && strcmp (from, to) == 0)
            {
                fprintf (stderr, "%s=%s,%s: files are the same, no copy required\n",
                    parm[n]->key,from,to);
                continue;
            }
            do_copy (n, from, mapset, to);
        }
    }
    exit(0);
}
