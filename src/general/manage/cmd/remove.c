#define MAIN
#include "list.h"
#include "local_proto.h"

int 
main (int argc, char *argv[])
{
    int i,n;
    struct GModule *module;
    struct Option **parm, *p;
    char *name, *mapset;
    char rname[256], rmapset[256];
    int nrmaps;
    char **rmaps, *location_path, *str, buf1[256], buf2[256];
    FILE *fp;

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

    location_path = G__location_path();
    mapset = G_mapset();

    for (n = 0; n < nlist; n++)
    {
	if (parm[n]->answers)
	    for (i = 0; name = parm[n]->answers[i]; i++)
	    {
		if(G_is_reclassed_by(name, mapset, &nrmaps, &rmaps) > 0)
		{
		    fprintf(stderr, "[%s in %s] is reclassed by(remove these first):\n",
					name, mapset);
		    for(; *rmaps; rmaps++)
		        fprintf(stderr, "\t%s\n", *rmaps);
		    continue;
		}
		if(G_is_reclass(name, mapset, rname, rmapset) > 0 &&
		   G_is_reclassed_by(rname, rmapset, &nrmaps, &rmaps) > 0)
		{
		    sprintf (buf1, "%s/%s/cell_misc/%s/reclassed_by",
				    location_path, rmapset, rname);
		    sprintf(buf2, "%s@%s", name, mapset);

		    if(nrmaps == 1 && !strcmp(rmaps[0], buf2))
		    {
			sprintf(buf2, "rm %s", buf1);
			system(buf2);
		    }
		    else
		    {
		        fp = fopen(buf1, "w");
		        for(; *rmaps; rmaps++)
		        {
			    if(strcmp(*rmaps, buf2))
			        fprintf(fp, "%s\n", *rmaps);
		        }
		        fclose(fp);
		    }
		}
		do_remove (n, name);
	    }
    }
    exit(0);
}
