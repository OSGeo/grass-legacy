#include <stdlib.h>
#include <string.h>
#include <grass/glocale.h>
#define MAIN
#include "list.h"
#include "local_proto.h"

int 
main (int argc, char *argv[])
{
    int i,n;
    struct GModule *module;
    struct Option **parm, *p;
    struct Flag *force_flag;
    char *name, *mapset;
    char rname[256], rmapset[256];
    int nrmaps;
    char **rmaps, *location_path;
    FILE *fp;
    int result = EXIT_SUCCESS;
    int force = 0;

    init (argv[0]);

    module = G_define_module();
    module->keywords = _("general, map management");
    module->description =
		_("Removes data base element files from "
		"the user's current mapset.");

    force_flag = G_define_flag();
    force_flag->key         = 'f';
    force_flag->description = _("Force remove");

    parm = (struct Option **) G_calloc (nlist, sizeof(struct Option *));

    for (n = 0; n < nlist; n++)
    {
	p = parm[n] = G_define_option();
	p->key = list[n].alias;
	p->type = TYPE_STRING;
	p->required = NO;
	p->multiple = YES;
	p->gisprompt = G_malloc (64);
	sprintf (p->gisprompt, "old,%s,%s", list[n].mainelem, list[n].maindesc);
	p->description = G_malloc (64);
	sprintf (p->description, _("%s file(s) to be removed"), list[n].alias);
    }

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    location_path = G_location_path();
    mapset = G_mapset();

    if (force_flag->answer)
	force = 1;

    for (n = 0; n < nlist; n++)
    {
	if (parm[n]->answers)
	    for (i = 0; (name = parm[n]->answers[i]); i++)
	    {
		if(G_is_reclassed_to(name, mapset, &nrmaps, &rmaps) > 0)
		{
		    for(; *rmaps; rmaps++) {
                        /* force remove */
                        if ( force ) {
                            G_warning(
                            _("[%s@%s] is a base map for [%s]. Remove forced."),
                                                name, mapset,  *rmaps);
                        }
                        else {
                            G_warning(
                            _("[%s@%s] is a base map. Remove reclassed map first: %s"),
                                                name, mapset,  *rmaps);
                        }
                    }
                    if ( !force ) 
                        continue;
		}
		if(G_is_reclass(name, mapset, rname, rmapset) > 0 &&
		   G_is_reclassed_to(rname, rmapset, &nrmaps, &rmaps) > 0)
		{
		    char path[GPATH_MAX];
		    char *p = strchr(rname, '@');
		    char *qname = G_fully_qualified_name(name, mapset);
		    if (p)
			*p = '\0';
		    G__file_name_misc(path, "cell_misc", "reclassed_to", rname, rmapset);

		    if(nrmaps == 1 && !G_strcasecmp(rmaps[0], qname))
		    {
			
		        if (  remove(path) < 0 ) {
                            G_warning(
                            _("Removing information about reclassed map from [%s@%s] failed"),
                                                rname, rmapset);
                        }

		    }
		    else
		    {
		        if ( (fp = fopen(path, "w")) ) {
                            for(; *rmaps; rmaps++)
                            {
                                if(G_strcasecmp(*rmaps, qname))
                                    fprintf(fp, "%s\n", *rmaps);
                            }
                            fclose(fp);
                        }
                        else {
                            G_warning(
                            _("Removing information about reclassed map from [%s@%s] failed"),
                                                rname, rmapset);

                        }
		    }
		}
		if ( do_remove (n, name) == 1 )
                {
		    result = EXIT_FAILURE;
                }
	    }
    }
    exit(result);
}
