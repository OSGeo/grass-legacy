
/****************************************************************************
 *
 * MODULE:       g.rename cmd
 * AUTHOR(S):    CERL (original contributor)
 *               Radim Blazek <radim.blazek gmail.com>, 
 *               Cedric Shock <cedricgrass shockfamily.net>, 
 *               Glynn Clements <glynn gclements.plus.com>, 
 *               Markus Neteler <neteler itc.it>, 
 *               Martin Landa <landa.martin gmail.com>
 * PURPOSE:      
 * COPYRIGHT:    (C) 1994-2007 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/
#define MAIN
#include <stdlib.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include "list.h"
#include "local_proto.h"

int main(int argc, char *argv[])
{
    int i, n;
    struct GModule *module;
    struct Option **parm, *p;
    char *old, *new;
    int nrmaps;
    char *mapset, *location_path, **rmaps;
    int result = EXIT_SUCCESS;

    init(argv[0]);

    module = G_define_module();
    module->keywords = _("general, map management");
    module->description =
	_("Renames data base element files in the user's current mapset.");
    module->overwrite = TRUE;
    
    parm = (struct Option **)G_calloc(nlist, sizeof(struct Option *));

    for (n = 0; n < nlist; n++) {
	char *str;

	p = parm[n] = G_define_option();
	p->key = list[n].alias;
	p->key_desc = "old,new";
	p->type = TYPE_STRING;
	p->required = NO;
	p->multiple = NO;
	G_asprintf(&str, "old,%s,%s", list[n].mainelem, list[n].maindesc);
	p->gisprompt = str;
	G_asprintf(&str, _("%s file(s) to be renamed"), list[n].alias);
	p->description = str;
	if (G_strcasecmp(list[n].mainelem, "cell") == 0 ||
	    G_strcasecmp(list[n].mainelem, "grid3") == 0 ||
	    G_strcasecmp(list[n].mainelem, "vector") == 0 ||
	    G_strcasecmp(list[n].mainelem, "windows") == 0 ||
	    G_strcasecmp(list[n].mainelem, "group") == 0)
	    p->guisection = _("Basic");
	else
	    p->guisection = _("Other");
    }

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    location_path = G__location_path();
    mapset = G_mapset();

    for (n = 0; n < nlist; n++) {
	if (parm[n]->answers == NULL)
	    continue;
	i = 0;
	while (parm[n]->answers[i]) {
	    old = parm[n]->answers[i++];
	    new = parm[n]->answers[i++];
	    if (!find(n, old, mapset)) {
		G_warning(_("%s <%s> not found"), list[n].maindesc, old);
		continue;
	    }
	    if (find(n, new, "") && !(module->overwrite)) {
		G_warning(_("<%s> already exists in mapset <%s>"), new,
			  find(n, new, ""));
		continue;
	    }
	    if (G_legal_filename(new) < 0) {
		G_warning(_("<%s> is an illegal file name"), new);
		continue;
	    }
	    if (G_strcasecmp(old, new) == 0) {
		/* avoid problems on case-insensitive file systems (FAT, NTFS, ...) */
		G_warning(_("%s=%s,%s: files could be the same, no rename possible"),
			  parm[n]->key, old, new);
		continue;
	    }

	    if (G_is_reclassed_to(old, mapset, &nrmaps, &rmaps) > 0) {
		int ptr, l;
		char buf1[256], buf2[256], buf3[256], *str;
		FILE *fp;

		G_message(_("Renaming reclass maps"));

		for (; *rmaps; rmaps++) {
		    G_message(" %s", *rmaps);
		    sprintf(buf3, "%s", *rmaps);
		    if ((str = strchr(buf3, '@'))) {
			*str = 0;
			sprintf(buf2, "%s", str + 1);
		    }
		    else {
			sprintf(buf2, "%s", mapset);
		    }
		    sprintf(buf1, "%s/%s/cellhd/%s", location_path, buf2,
			    buf3);

		    fp = fopen(buf1, "r");
		    if (fp == NULL)
			continue;

		    fgets(buf2, 255, fp);
		    fgets(buf2, 255, fp);
		    fgets(buf2, 255, fp);

		    ptr = ftell(fp);
		    fseek(fp, 0L, SEEK_END);
		    l = ftell(fp) - ptr;

		    str = (char *)G_malloc(l);
		    fseek(fp, ptr, SEEK_SET);
		    fread(str, l, 1, fp);
		    fclose(fp);

		    fp = fopen(buf1, "w");
		    fprintf(fp, "reclass\n");
		    fprintf(fp, "name: %s\n", new);
		    fprintf(fp, "mapset: %s\n", mapset);
		    fwrite(str, l, 1, fp);
		    G_free(str);
		    fclose(fp);
		}
	    }
	    if (do_rename(n, old, new) == 1) {
		result = EXIT_FAILURE;
	    }
	}
    }
    exit(result);
}
