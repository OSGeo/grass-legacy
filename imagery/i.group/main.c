/* parser support by Bob Covill, Tekmap 2001
 * <bcovill@tekmap.ns.ca>
 */

#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "imagery.h"
#include "local_proto.h"
#include "glocale.h"

static char title[80];

int main(int argc, char *argv[])
{
    char group[INAME_LEN];
    char subgroup[INAME_LEN];
    char **rasters;
    int m, k=0;
    FILE *output;

    struct Option *grp, *rast, *sgrp;
    struct Flag *r, *l;
    struct GModule *module;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->description =
	_("Creates and edits groups and subgroups of imagery files.");

/* Get Args */
    grp = G_define_option();
    grp->key = "group";
    grp->type = TYPE_STRING;
    grp->required = YES;
    grp->gisprompt = "old,group,group";
    grp->description = _("Name of imagery group");

    sgrp = G_define_option();
    sgrp->key = "subgroup";
    sgrp->type = TYPE_STRING;
    sgrp->required = NO;
    sgrp->gisprompt = "old,group,group";
    sgrp->description = _("Name of imagery sub-group");

    rast = G_define_option();
    rast->key = "input";
    rast->type = TYPE_STRING;
    rast->required = NO;   /* why is it NO ?? */
    rast->multiple = YES;
    rast->gisprompt = "old,cell,raster";
    rast->description = _("Name of raster(s) to include in group");

    r = G_define_flag();
    r->key = 'r';
    r->description = _("Remove selected files from specified group");

    l = G_define_flag();
    l->key = 'l';
    l->description = _("List files from specified (sub)group");

    if (G_parser(argc, argv))
	exit(-1);

    G_strip(grp->answer);
    strcpy(group, grp->answer);

    if (sgrp->answer) {
/* Get sub-group */
	G_strip(sgrp->answer);
	strcpy(subgroup, sgrp->answer);
    }

/* Determine number of files to include */
    if (rast->answers) {
	for (m = 0; rast->answers[m]; m++) {
	    k = m;
	}
	k++;
    }
    
    if (k < 1 && !l->answer)  /* remove if input is requirement */
        G_fatal_error( _("No input map(s) specified.") );

    rasters = rast->answers;

    I_location_info(title, argv[0]);

    if (r->answer) {
	/* Remove files from Group */
	if (I_find_group(group) == 0)
	    G_fatal_error(_("Specified group does not exist ... Exiting"));
	if (sgrp->answer) {
	    G_message(_("Removing files from subgroup ...\n"));
	    remove_subgroup_files(group, subgroup, rasters, k);
	}
	else {
	    G_message(_("Removing files from group ...\n"));
	    remove_group_files(group, rasters, k);
	}
    }
    else {
        if (l->answer) {
		output = stdout;
		
		if (sgrp->answer)
			list_subgroup_files(group, subgroup, output);
		else
			list_group_files(group, output);
	}
	else {
		/* Create or update Group REF */
		if (I_find_group(group) == 0) {
		    G_message(_("group %s - does not yet exist...\n"), group);
		    G_message(_("Creating new group %s\n"), group);
		}
		if (sgrp->answer) {
		    G_message(_("Adding files to sub-group\n"));
		    add_or_update_subgroup(group, subgroup, rasters, k);
		}
		else {
		    G_message(_("Adding files to group\n"));
		    add_or_update_group(group, rasters, k);
		}
	}
    }

    return 0;
}

int add_or_update_group(char group[INAME_LEN], char **rasters, int k)
{
    int m, n, skip;
    int nfiles;
    struct Ref ref;
    char *mapset;
    char tmp_name[INAME_LEN];

    I_get_group_ref(group, &ref);
    nfiles = ref.nfiles;

    for (m = 0; m < k; m++) {
	skip = 0;
	strcpy(tmp_name, rasters[m]);
	if ((mapset = G_find_cell(tmp_name, "")) == NULL) {
	    G_fatal_error(_("Unable to find raster <%s>"), tmp_name);
	}
	G_message(_("Adding raster map %s\n"), tmp_name);
	/* Go through existing files to check for duplicates */
	for (n = 0; n < nfiles; n++) {
	    if (strcmp(tmp_name, ref.file[n].name) == 0) {
		G_message(_("Raster map <%s> exists in group\n"), tmp_name);
		G_message(_("Skipping <%s>...\n"), tmp_name);
		skip = 1;
		continue;
	    }
	}
	if (skip == 0)
	    I_add_file_to_group_ref(tmp_name, mapset, &ref);
    }
    G_message(_("Done.\n"));
    I_put_group_ref(group, &ref);

    return 0;
}


int add_or_update_subgroup(char group[INAME_LEN],
			   char subgroup[INAME_LEN], char **rasters, int k)
{
    int m, n, skip;
    int nfiles;
    struct Ref ref;
    char *mapset;
    char tmp_name[INAME_LEN];

    I_get_subgroup_ref(group, subgroup, &ref);
    nfiles = ref.nfiles;

    for (m = 0; m < k; m++) {
	skip = 0;
	strcpy(tmp_name, rasters[m]);
	if ((mapset = G_find_cell(tmp_name, "")) == NULL) {
	    G_fatal_error(_("Unable to find raster <%s>"), tmp_name);
	}
	G_message(_("Adding raster map %s\n"), tmp_name);
	/* Go through existing files to check for duplicates */
	for (n = 0; n < nfiles; n++) {
	    if (strcmp(tmp_name, ref.file[n].name) == 0) {
		G_message(_("Raster map <%s> exists in group\n"), tmp_name);
		G_message(_("Skipping <%s>...\n"), tmp_name);
		skip = 1;
		continue;
	    }
	}
	if (skip == 0)
	    I_add_file_to_group_ref(tmp_name, mapset, &ref);
    }
    G_message(_("Done.\n"));
    I_put_subgroup_ref(group, subgroup, &ref);

    return 0;
}

int remove_group_files(char group[INAME_LEN], char **rasters, int k)
{
    int m, n, skip;
    int nfiles;
    struct Ref ref;
    struct Ref ref_tmp;
    char *mapset;
    char tmp_name[INAME_LEN];
    char xname[512], xmapset[512];

    I_get_group_ref(group, &ref_tmp);
    nfiles = ref_tmp.nfiles;
    I_init_group_ref(&ref);

    /* Parse through supplied rasters */
    for (m = 0; m < k; m++) {
	skip = 0;
	strcpy(tmp_name, rasters[m]);
	mapset = G_mapset();
	/* Parse out mapset */
	if (G__name_is_fully_qualified(tmp_name, xname, xmapset)) {
	    strcpy(tmp_name, xname);
	    strcpy(mapset, xmapset);
	}
	/* Go through existing files to check for duplicates */
	for (n = 0; n < nfiles; n++) {
	    if ((strcmp(tmp_name, ref_tmp.file[n].name) == 0) &&
		(strcmp(mapset, ref_tmp.file[n].mapset) == 0)) {
		G_message(_("Found file %s@%s in Group\n"), tmp_name,
			mapset);
	    }
	    else {
		I_add_file_to_group_ref(ref_tmp.file[n].name,
					ref_tmp.file[n].mapset, &ref);
	    }
	}

    }
    G_message(_("Done ... Put group ref\n"));
    I_put_group_ref(group, &ref);


    return 0;
}


int remove_subgroup_files(char group[INAME_LEN],
			  char subgroup[INAME_LEN], char **rasters, int k)
{
    int m, n, skip;
    int nfiles;
    struct Ref ref;
    struct Ref ref_tmp;
    char *mapset;
    char tmp_name[INAME_LEN];
    char xname[512], xmapset[512];

    I_get_subgroup_ref(group, subgroup, &ref_tmp);
    nfiles = ref_tmp.nfiles;
    I_init_group_ref(&ref);

    /* Parse through supplied rasters */
    for (m = 0; m < k; m++) {
	skip = 0;
	strcpy(tmp_name, rasters[m]);
	mapset = G_mapset();
	/* Parse out mapset */
	if (G__name_is_fully_qualified(tmp_name, xname, xmapset)) {
	    strcpy(tmp_name, xname);
	    strcpy(mapset, xmapset);
	}
	/* Go through existing files to check for duplicates */
	for (n = 0; n < nfiles; n++) {
	    if ((strcmp(tmp_name, ref_tmp.file[n].name) == 0) &&
		(strcmp(mapset, ref_tmp.file[n].mapset) == 0)) {
		G_message(_("Found file %s@%s in Sub-Group\n"), tmp_name,
			mapset);
	    }
	    else {
		I_add_file_to_group_ref(ref_tmp.file[n].name,
					ref_tmp.file[n].mapset, &ref);
	    }
	}

    }
    G_message(_("Done ... Put subgroup ref\n"));
    I_put_subgroup_ref(group, subgroup, &ref);

    return 0;
}

int list_group_files(char group[INAME_LEN], FILE *fd)
{
    struct Ref ref;

    I_get_group_ref(group, &ref);
    I_list_group (group, &ref, fd);

    return 0;
}

int list_subgroup_files(char group[INAME_LEN], char subgroup[INAME_LEN], FILE *fd)
{
    struct Ref ref;

    I_get_group_ref(group, &ref);
    I_list_subgroup (group, subgroup, &ref, fd);

    return 0;
}
