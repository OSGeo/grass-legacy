#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <grass/glocale.h>
#include "global.h"
#include <grass/vask.h>

#define NFILES 15

int ask_files(char *groupname)
{
    char result[NFILES][15];
    int repeat;
    int do_all;
    int i, k, f1;
    int *r;
    char **nm;

    r = ref_list;
    nm = new_name;

    repeat = 0;

    /* name extension for rectified maps */
    extension = (char *)G_malloc(GNAME_MAX * sizeof(char));
    sprintf(extension, ".ortho");

    repeat = 1;
    while (repeat) {
	repeat = 0;
	V_clear();
	V_line(1, _("Enter an extension to be appended to rectified maps:"));
	V_ques(extension, 's', 3, 0, 20);
	V_intrpt_ok();
	if (!V_call())
	    exit(0);

	/* test for legal file name */
	sprintf(result[0], "%s%s", group.group_ref.file[0].name, extension);
	if (G_legal_filename(result[0]) < 0) {
	    G_clear_screen();
	    fprintf(stderr, _("Extension <%s> is illegal"), extension);
	    repeat = G_yes(_("\nChoose another extension? "), 1);
	    if (!repeat) {
		fprintf(stderr,_("Orthorectification cancelled."));
		exit(0);
	    }
	}
    }
	
    G_debug(1, "Extension: %s", extension);

    /* rectify all files ? */
    do_all = 1;
    G_clear_screen();
    do_all = G_yes(_("\nRectify all files in the group? "), do_all);

    /* create list of files to be rectified */
    f1 = 0;
    for (i = 0; i < group.group_ref.nfiles && i < NFILES; i++) {
	int ok = 1;
	char buf[100];

	if (!do_all) {
	    sprintf(buf, _("\nRectify image <%s>? "), group.group_ref.file[i].name);
	    ok = G_yes(buf, ok);
	}
	if (ok) {
	    sprintf(result[i], "%s%s", group.group_ref.file[i].name, extension);
	    *r++ = f1++;
	    *nm++ = G_store(result[i]);
	}
    }
    for (i = f1; i < NFILES; i++) {
	result[i][0] = 0;
    }

    /* check if raster exists in target location/mapset */
    select_target_env();
    repeat = 0;
    G_clear_screen();
    for (i = 0; i < NFILES; i++) {
	if (result[i][0] && G_find_cell(result[i], G_mapset())) {
	    if (!repeat++) {
		repeat = 1;
		fprintf(stderr, "\n");
		fprintf(stderr,
			"** The following raster maps already exist in\n");
		fprintf(stderr, "** LOCATION %s, MAPSET %s:\n\n",
			G_location(), G_mapset());
	    }
	    fprintf(stderr, "%-18s\n", result[i]);
	}
    }
    select_current_env();
    if (repeat) {
	if (!G_yes("\n\nOk to overwrite? ", 0)) {
	    fprintf(stderr,_("Orthorectification cancelled."));
	    exit(0);
	}
    }

    for (k = 0; k < group.group_ref.nfiles; k++)
	if (ref_list[k] >= 0)
	    return 1;
    fprintf(stderr, "No files selected! Bye\n");
    G_sleep(3);
    exit(0);
}
