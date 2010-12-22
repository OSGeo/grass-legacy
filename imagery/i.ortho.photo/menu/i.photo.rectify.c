#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include <grass/gis.h>
#include <grass/vask.h>
#include <grass/imagery.h>
#include <grass/ortholib.h>
#include <grass/glocale.h>
#include "orthophoto.h"
#include "local_proto.h"

#define L2BDIM 6
#define BDIM (1<<(L2BDIM))
#define L2BSIZE (2*(L2BDIM))
#define BSIZE (1<<(L2BSIZE))
#define HI(i) ((i)>>(L2BDIM))
#define LO(i) ((i)&((BDIM)-1))

typedef DCELL block[BDIM][BDIM];   /* FCELL sufficient ? */

/* interactive command line construction for i.photo.rectify */
/* return 1 on success, 0 on failure
 */
int rectify(char *groupname)
{
    int i, max_rows, max_cols;
    int nx, ny;
    struct Cell_head win, target_window;
    double max_mb_img, max_mb_elev, max_mb;
    struct Ortho_Image_Group group;
    int do_all, ok, repeat, use_target_window;
    char extension[GNAME_MAX];
    char result[GNAME_MAX];

    char pgm[1024];
    int stat;

    sprintf(pgm, "i.photo.rectify  group=%s", groupname);
    strcpy(group.name, groupname);

    /* get group ref */
    if (!I_get_group_ref(group.name, (struct Ref *)&group.group_ref)) {
	fprintf(stderr, _("Could not read REF file for group [%s]"), group.name);
	fprintf(stderr, _("Orthorectification cancelled"));
	return 0;
    }
    if (group.group_ref.nfiles <= 0) {
	fprintf(stderr, _("No files in this group!"));
	fprintf(stderr, _("Orthorectification cancelled"));
	return 0;
    }
    
    /* get target location/mapset */
    get_target(group.name, &target_window);

    /* rectify all images in group ? */
    do_all = 1;
    if (group.group_ref.nfiles > 1) {
	G_clear_screen();
	do_all = G_yes(_("\nRectify all images in the group? "), do_all);
    }
    
    if (!do_all) {
	int got_one = 0;
	/* create list of files to be rectified */
	for (i = 0; i < group.group_ref.nfiles; i++) {
	    ok = 1;
	    char buf[100];

	    sprintf(buf, _("\nRectify image <%s>? "), group.group_ref.file[i].name);
	    ok = G_yes(buf, ok);
	    if (ok) {
		if (!got_one) {
		    sprintf(pgm, "%s input=%s@%s", pgm, group.group_ref.file[i].name, group.group_ref.file[i].mapset);
		}
		else {
		    sprintf(pgm, "%s,%s@%s", pgm, group.group_ref.file[i].name, group.group_ref.file[i].mapset);
		}
		got_one = 1;
	    }
	}
	if (!got_one) {
	    fprintf(stderr,_("\nNo images selected, orthorectification cancelled."));
	    G_sleep(3);
	    return 0;
	}
    }
    else
	sprintf(pgm, "%s -a", pgm);
    
    /* name extension for rectified maps */
    sprintf(extension, ".ortho");

    repeat = 1;
    while (repeat) {
	repeat = 0;
	V_clear();
	V_line(1, _("Enter an extension to be appended to rectified maps:"));
	V_ques(extension, 's', 3, 0, 20);
	V_intrpt_ok();
	if (!V_call())
	    return 0;

	/* test for legal file name */
	sprintf(result, "%s%s", group.group_ref.file[0].name, extension);
	if (G_legal_filename(result) < 0) {
	    G_clear_screen();
	    fprintf(stderr, _("Extension <%s> is illegal"), extension);
	    repeat = G_yes(_("\nChoose another extension? "), 1);
	    if (!repeat) {
		fprintf(stderr,_("Orthorectification cancelled."));
		G_sleep(3);
		return 0;
	    }
	}
    }
    sprintf(pgm, "%s extension=%s", pgm, extension);
    
    /* compute local camera angle */
    G_clear_screen();
    ok = G_yes(_("\nCompute local camera angle? "), 0);
    if (ok) {
	char angle_name[GNAME_MAX];

	sprintf(angle_name, "%s.camera_angle", groupname);
	
	repeat = 1;
	while (repeat) {
	    repeat = 0;
	    V_clear();
	    V_line(1, _("Enter a name for the camera angle map:"));
	    V_ques(angle_name, 's', 3, 0, 30);
	    V_intrpt_ok();
	    if (!V_call())
		return 0;

	    /* test for legal file name */
	    if (G_legal_filename(angle_name) < 0) {
		G_clear_screen();
		fprintf(stderr, _("Map name <%s> is illegal"), angle_name);
		repeat = G_yes(_("\nChoose another name? "), 1);
		if (!repeat) {
		    fprintf(stderr,_("Orthorectification cancelled."));
		    G_sleep(3);
		    return 0;
		}
	    }
	    else 
		sprintf(pgm, "%s angle=%s", pgm, angle_name);
	}
    }

    /* overwrite maps in target location/mapset */
    G_clear_screen();
    ok = G_yes(_("\nOverwrite maps in target location/mapset? "), 0);
    if (ok) {
	sprintf(pgm, "%s --o", pgm);
    }

    /* use current region settings in target location (def.=calculate smallest area) ? */
    use_target_window = 0;
    while (1) {
	char buf[100];

	fprintf(stderr, "Please select one of the following options\n");
	fprintf(stderr,
		" 1. Use the current window in the target location\n");
	fprintf(stderr,
		" 2. Determine the smallest window which covers the image\n");
	fprintf(stderr, "> ");
	if (!G_gets(buf))
	    continue;
	G_strip(buf);

	if (strcmp(buf, "1") == 0) {
	    use_target_window = 1;
	    break;
	}
	if (strcmp(buf, "2") == 0)
	    break;
    }

    if (use_target_window) {
	sprintf(pgm, "%s -c", pgm);
    }
    else {
	/* ask for target resolution */
	double res = -1;
	
	while (1) {
	    char buf[100];

	    fprintf(stderr, "Enter desired target resolution, or\n");
	    fprintf(stderr,
		    " RETURN   to determine it automatically:\n");
	    fprintf(stderr, "> ");
	    if (!G_gets(buf))
		continue;

	    if (*buf == 0) {  /* determine automatically */
		break;
	    }

	    G_strip(buf);

	    if ((res = atof(buf)) <= 0) {
		fprintf(stderr, "Resolution must be larger than zero!");
		G_clear_screen();
	    }
	    else
		break;
	}
	if (res > 0)
	    sprintf(pgm, "%s res=%f", pgm, res);
    }

    /* interpolation method */
    while (1) {
	char buf[100];

	G_clear_screen();
	fprintf(stderr, "\n");
	fprintf(stderr, _("Please select one of the following interpolation methods\n"));
	fprintf(stderr, _(" 1. nearest neighbor\n"));
	fprintf(stderr, _(" 2. bilinear\n"));
	fprintf(stderr, _(" 3. bicubic\n"));
	fprintf(stderr, _(" 4. bilinear with fallback\n"));
	fprintf(stderr, _(" 5. bicubic with fallback\n"));
	fprintf(stderr, "> ");
	if (!G_gets(buf))
	    continue;
	G_strip(buf);

	if (strcmp(buf, "1") == 0) {
	    sprintf(pgm, "%s method=nearest", pgm);
	    break;
	}
	if (strcmp(buf, "2") == 0) {
	    sprintf(pgm, "%s method=bilinear", pgm);
	    break;
	}
	if (strcmp(buf, "3") == 0) {
	    sprintf(pgm, "%s method=cubic", pgm);
	    break;
	}
	if (strcmp(buf, "4") == 0) {
	    sprintf(pgm, "%s method=bilinear_f", pgm);
	    break;
	}
	if (strcmp(buf, "5") == 0) {
	    sprintf(pgm, "%s method=cubic_f", pgm);
	    break;
	}
    }

    /* amount of memory to use */
    if (use_target_window) {
	max_rows = max_cols = 0;
	for (i = 0; i < group.group_ref.nfiles; i++) {
	    G_get_cellhd(group.group_ref.file[i].name,
			 group.group_ref.file[i].mapset, &win);
	    if (max_rows < win.rows)
		max_rows = win.rows;
	    if (max_cols < win.cols)
		max_cols = win.cols;
	}

	ny = (max_rows + BDIM - 1) / BDIM;
	nx = (max_cols + BDIM - 1) / BDIM;

	max_mb_img = ((double)nx * ny * sizeof(block)) / (1<<20);

	ny = (target_window.rows + BDIM - 1) / BDIM;
	nx = (target_window.cols + BDIM - 1) / BDIM;

	max_mb_elev = ((double)nx * ny * sizeof(block)) / (1<<20);
	max_mb = ceil(max_mb_img + max_mb_elev);
	if (max_mb < 1)
	    max_mb = 1;
    }
    else
	/* skip calculating mimimum window, use default */
	max_mb = 100;

    fprintf(stderr, "\n\n");
    while (1) {
	char buf[100];
	int seg_mb = max_mb + 0.5;

	fprintf(stderr, _("Enter amount of memory to use in MB, or\n"));
	if (use_target_window)
	    fprintf(stderr, _("RETURN   use %d MB to keep all data in RAM\n"), seg_mb);
	else {
	    fprintf(stderr, _("RETURN   use %d MB\n"), seg_mb);
	}
	fprintf(stderr, "> ");
	if (!G_gets(buf))
	    continue;

	if (*buf == 0) {		/* all in memory */
	    sprintf(pgm, "%s memory=%d", pgm, seg_mb);
	    break;
	}

	G_strip(buf);
	if ((seg_mb = atoi(buf)) > 0) {
	    sprintf(pgm, "%s memory=%d", pgm, seg_mb);
	    break;
	}
    }

    if ((stat = G_system(pgm)))
	G_sleep(3);

    return stat;
}
