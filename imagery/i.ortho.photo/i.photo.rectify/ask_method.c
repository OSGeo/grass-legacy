#include <stdlib.h>
#include <string.h>
#include "global.h"

int ask_method(void)
{
    int f1, f2, count, max_rows, max_cols;
    int nx, ny;
    struct Cell_head win;
    double max_mb_img, max_mb_elev, max_mb;
    
    fprintf(stderr, "\n\n");
    while (1) {
	char buf[100];

	G_clear_screen();
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
	    interpolate = p_nearest;
	    method = "nearest";
	    break;
	}
	if (strcmp(buf, "2") == 0) {
	    interpolate = p_bilinear;
	    method = "bilinear";
	    break;
	}
	if (strcmp(buf, "3") == 0) {
	    interpolate = p_cubic;
	    method = "bicubic";
	    break;
	}
	if (strcmp(buf, "4") == 0) {
	    interpolate = p_bilinear_f;
	    method = "bilinear_f";
	    break;
	}
	if (strcmp(buf, "5") == 0) {
	    interpolate = p_cubic_f;
	    method = "bicubic_f";
	    break;
	}
    }

    count = max_rows = max_cols = 0;
    for (f1 = 0; f1 < group.group_ref.nfiles; f1++) {
	if (ref_list[f1] >= 0) {
	    f2 = ref_list[f1];
	    G_get_cellhd(group.group_ref.file[f2].name,
			 group.group_ref.file[f2].mapset, &win);
	    if (max_rows < win.rows)
		max_rows = win.rows;
	    if (max_cols < win.cols)
		max_cols = win.cols;
	}
    }

    ny = (max_rows + BDIM - 1) / BDIM;
    nx = (max_cols + BDIM - 1) / BDIM;

    max_mb_img = ((double)nx * ny * sizeof(block)) / (1<<20);

    ny = (target_window.rows + BDIM - 1) / BDIM;
    nx = (target_window.cols + BDIM - 1) / BDIM;

    max_mb_elev = ((double)nx * ny * sizeof(block)) / (1<<20);
    max_mb = max_mb_img + max_mb_elev + 0.5; /* + 0.5 for rounding */
    if (max_mb < 1)
	max_mb = 1;

    fprintf(stderr, "\n\n");
    while (1) {
	char buf[100];
	int seg_mb;

	fprintf(stderr, _("Amount of memory to use in MB\n"));
	fprintf(stderr, _("RETURN   use %d MB to keep all data in RAM\n"), (int)(max_mb));
	fprintf(stderr, "> ");
	if (!G_gets(buf))
	    continue;

	if (*buf == 0) {		/* all in memory */
	    seg_mb_elev = max_mb_elev;
	    seg_mb_img = max_mb_img;
	    break;
	}

	G_strip(buf);
	if ((seg_mb = atoi(buf)) > 0) {
	    seg_mb_elev = seg_mb * max_mb_elev / (max_mb_img + max_mb_elev);
	    seg_mb_img = seg_mb * max_mb_img / (max_mb_img + max_mb_elev);
	    break;
	}
    }

    fprintf(stderr, "\n\n");

    return 0;
}
