#include "global.h"
#include "crs.h"     /* CRS HEADER FILE */
#include "protodefs.h"

int get_target_window (struct Ref ref)
{
    char name[30], mapset[30];
    struct Cell_head cellhd;


    fprintf (stderr, "\n\n");
    while(1)
    {
	char buf[100];
	fprintf (stderr, "Please select one of the following options\n");
	fprintf (stderr, " 1. Use the current region in the target location\n");
	fprintf (stderr, " 2. Determine the smallest region which covers the image\n");
	fprintf (stderr, "> ");
	if (!G_gets(buf)) continue;
	G_strip (buf);
	if (strcmp (buf,"1") == 0) return 0;
	if (strcmp (buf,"2") == 0) break;
    }

    ask_file_from_list (ref, name, mapset);
    if (G_get_cellhd (name, mapset, &cellhd) < 0)
	exit(-1);
    georef_window (&cellhd, &target_window);
    ask_window (&target_window);

    if(!G_yes("Would you like this region saved as the region in the target location?\n", -1))
	return 0;
    select_target_env();
    if(G_put_window (&target_window)>=0)
	fprintf (stderr, "Saved!\n");
    select_current_env();

    return 0;
}

int georef_window (struct Cell_head *w1, struct Cell_head *w2)
{
    double e,  n;          /* east, north, elev in target */
    double nused1=0.0, nused2;     /* for values not-used */


    /* Calculate the transformation parameters */
    group.calculate_trans(&group);


/*      CRS_georef (w1->west, w1->north, &e, &n, E12, N12, order); */
      group.forward_trans (&group, w1->west, w1->north, nused1,
			    &e, &n, &nused2);
      w2->north = w2->south = n;
      w2->west  = w2->east  = e;

/*      CRS_georef (w1->east, w1->north, &e, &n, E12, N12, order); */
      group.forward_trans (&group, w1->east, w1->north, nused1,
			    &e, &n, &nused2);
      if (n > w2->north) w2->north = n;
      if (n < w2->south) w2->south = n;
      if (e > w2->east ) w2->east  = e;
      if (e < w2->west ) w2->west  = e;

/*      CRS_georef (w1->west, w1->south, &e, &n, E12, N12, order); */
      group.forward_trans (&group, w1->west, w1->south, nused1,
			    &e, &n, &nused2);
      if (n > w2->north) w2->north = n;
      if (n < w2->south) w2->south = n;
      if (e > w2->east ) w2->east  = e;
      if (e < w2->west ) w2->west  = e;

/*      CRS_georef (w1->east, w1->south, &e, &n, E12, N12, order); */
      group.forward_trans (&group, w1->east, w1->south, nused1,
			    &e, &n, &nused2);
      if (n > w2->north) w2->north = n;
      if (n < w2->south) w2->south = n;
      if (e > w2->east ) w2->east  = e;
      if (e < w2->west ) w2->west  = e;



    w2->ns_res = (w2->north - w2->south) / w1->rows;
    w2->ew_res = (w2->east  - w2->west ) / w1->cols;

    return 0;
}

