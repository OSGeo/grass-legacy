#include <string.h>
#include "global.h"

/* Corrected  get_target_window (void) to correctly register
   current window settings into the target_window structure 
   Pierre de Mouveaux (pmx@audiovu.com) 05 april 2000       
 */

int get_target_window (void)
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
	if (strncmp (buf,"1",1) == 0) {
/*	Pierre de Mouveaux - 11 april 2000*/
		G_get_window(&target_window);
		return 1;
	}
	if (strncmp (buf,"2",1) == 0) break;
    }
    ask_file_from_list (name, mapset);
    if (G_get_cellhd (name, mapset, &cellhd) < 0)
	exit(-1);
    georef_window (&cellhd, &target_window);
    ask_window (&target_window);

    if(!G_yes("Would you like this region saved as the region in the target location?\n", -1))
	return 1;
    select_target_env();
    if(G_put_window (&target_window)>=0)
	fprintf (stderr, "Saved!\n");
    select_current_env();

    return 0;
}

int georef_window (struct Cell_head *w1, struct Cell_head *w2)
{
    double n,e;

    I_georef (w1->west, w1->north, &e, &n, E12, N12);
    w2->north = w2->south = n;
    w2->west  = w2->east  = e;

    I_georef (w1->east, w1->north, &e, &n, E12, N12);
    if (n > w2->north) w2->north = n;
    if (n < w2->south) w2->south = n;
    if (e > w2->east ) w2->east  = e;
    if (e < w2->west ) w2->west  = e;

    I_georef (w1->west, w1->south, &e, &n, E12, N12);
    if (n > w2->north) w2->north = n;
    if (n < w2->south) w2->south = n;
    if (e > w2->east ) w2->east  = e;
    if (e < w2->west ) w2->west  = e;

    I_georef (w1->east, w1->south, &e, &n, E12, N12);
    if (n > w2->north) w2->north = n;
    if (n < w2->south) w2->south = n;
    if (e > w2->east ) w2->east  = e;
    if (e < w2->west ) w2->west  = e;

    w2->ns_res = (w2->north - w2->south) / w1->rows;
    w2->ew_res = (w2->east  - w2->west ) / w1->cols;

    return 0;
}
