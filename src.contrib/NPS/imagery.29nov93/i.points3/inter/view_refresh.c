
#include "globals.h"

static int use = 1;

plot_refresh()
{
    int cancel();
    int refresh_img(); 
    int refresh_tar(); 

    static Objects objects[] =
    {
	MENU ("CANCEL", cancel, &use),
	TITLE("<REFRESH>", &use),
	MENU ("IMAGE",  refresh_img, &use), 
	MENU ("TARGET", refresh_tar, &use), 
	INFO ("Select side to refresh.", &use),
	{0}
    };

    Input_pointer (objects);
    return 0;
}

static
cancel()
{
    return 1;
}




refresh_img()
{
    char name[100], mapset[100];
    struct Cell_head cellhd;


    /* erase imagery */
    Erase_view (VIEW_MAP1);
    Erase_view (VIEW_MAP1_ZOOM);

    /* redraw imagery */
    select_current_env();
    drawcell(VIEW_MAP1);
    drawcell(VIEW_MAP1_ZOOM);

    /* TODO */
    /* need to redisplay zoom box */

    /* redisplay points */
    display_points(1); 
    return 0;
}


refresh_tar()
{
    char name[100], mapset[100];
    struct Cell_head cellhd;
    int i;


    /* erase imagery */
    Erase_view (VIEW_MAP2);
    Erase_view (VIEW_MAP2_ZOOM);

    /* redraw target raster and vectors */
    select_target_env();

    /* display raster */
    if (display_list.num_raster > 0) {
      drawcell (VIEW_MAP2);
      drawcell (VIEW_MAP2_ZOOM);
    }

    /* any vectors to zoom */
    if (display_list.num_vectors >= 0) {
      for (i = 0; i <= display_list.num_vectors; i++) 
	_plotvect(display_list.vects[i].vect_name,
		  display_list.vects[i].vect_mapset,
		  display_list.vects[i].vect_color, 0);
      select_current_env();
    }

    select_current_env();

    /* TODO */
    /* need to redisplay zoom box */

    /* redisplay points */
    display_points(1); 
    return 0;
}
