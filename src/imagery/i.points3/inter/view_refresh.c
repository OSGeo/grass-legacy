#include "globals.h"

static int use = 1;
static int cancel(void);
static int refresh_img(void);
static int refresh_tar(void);

int plot_refresh(void)
{
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

static int cancel(void)
{
    return 1;
}

static int refresh_img(void)
{
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

static int refresh_tar(void)
{
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
