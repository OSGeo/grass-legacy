#include "globals.h"

static int use = 1;
static int cancel(void);
static int clear_tar(void);

int plot_clear()
{
    static Objects objects[] =
    {
	MENU ("CANCEL", cancel, &use),
	TITLE("<CLEAR>", &use),
/* 	MENU ("IMAGE",  clear_img, &use), */ 
	MENU ("TARGET", clear_tar, &use), 
	INFO ("Select side to clear", &use),
	{0}
    };

    Input_pointer (objects);
    return 0;
}

static int cancel()
{
    return 1;
}



int clear_img()
{
    char name[100], mapset[100];
    struct Cell_head cellhd;


    /* erase imagery */
    Erase_view (VIEW_MAP1);
    Erase_view (VIEW_MAP1_ZOOM);

    /* no longer configured */
    VIEW_MAP1->cell.configured = 0;
    VIEW_MAP1_ZOOM->cell.configured = 0;
    VIEW_MAP1->vect.configured = 0;
    VIEW_MAP1_ZOOM->vect.configured = 0;

    return 1;  /* have plot clear quit */
}


static int clear_tar(void)
{
    int i;


    /* erase imagery */
    Erase_view (VIEW_MAP2);
    Erase_view (VIEW_MAP2_ZOOM);

    /* clear display list */
    display_list.num_raster = 0;
    display_list.num_vectors = -1;

    /* no longer configured */
    VIEW_MAP2->cell.configured = 0;
    VIEW_MAP2_ZOOM->cell.configured = 0;
    VIEW_MAP2->vect.configured = 0;
    VIEW_MAP2_ZOOM->vect.configured = 0;

    return 1; /* have plot clear quit */
}
