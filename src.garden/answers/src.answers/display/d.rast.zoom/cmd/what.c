#include "what.h"

what (name,window)
char name[128];
struct Cell_head window;
{
    int width;
    int i;
    struct Cell_head make_window();
    int	count, map_id, t,b,l,r, j;
    int row, col;
    int nrows, ncols;
    CELL *buf;
    int screen_x, screen_y ;
    double east, north ;
    int button ;
    char	*mapset, buf1[512];

    mapset = G_find_cell2(name,"");
    if (!mapset){
    	sprintf(buf1, "map [%s] not found\n", name);
	G_fatal_error (buf1);
	exit(1);
	}

    G_get_set_window (&window);
    nrows = window.rows;
    ncols = window.cols;

    window = make_window(&window, name, mapset);

    R_standard_color(D_translate_color("black"));

    D_erase_window();
    D_clear_window();
    R_close_driver();

    confirm_window(&window, name, mapset);
}
