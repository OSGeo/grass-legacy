#include "what.h"
#include "raster.h"
#include "display.h"
#include "local_proto.h"

int 
what (char name[128], struct Cell_head *window)
{
    int nrows, ncols;
    char	*mapset, buf1[512];

    mapset = G_find_cell2(name,"");
    if (!mapset){
    	sprintf(buf1, "map [%s] not found\n", name);
	G_fatal_error (buf1);
	exit(1);
	}

    G_get_set_window (window);
    nrows = window->rows;
    ncols = window->cols;

    make_window(window, name, mapset);

    R_standard_color(D_translate_color("black"));

    D_erase_window();
    D_clear_window();
    R_close_driver();

    confirm_window(window, name, mapset);

    return 0;
}
