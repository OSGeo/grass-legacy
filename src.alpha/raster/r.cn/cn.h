#include "gis.h"
#include <math.h>

#define SHORT	short

SHORT        		nrows, ncols;
struct Cell_head 	window;
struct Categories 	hy_soil_group_cats, land_use_cats;
struct Categories 	hy_cond_cats, veg_cover_cats;

int 	hy_soil_group_flag, land_use_flag, veg_cover_flag; 
int 	cn_flag, hy_cond_flag, amc_flag;
char 	*hy_soil_group_name, *land_use_name, amc_name[10];
char 	*veg_cover_name, *cn_name, *hy_cond_name;
char 	*hy_soil_group_mapset, *land_use_mapset;
char 	*veg_cover_mapset, *cn_mapset, *hy_cond_mapset;
char    *this_mapset;

int	hy_soil_cover[31][3];

void data();
void usage();

int G_get_set_window();
int G_window_rows();
int G_window_cols();
int G_read_cats();
int G_get_map_row();
int G_zero_cell_buf();
int G_put_map_row();
int G_close_cell();
int G_put_cell_title();
int G_gisinit();
int G_legal_filename();
int G_fatal_error();

