
#include "gis.h"
#include "graphics.h"


static int big_x_box[5] = {0,  0, 10,   0, -10 } ;
static int big_y_box[5] = {0, 10,  0, -10,   0 } ;

static int small_x_box[5] = {0,  0,  8,  0, -8 } ;
static int small_y_box[5] = {0,  8,  0, -8,  0 } ;

int legend_row, legend_col ;
static int screen_top ;
static int t, b, l, r ;
static int SEP ;

make_leg(mapfile,num_categories) 
	char *mapfile ;
	int num_categories ;
{
	D_get_screen_window(&t, &b, &r, &l) ;

	b = b - (b - t) / 20 ;

	SEP = (float)(b - t) / (float)(num_categories+2) ;
	big_x_box[0] = 0 ;
	big_x_box[1] = 0 ;
	big_x_box[2] = SEP-2 ;
	big_x_box[3] = 0 ;
	big_x_box[4] = -(SEP-2) ;

	big_y_box[0] = 0 ;
	big_y_box[1] = -(SEP-2) ;
	big_y_box[2] = 0 ;
	big_y_box[3] = SEP-2 ;
	big_y_box[4] = 0 ;

	small_x_box[0] = 0 ;
	small_x_box[1] = 0 ;
	small_x_box[2] = SEP-4 ;
	small_x_box[3] = 0 ;
	small_x_box[4] = -(SEP-4) ;

	small_y_box[0] = 0 ;
	small_y_box[1] = -(SEP-4) ;
	small_y_box[2] = 0 ;
	small_y_box[3] = SEP-4 ;
	small_y_box[4] = 0 ;

	write_title(mapfile,num_categories) ;
	write_colors(num_categories) ;
}

write_title(mapfile,num_categories) 
	char *mapfile ;
	int num_categories ;
{
	struct Cell_head windo ;
	char buffer[128] ;
	int incr ;
	float text_siz ;

	text_siz = (float)SEP * .8 ;

	R_text_size(text_siz,text_siz);
	R_standard_color(D_translate_color("white")) ;

/* Place window information above map area */
	G_get_set_window(&windo) ;
	sprintf(buffer,"NORTH: %10.1f WEST: %10.1f", windo.north, windo.west) ;
	R_move_abs(get_screen_x_pos(TEXT1_X), get_screen_y_pos(TEXT1_Y)) ; 
	R_text(buffer) ;

	sprintf(buffer,"SOUTH: %10.1f EAST: %10.1f", windo.south, windo.east) ;
	R_move_abs(get_screen_x_pos(TEXT2_X), get_screen_y_pos(TEXT2_Y)) ; 
	R_text(buffer) ;

/* Place title above map area */
	R_move_abs(get_screen_x_pos(TITL1_X), get_screen_y_pos(TITL1_Y)) ; 
	R_text(G_myname()) ;
	R_move_abs(get_screen_x_pos(TITL2_X), get_screen_y_pos(TITL2_Y)) ; 
	R_text(mapfile) ;

	return(0) ;
}

write_colors(num_categories) 
	int num_categories ;
{
	char buffer[128] ;
	int incr ;

	screen_top = get_screen_top() ;

	R_text_size(6,6);
	R_standard_color(D_translate_color("white")) ;

	R_text_size(6,6) ;
	for (incr=0; incr<=num_categories; incr++)
	{
		legend_col = incr / BLOCKS_PER_COL ;
		legend_row = incr % BLOCKS_PER_COL ;
		R_move_abs((3 + legend_col * SEPARATION), 
			(screen_top - ( legend_row + 1) * SEPARATION )) ;
		R_color(incr) ;
		R_polygon_rel(big_x_box, big_y_box, 4) ;
		if (num_categories<BLOCKS_PER_COL)
		{
			R_move_abs((3 + legend_col * SEPARATION + SEPARATION), 
				(screen_top - (legend_row + 1) * SEPARATION)) ;
			sprintf(buffer,"%d",incr) ;
			R_standard_color(D_translate_color("white")) ;
			R_text(buffer) ;
		}
	}

	return(0) ;
}

mark_category(cat_num, on_off)
	int cat_num, on_off ;
{
	legend_col = cat_num / BLOCKS_PER_COL ;
	legend_row = cat_num % BLOCKS_PER_COL ;

	/* Draw big box */
	R_move_abs((3 + legend_col * SEPARATION), 
		(screen_top - (legend_row+1) * SEPARATION)) ;
	if (on_off)
		R_standard_color(D_translate_color("white")) ;
	else
		R_color(cat_num) ;
	R_polyline_rel(big_x_box, big_y_box, 5) ;

	/* Draw small box */
	R_move_abs((3 + legend_col * SEPARATION + 1), 
		(screen_top - (legend_row+1) * SEPARATION + 1)) ;
	if (on_off)
		R_standard_color(D_translate_color("black")) ;
	else
		R_color(cat_num) ;
	R_polyline_rel(small_x_box, small_y_box, 5) ;
}
