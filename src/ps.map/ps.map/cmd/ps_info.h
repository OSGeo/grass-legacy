#include "gis.h"

#define DELTA_Y	0.05
#define MAX_PSFILES	20
/* Following XCONV, YCONV were commented because of using G_plot_where_xy()
 * and uncommented again because G_adjust_easting 
 * in it is not best for each case,  RB Jan 2000 
 */
#define XCONV(E_COORD)	(PS.map_left + PS.ew_to_x * ((E_COORD) - PS.w.west))
#define YCONV(N_COORD)	(PS.map_bot  + PS.ns_to_y * ((N_COORD) - PS.w.south))


struct PS_data 
{
    struct Cell_head w;
    struct Colors colors;
    struct Categories cats;
    CELL min_color, max_color;
    char *cell_mapset;
    char *cell_name; 
    char *plfile;
    char *commentfile;
    char *grid_font;
    char *psfiles[MAX_PSFILES];
    char scaletext[100];
    char celltitle[100];
    int level;
    int grey;
    int mask_needed;
    int do_header;
    int do_raster;
    int do_colortable;
    int num_psfiles;
    int grid, grid_color, grid_numbers, grid_numbers_color, grid_fontsize;
    double grid_width;
    int do_outline, outline_color;
    int cell_fd;
    int row_delta, col_delta; 
    int cells_wide, cells_high;
    int num_panels, startpanel, endpanel;
    int res;
    double page_width, page_height;
    double left_marg, right_marg, top_marg, bot_marg;
    double map_x_orig, map_y_orig, min_y, set_y;
    double map_pix_wide, map_pix_high;
    double map_width, map_height;
    double map_top, map_bot, map_left, map_right;
    double ew_res, ns_res;
    double ew_to_x, ns_to_y;
    double r0, g0, b0;
    double outline_width;
    FILE *fp;
};

#ifdef WHITE
#undef WHITE
#endif
#ifdef BLACK
#undef BLACK
#endif
#ifdef GREY
#undef GREY
#endif
#ifdef MAIN
struct PS_data PS;
int WHITE = 0;
int BLACK = 1;
int GREY = 9;
int sec_draw;
#else
extern struct PS_data PS;
extern int WHITE, BLACK, GREY, sec_draw;
#endif
