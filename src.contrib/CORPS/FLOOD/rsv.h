/*******************************************************************************
                    Floodplain Analysis Toolkit
               Mother Earth Systems, Boulder, Colorado


This software was been developed for the U.S. Army Corps of Engineers,
Omaha District under contract #DACW45-92-P-1301.

This code is in the public domain.  Permission to use, copy, modify, and
distribute this software and its documentation for any purpose and without
fee is granted.

*******************************************************************************/

#define TRUE  1
#define FALSE 0

#define MAX_PVERTS 256

#define FNAMELEN   257
#define BUFFLEN   1024

/*-----------------------*/
/* structure definitions */
/*-----------------------*/
#define C_HEAD struct Cell_head
#define M_INFO struct Map_info
#define L_PNTS struct line_pnts
#define X_INFO struct xs_info
 
/*-----------------------*/
/* function declarations */
/*-----------------------*/
void   R_open_driver();
void   R_flush();
void   R_standard_color();

int    D_move_abs(), D_cont_abs();
int    D_translate_color();
void   D_setup();
double D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east();
double D_d_to_u_row(), D_d_to_u_col();

void   G_setup_plot();
char  *G_find_cell();
int    G_open_cell_old();
int    G_get_set_window();
CELL  *G_allocate_cell_buf();
int    G_get_map_row();
double G_col_to_easting();
double G_row_to_northing();
void   G_plot_line();
