#ifndef BUFSIZ
#include <stdio.h>
#endif

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

#define DEF_WIDTH  640
#define DEF_HEIGHT 480
#define FILE_NAME "D_cell"


GLOBAL unsigned char Cur_color;
GLOBAL char *Filename;
GLOBAL FILE *Temp_fp;
GLOBAL unsigned char Color_table[256][3];
GLOBAL unsigned char *Row_buf;

/* Can_do.c */
int can_do_float(void);
/* Color.c */
int color(int);
/* Draw_line.c */
int draw_line(int, int, int, int);
/* Get_w_box.c */
int Get_location_with_box(int, int, int *, int *, int *);
/* Get_w_line.c */
int Get_location_with_line(int, int, int *, int *, int *);
/* Get_w_pnt.c */
int Get_location_with_pointer(int *, int *, int *);
/* Graph_Clse.c */
int Graph_Close(void);
/* Graph_Set.c */
int Graph_Set(void);
/* Panel.c */
int Panel_save(char *, int, int, int, int);
int Panel_restore(char *);
int Panel_delete(char *);
/* Polygn_abs.c */
int Polygon_abs(int *, int *, int);
/* Reset_clr.c */
int reset_color(int, int, int, int);
/* Returns.c */
int Screen_left(int *);
int Screen_rite(int *);
int Screen_bot(int *);
int Screen_top(int *);
int Get_num_colors(int *);
int get_num_colors(void);
/* bresline.c */
int bres_line(int, int, int, int);
/* connect.c */
int get_connection(char *, int *, int *);
int prepare_connection(void);
int check_connection(char *, char *);
/* file_io.c */
int store_xy(int, int);
int horiz_line(int, int, int);
/* polyfill.c */
int polyfill(int *, int *, int, int (*)(int,int,int));
