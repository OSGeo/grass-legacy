/* devices/lib routines */
#include "../lib/driverlib.h"

/* XDRIVER/lib routines */
/* Color.c */
int Color(int);
int RGB_color(unsigned char, unsigned char, unsigned char);
int _get_color_index(int);
int _get_color_index_array(int *, int);
int Standard_color(int);
int Color_offset(int);
int get_color_offset(void);
/* Erase.c */
int Erase(void);
/* Font.c */
int Font_get(char *);
/* Get_t_box.c */
int Get_text_box(char *, int *, int *, int *, int *);
/* Move_abs.c */
int Move_abs(int, int);
int Get_current_xy(int *, int *);
/* Move_rel.c */
int Move_rel(int, int);
/* Num_colors.c */
int Number_of_colors(int *);
/* Plydts_abs.c */
int Polydots_abs(int *, int *, int);
int Polydots_rel(int *, int *, int);
/* RGB.c */
/* Raster_def.c */
int Raster_char(int, int, unsigned char *, int, int);
int Raster_int_def(int, int, int *, int, int);
/* Reset_clrs.c */
int Reset_colors(int, int, unsigned char *, unsigned char *, unsigned char *);
int Reset_color(unsigned char, unsigned char, unsigned char, int);
/* Set_window.c */
int Set_window(int, int, int, int);
int window_clip(double *, double *, double *, double *);
/* Text.c */
int Text(char *);
/* Text_size.c */
int Text_size(int, int);
int Text_rotation(double);
/* clip.c */
int clip(double,double,double,double,double *,double *,double *,double *);
/* color_supp.c */
int assign_fixed_color(int, int);
int get_fixed_color(int);
int get_fixed_color_array(register int *, register int);
int assign_standard_color(int, int);
int get_standard_color(int);
int get_max_std_colors(void);
/* connect.c */
int get_connection(char *, int *, int *);
int prepare_connection(void);
int check_connection(char *, char *);
/* font.c */
int init_font(char *);
int get_char_vects(unsigned char, int *, unsigned char **, unsigned char **);
/* text.c */
int drawchar(double, double, register double, register double, unsigned char);
int soft_text_ext(int, int, double, double, double, char *);
int get_text_ext(int *, int *, int *, int *);
int soft_text(int, int, double, double, double, char *);
int onechar(int, int, double, double, double, unsigned char);

/* routines defined in the driver module */
/* Box_abs.c */
int Box_abs(int, int, int, int);
int Box_abs2(int, int, int, int);
/* Clr_table.c */
int reset_color(int, int, int, int);
int _get_lookup_for_color(int, int, int);
int get_table_type(void);
/* Color.c */
int SetXColor(int);
/* Cont_abs.c */
int Cont_abs(int, int);
int Cont_rel(int, int);
/* Raster.c */
int Raster_int(int, int, int *, int, int);
/* Returns.c */
int Screen_left(int *);
int Screen_rite(int *);
int Screen_bot(int *);
int Screen_top(int *);
int Get_num_colors(int *);
