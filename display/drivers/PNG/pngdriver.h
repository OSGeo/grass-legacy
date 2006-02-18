#include <stdio.h>

#include <grass/config.h>
#include "driver.h"

#define FILE_NAME  "map.png"

extern char *file_name;
extern FILE *output;
extern int currentColor;
extern unsigned int *xpixels;
extern int true_color;
extern int auto_write;
extern int has_alpha;

extern int width, height;
extern unsigned int *grid;
extern unsigned char palette[256][4];
extern unsigned int transparent;
extern unsigned int background;
extern int modified;

extern int linewidth;

extern void write_image(void);
extern void init_color_table(void);

extern void PNG_Box_abs(int,int,int,int);
extern int PNG_Can_do_float(void);
extern void PNG_Client_Close(void);
extern int PNG_Color_table_fixed(void);
extern int PNG_Color_table_float(void);
extern void PNG_Erase(void);
extern void PNG_Graph_close(void);
extern int PNG_Graph_set(int,char **);
extern int PNG_Line_width(int);
extern void PNG_Raster_int(int,int,const int *,int,int);
extern void PNG_Respond(void);
extern void PNG_reset_color(int,int,int,int);
extern void PNG_color(int);
extern void PNG_draw_line(int,int,int,int);
extern void PNG_draw_point(int,int);
extern int PNG_get_table_type(void);
extern int PNG_lookup_color(int, int, int);
