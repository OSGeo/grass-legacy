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

extern void read_image(void);
extern void read_ppm(void);
extern void read_pgm(void);
extern void read_png(void);

extern void write_image(void);
extern void write_ppm(void);
extern void write_pgm(void);
extern void write_png(void);

extern void init_color_table(void);

extern void PNG_Box_abs(int,int,int,int);
extern void PNG_Client_Close(void);
extern void PNG_Erase(void);
extern void PNG_Graph_close(void);
extern int PNG_Graph_set(int,char **);
extern void PNG_Line_width(int);
extern void PNG_begin_scaled_raster(int [2][2], int [2][2]);
extern int PNG_scaled_raster(int, int, const unsigned char *, const unsigned char *, const unsigned char *, const unsigned char *);
extern void PNG_Respond(void);
extern void PNG_color(int);
extern void PNG_draw_bitmap(int,int,int,const unsigned char *);
extern void PNG_draw_line(int,int,int,int);
extern void PNG_draw_point(int,int);
extern int PNG_lookup_color(int, int, int);
