#ifndef __CAIRODRIVER_H__
#define __CAIRODRIVER_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cairo.h>

#include <grass/config.h>
#include <grass/gis.h>

#include "driver.h"

#define DEFAULT_FILE_NAME "map.png"

/* Scale for converting colors from [0..255] to cairo's [0.0..1.0] */
#define COLORSCALE (1.0/255.0)
#define CAIROCOLOR(a) (((double)(a))*COLORSCALE)

/* File types */
#define FTYPE_UNKNOWN 0
#define FTYPE_PNG 1
#define FTYPE_PDF 2
#define FTYPE_PS  3
#define FTYPE_SVG 4

/* CAIRO drawing operations */
#define OP_NONE 0
#define OP_STROKE 1
#define OP_FILL 2

extern cairo_surface_t *surface;
extern cairo_t *cairo;

extern char *file_name;
extern int file_type;
extern int width, height;
extern int clip_left, clip_right, clip_top, clip_bottom;
extern int auto_write;
extern double bgcolor_r, bgcolor_g, bgcolor_b, bgcolor_a;
extern int modified;

extern const struct driver *Cairo_Driver(void);

extern void Cairo_Client_Close(void);
extern int  Cairo_Graph_set(int, char**);
extern void Cairo_Graph_close(void);
extern void Cairo_Box_abs(int, int, int, int);
extern void Cairo_Set_window(int, int, int, int);
extern void Cairo_draw_line(int, int, int, int);
extern void Cairo_draw_bitmap(int, int, int, const unsigned char*);
extern void Cairo_draw_point(int, int);
extern void Cairo_color(int);
extern int  Cairo_lookup_color(int, int, int);
extern void Cairo_Erase(void);
extern void Cairo_begin_scaled_raster(int, int[2][2], int[2][2]);
extern int  Cairo_scaled_raster(int, int,
				const unsigned char*, const unsigned char*,
				const unsigned char*, const unsigned char*);
extern void Cairo_end_scaled_raster(void);
extern void Cairo_Line_width(int);
extern void Cairo_Polygon_abs(const int*, const int*, int);
extern void Cairo_Polyline_abs(const int*, const int*, int);

/* Graph.c */
extern void init_cairo(void);
extern int ends_with(const char*, const char*);

/* write.c */
extern void write_image(void);

/* drawing.c */
extern int current_drawing_op;
extern int current_pos_x, current_pos_y;
extern void set_drawing_op(int);
extern void finish_drawing_op(void);
extern void move_to(int, int);
extern void set_pos(int, int);
extern void reset_pos(void);

#endif /* __CAIRODRIVER_H__ */
