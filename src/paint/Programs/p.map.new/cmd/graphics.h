#define RASTER_ROWS 512
/*
#define RASTER_ROWS  1000 
*/

struct graphics
{
    unsigned char *raster[RASTER_ROWS];
    int raster_size;
    int dirty;      /* has any raster value been set? */
    int color;      /* current color */
    int width1;     /* dot thickness */
    int width2;     /* dot thickness */
    int drawover;   /* dot replaces anything? */

    struct          /* window for panels and clipping */
    {
	int top;
	int bottom;
	int left;
	int right;
	double north;
	double south;
	double east ;
	double west ;
	double ns_res ;
	double ew_res ;
    } window;

    struct          /* line style control */
    {
	int *table;     /* table of 0-9's */
	int len;        /* length of table */
	int colors[9];  /* colors for 1-9 */
	int cur;        /* current table index */
	int prevx;      /* prev dot plotted */
	int prevy;
    } linestyle;
} ;


#ifdef MAIN

    struct graphics graphics;
    unsigned char MAXCOLOR;

#else

    extern struct graphics graphics;
    extern unsigned char MAXCOLOR;

#endif

#ifdef GRASS_GIS_H
/* draw_bar.c */
int draw_barscale(FILE *, int, int, double, char *, double, double, double, int, int, int, int);
/* set_grphcs.c */
int set_graphics(struct Cell_head *, int, int);
/* draw_cat.c */
int draw_cat(double,double,int,int,DCELL,DCELL,struct Colors *);
/* draw_ramp.c */
int draw_ramp(double, double, int, int, int, struct Cell_stats *);
#endif

/* dobackground.c */
void dobg(int, int, int, int, int, int, int, int, int);
/* dot.c */
int dot(int, int);
/* draw_line.c */
int draw_line(int, int, int, int);
/* drwline.c */
int drwline(double, double, double, double);
/* drwscale.c */
int drwscale(double, double, double, double);
/* graph_line.c */
int graph_line(int,int, int, int);
/* graph_pnt.c */
int graph_point(int, int);
/* griddraw.c */
int drawgrid(void);
int do_grid_numbers(int);
void draw_tickew(double);
void draw_tickns(double);
/* line_eq.c */
int line_eq(int, int, int, int, int, int);
/* line_style.c */
int set_line_style_solid(void);
int set_line_style(char *, int [9]);
int regress_line_style(void);
/* linefile.c */
int linefile(double, double, double, double);
/* outline.c */
int outlinefile(void);
/* redraw_bar.c */
int redraw_barscale(double, double, int, double, int, int);
/* set_color.c */
int set_color(int);
/* set_grphcs.c */
int init_graphics(int);
/* set_width.c */
int set_width(int);
