#define RASTER_ROWS 512

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

#else

    extern struct graphics graphics;

#endif
