#include "gis.h"
#include "config.h"


#define	LEFTB	1

#ifndef	ANOTHER_BUTTON
#	define	MIDDLEB	2
#	define	RIGHTB	3
#else
#	define	MIDDLEB	3
#	define	RIGHTB	2
#endif

#define	LBTN	"Left:  "
#define	MBTN	"Middle:"
#define	RBTN	"Right: "

/* ask.c */
int yes(char *);
int just_click(char *);
int ask(char *[]);
/* box.c */
int make_window_box(struct Cell_head *, double);
/* center.c */
int make_window_center(struct Cell_head *, double, double, double);
/* returns.c */
int get_wind_bot(void);
int get_wind_top(void);
int get_wind_rite(void);
int get_wind_left(void);
int get_map_bot(void);
int get_map_top(void);
int get_map_left(void);
int get_map_rite(void);
int get_wind_y_pos(float);
int get_wind_x_pos(float);
/* zoom.c */
int zoomwindow(int, double);
/* redraw.c */
int redraw(void);


#ifdef MAIN
#define	GLOBAL
#else
#define	GLOBAL	extern
#endif

GLOBAL char *cmd;
GLOBAL char **rast, **vect, **site, **list;
GLOBAL int nrasts, nvects, nsites, nlists;
GLOBAL double U_east, U_west, U_south, U_north;

