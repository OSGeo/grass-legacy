                        /********************************/
                        /*     r.le.setup/r.le.trace.h  */
                        /*                              */
                        /*             2.1              */
                        /*                              */
                        /*       07/25/94 version       */
                        /*                              */
                        /*      Programmer: Baker       */
                        /*      Univ. of Wyoming        */
                        /********************************/

#include "stdio.h"
#include "math.h"
#include "ctype.h"
#include "stdlib.h"
#include "string.h"
#include "gis.h"

#define EQ(a, b)    (a-b < 0.01 && a-b > -0.01 )
#define BIG   500000000.0
#define MIN   5
#define NULLPTR (PATCH *) 0

typedef struct pt {
	int	     row, col;
	struct pt    *next;
	} PT;

typedef struct patch {
	int          att, num;
	double       area;
	double 	     perim;
	double       long_axis;
	int          n, s, e, w;
        double	     c_row, c_col;
	int          npts;
	int          *col, *row;
	struct patch *next;
	} PATCH;


struct CHOICE {
	char fn[30], out[30];
	int patchmap, trace, perim2;
};

void  set_map();
void  show_patch();
void  patch_attr();
void  draw_patch();
void  scr_cell();   
void  cell_clip_drv();
void  cell_clip();
int   is_not_empty_buffer();
int   center_is_not_zero();
void  trace();
PATCH *get_bd();
int   yes_nb();
void  clockwise();
