#include "gis.h"

#ifdef GRASS_VECT_H
/* vo_polygons.c */
void write_polygons(struct Map_info *, char *, int);
#endif

#ifdef GRASS_GIS_H

/* init_head.c */
int init_header(FILE *, struct Cell_head *, struct dig_head *);
#endif

/* vo_cats_att.c */
#define CATNUM_NO 0
#define CATNUM_KEEP 1
#define CATNUM_GEN 2

#define LABEL_NO 0
#define LABEL_CAT 1
#define LABEL_STR 2
#define LABEL_DEC 3
int write_cats_att(char *, FILE *, FILE *, char *, struct Cell_head, int, int, int);

/* vo_extend.c */
int extend_line (double, double, double, double, double, double, double, double, double, double*, double*, int);

/* vo_inregion.c */
int in_region(double, double);

/* vo_boundary.c */
#define NORTH 0
#define SOUTH 1
#define EAST 2
#define WEST 3
void prepareBoundaryArrays();
void handleBoundaryPoint(double, double);
void outputBoundary(int boundary, struct Map_info *, struct line_pnts *);
void freeBoundaryArrays();
