#include <stdio.h>
#include <math.h>
#include "gis.h"

#define SHORT	short

SHORT        		nrows, ncols;
struct Cell_head 	window;
struct Categories 	hy_soil_group_cats, land_use_cats;
struct Categories 	hy_cond_cats, veg_cover_cats;

char 	hy_soil_group_name[40], land_use_name[40], amc_name[10];
char 	veg_cover_name[40], cn_name[40], hy_cond_name[40];
char 	*hy_soil_group_mapset, *land_use_mapset;
char 	*veg_cover_mapset, *cn_mapset, *hy_cond_mapset;
char    *this_mapset;

int	hy_soil_cover[32][3]; /* changed to 32 for landuse "water"  12/96 MN */
