/***************************************************************************
* MODULE:       r3.mask
*
* AUTHOR(S):    Roman Waupotitsch, Michael Shapiro, Helena Mitasova,
*		Bill Brown, Lubos Mitas, Jaro Hofierka
*
* PURPOSE:      Establishes the current working 3D raster mask.
*
* COPYRIGHT:    (C) 2005 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/
/*Headerfile for global structs and funcktions*/

/*Structures*/
typedef struct _d_interval {
	double low, high;
	int inf;
	struct _d_interval *next;
} d_Interval;

typedef struct _d_mask {
	d_Interval *list;
} d_Mask;

/*Prototypes*/
int mask_d_select (DCELL *x, d_Mask *mask);
extern DCELL mask_match_d_interval (DCELL x, d_Interval *I);
void parse_vallist (char **vallist, d_Mask **d_mask); 
