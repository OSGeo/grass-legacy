#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include "gis.h"
#include "bintree.h"
#include "dem.h"

struct flags {struct Flag *v;};
#define VERBOSE flag.v->answer

#define G_northing_to_row(north, cell_head) ((cell_head->north - north) / cell_head->ns_res)
#define G_easting_to_col(east, window) (east - window->west) / window->ew_res

/************************************************************************/
/** Translates DEM head to GRASS head and returns conversion constants **/
/**  such that x' = [0](x-[4]) + [1](y-[5]) + [6]                      **/
/**            y' = [2](x-[4]) + [3](y-[5]) + [7]                      **/
/************************************************************************/
int conv_head_ll2u(struct dem_head *dem_head, struct Cell_head *cell_head){
  unsigned int i; 
  int isok;
  double *east, *north, emin, emax, nmin, nmax;
  extern struct flags flag;

  if (VERBOSE) fprintf(stdout, "Converting DEM head from LL to UTM\n");
  east = (double *)G_calloc((int)dem_head->num_sides, (int)sizeof(*east));
  north = (double *)G_calloc((int)dem_head->num_sides, (int)sizeof(*north));
  emin = nmin = DBL_MIN;
  emax = nmax = DBL_MAX;
  cell_head->zone = 0;

  for (i=0; i<dem_head->num_sides; i++){   /* Get extremeties on UTM grid */ 
    if (!(isok = CC_ll2u(*(dem_head->corner_list+(i*2+1)), *(dem_head->corner_list+(i*2)), east+i, north+i, &(cell_head->zone)))){
      if (isok == -1){
	fprintf (stderr, "Corner latitude above 84 degrees.");
	exit (-3);
      }
      if (isok == -2){
	fprintf (stderr, "Map area too large for one UTM zone/");
	exit (-4);
      }
    }

    if (*(east+i) < emin) emin = *(east+i);
    if (*(east+i) > emax) emax = *(east+i);
    if (*(north+i) < nmin) nmin = *(north+i);
    if (*(north+i) > nmax) nmax = *(north+i);
  }
  /* The guesses in this section could be improved */
  cell_head->format = 1;                   /* guess at 2 bytes/cell */
  cell_head->compressed = YES;
  cell_head->proj = PROJECTION_UTM;
  cell_head->north = nmax;       /* Circumscribe rectangle around data */
  cell_head->south = nmin;       /* This will probably leave edges w/o data */
  cell_head->east = emax;
  cell_head->west = emin;
  cell_head->rows = (int)((double)dem_head->cols * 1.2); /* guess */
  cell_head->cols = (int)((double)dem_head->cols * 1.2); /* guess */
  cell_head->ew_res = ((cell_head->east - cell_head->west) / (double)cell_head->cols);
  cell_head->ns_res = ((cell_head->north - cell_head->south) / (double)cell_head->rows);
  return 1;
}

/*****************************************************************************/
/* Convert and write a record to the output file. This takes two rows and    */
/* interpolates triangles. It assumes the two rows to be aligned, and        */
/* also assumes a square grid (no tricks to find the best triangles).        */
/*****************************************************************************/
int conv_row_ll2u(struct dem_head *dem_head, struct dem_data *record[2], struct Cell_head *cell_head){
  unsigned int dem_row, row, col, minrow, maxrow, mincol, maxcol;
  double cell_row[4], cell_col[4];  /* Position of source points in target grid coords (non-integral)  */
  double xoff[3], yoff[3];          /* offsets of two triangle corners from vertex; off[2] is the test pt */
  double co[2];  /* The fractions of two triangle sides whose sum gives the location of the test point */ 
  double denom;  /* The cross-product of the two triangle sides, used to calculate co[] */
  cell_data_t eloff[2], el;  /* elevation offsets of the two triangle corners, el. of test point */
  double lat, lon[2], east, north;
/*  extern struct flags flag;*/

  lon[0] = record[0]->start_coord[0]; /* Presumed longitude of column (assumes N-S trending edge)*/ 
  lon[1] = record[1]->start_coord[0];

/** Preset topmost two points (row 0) **/
  lat = record[0]->start_coord[1] + (double)0 * (double)dem_head->resolution[1];
  CC_ll2u(lat, lon[0], &east, &north, &(cell_head->zone));
  cell_col[0] = G_easting_to_col(east, cell_head);
  cell_row[0] = (double)G_northing_to_row(north, cell_head);
  lat = record[1]->start_coord[1] + (double)0 * (double)dem_head->resolution[1]; /* Ought to be the same as the last */
  CC_ll2u(lat, lon[1], &east, &north, &(cell_head->zone));
  cell_col[1] = G_easting_to_col(east, cell_head);
  cell_row[1] = G_northing_to_row(north, cell_head);

  for (dem_row=1; dem_row < record[0]->rows; dem_row++){
/** Set next two points down **/
    lat = record[0]->start_coord[1] + (double)dem_row * (double)dem_head->resolution[1];
    CC_ll2u(lat, lon[0], &east, &north, &(cell_head->zone));
    cell_col[2] = G_easting_to_col(east, cell_head);
    cell_row[2] = G_northing_to_row(north, cell_head);
    lat = record[1]->start_coord[1] + (double)dem_row * (double)dem_head->resolution[1];
    CC_ll2u(lat, lon[1], &east, &north, &(cell_head->zone));
    cell_col[3] = G_easting_to_col(east, cell_head);
    cell_row[3] = G_northing_to_row(north, cell_head);

/** Calculate for triangle 012 (vertex 0) **/
/* find corners of superscribed rectangle */
    minrow = (cell_row[0] < cell_row[1]) ? 0 : 1;
    maxrow = (minrow == 0) ? 1 : 0;
    minrow = (cell_row[2] < cell_row[minrow]) ? 2 : minrow;
    maxrow = (cell_row[2] > cell_row[maxrow]) ? 2 : maxrow;
    mincol = (cell_col[0] < cell_col[1]) ? 0 : 1;
    maxcol = (mincol == 0) ? 1 : 0;
    mincol = (cell_col[2] < cell_col[mincol]) ? 2 : mincol;
    maxcol = (cell_col[2] > cell_col[maxcol]) ? 2 : maxcol;

/* find x,y,el offset vectors */
    xoff[0]  = cell_col[1] - cell_col[0];
    yoff[0]  = cell_row[1] - cell_row[0];
    eloff[0] = *(record[1]->data + dem_row - 1) - *(record[0]->data + dem_row - 1);
    xoff[1]  = cell_col[2] - cell_col[0];
    yoff[1]  = cell_row[2] - cell_row[0];
    eloff[1] = *(record[0]->data + dem_row) - *(record[0]->data + dem_row - 1);

    for (row=(int)(cell_row[minrow]+1); row <= (int)cell_row[maxrow]; row++){
      yoff[2] = (double)row - cell_row[0];
      for (col=(int)(cell_col[mincol]+1); col <= (int)cell_col[maxcol]; col++){
	xoff[2] = (double)col - cell_col[0];
	denom = xoff[0] * yoff[1] - xoff[1] * yoff[0];
	co[0] = (xoff[2] * yoff[1] - xoff[1] * yoff[2]) / denom;
	if (co[0] < 0) continue;   /* skip if outside triangle */
	co[1] = (xoff[0] * yoff[2] - xoff[2] * yoff[0]) / denom;
	if (co[1] < 0) continue;
	if (co[0] + co[1] > 1) continue;  /* beyond this, we have an inside point */
	el = *(record[0]->data + dem_row - 1) + co[0] * eloff[0] + co[1] * eloff[1];
	insert_ptree_node((cell_row_t)row, (cell_row_t)col, (cell_data_t)el);
      }
    } 

/** And for triangle 123 (vertex 3) **/
/* find corners of superscribed rectangle */
    minrow = (cell_row[3] < cell_row[1]) ? 3 : 1;
    maxrow = (minrow == 3) ? 1 : 3;
    minrow = (cell_row[2] < cell_row[minrow]) ? 2 : minrow;
    maxrow = (cell_row[2] > cell_row[maxrow]) ? 2 : maxrow;
    mincol = (cell_col[3] < cell_col[1]) ? 3 : 1;
    maxcol = (mincol == 3) ? 1 : 3;
    mincol = (cell_col[2] < cell_col[mincol]) ? 2 : mincol;
    maxcol = (cell_col[2] > cell_col[maxcol]) ? 2 : maxcol;

/* find x,y,el offset vectors */
    xoff[0]  = cell_col[1] - cell_col[3];
    yoff[0]  = cell_row[1] - cell_row[3];
    eloff[0] = *(record[1]->data + dem_row - 1) - *(record[1]->data + dem_row);
    xoff[1]  = cell_col[2] - cell_col[3];
    yoff[1]  = cell_row[2] - cell_row[3];
    eloff[1] = *(record[0]->data + dem_row) - *(record[1]->data + dem_row);

    for (row=(int)(cell_row[minrow]+1); row <= (int)cell_row[maxrow]; row++){
      yoff[2] = (double)row - cell_row[3];
      for (col=(int)(cell_col[mincol]+1); col <= (int)cell_col[maxcol]; col++){
	xoff[2] = (double)col - cell_col[3];
	denom = xoff[0] * yoff[1] - xoff[1] * yoff[0];
	co[0] = (xoff[2] * yoff[1] - xoff[1] * yoff[2]) / denom;
	if (co[0] < 0) continue;   /* skip if outside triangle */
	co[1] = (xoff[0] * yoff[2] - xoff[2] * yoff[0]) / denom;
	if (co[1] < 0) continue;
	if (co[0] + co[1] > 1) continue;  /* beyond this, we have an inside point */
	el = *(record[0]->data + dem_row - 1) + co[0] * eloff[0] + co[1] * eloff[1];
	insert_ptree_node((cell_row_t)row, (cell_row_t)col, (cell_data_t)el);
      }
    } 


/** Shift points down **/
    cell_row[0] = cell_row[2];
    cell_row[1] = cell_row[3];
    cell_col[0] = cell_col[2];
    cell_col[1] = cell_col[3];
  }
  return 1;
}


