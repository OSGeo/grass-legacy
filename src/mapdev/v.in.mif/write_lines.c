/******************************************************************************
 * write_lines.c [v.in.mif]
 * Write out line data imported from Mapinfo interchange file

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 15th. Oct. 2000
 * Last updated 16th. Oct. 2000
 *

 * This file is part of GRASS GIS. It is free software. You can 
 * redistribute it and/or modify it under the terms of 
 * the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option)
 * any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 ******************************************************************************/


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "local_structs.h"
#include "local_proto.h"
#include "static.h"

extern type_array *type0;
extern line_array *line0;
extern site_array *site0;
extern int has_mid;
extern d_type version_no;
extern d_type del0;
extern d_type proj_info;
extern field_data field_info;
extern field_data field_type;
extern int numcols;
extern char delchar;

int line_data_write(site_array *s0, line_array *l0, d_type *di, d_type del, 
		    int numsites, int numlines, char *vectfile, FILE *fp,
		    char *attstr) {

  /* local */

  struct Map_info Map1, Map2;
  struct Cell_head Region;
  FILE *att1 = NULL, *att2 = NULL;
  struct line_pnts tline1;

  int i;    /* loop */
  int k;
  int use_rec_num = 1;

  int len;
  char *tmpptr;
  char chdel[2];
  char out_lines[128], out_sites[128];

  int with_sites = 0, with_lines = 0, with_areas = 0;
  int attnum;
  int init_scale;

  double *curr_pnt_x, *curr_pnt_y;
  struct line_pnts *curr_line;
  int line_cnt, pnt_cnt, catval;
  double xpos, ypos;

  /* Check what kind of data we are to write, and set resp. flags */

  if(numsites > 0 && numlines > 0) {
    strcpy(out_lines, vectfile);
    strcat(out_lines, "_lines");
    strcpy(out_sites, vectfile);
    strcat(out_sites, "_sites");
    with_sites = 1;
    with_lines = 1;
  }

  else if(numsites > 0) {
    strcpy(out_sites, vectfile);
    with_sites = 1;
  }

  else if(numlines > 0) {
    strcpy(out_lines, vectfile);
    with_lines = 1;
  }

  else {
    fprintf(stderr, "No data available for writing. Aborting.\n");
    return -1;
  }


  /* Open the req'd database files for write */

  if(with_sites) {
    if( Vect_open_new(&Map1, out_sites) < 1 ) {
      fprintf(stderr, "Unable to open vector map `%s' to write site data.\n", out_sites);
      return -1;
    }

    if( (att1 = G_fopen_new("dig_att", out_sites)) == NULL ) {
      fprintf(stderr, "Unable to open attribute file `%s' to write site data.\n", out_sites);
      return -1;
    }

    /* atts_list1 = (int *)G_malloc( numsites * sizeof(int) ); */

  }
  
  if(with_lines) {
    if( Vect_open_new(&Map2, out_lines) < 1 ) {
      fprintf(stderr, "Unable to open vector map `%s' to write line data.\n", out_lines);
      return -1;
    }

    if( (att2 = G_fopen_new("dig_att", out_lines)) == NULL ) {
      fprintf(stderr, "Unable to open attribute file `%s' to write site data.\n", out_lines);
      return -1;
    }

    /* atts_list2 = (int *)G_malloc( numlines * sizeof(int) ); */

  }

  /* What is the initial scale value? */

  if( proc_init_scale(GET_VAL, &init_scale) < 0 ) {
    G_warning("Unable to retrieve initial scale value, setting default.\n");
    init_scale = 200;
  }
  

  /* Create array for attributes */


  fprintf(fp, "Mapinfo Data File:\n\n");
  fprintf(fp, "VERSION: %s\n", version_no);
  chdel[0] = delchar; chdel[1] = '\0';
  fprintf(fp, "DELIMITER: %s\n", chdel);
  fprintf(fp, "Projection info: %s\n", proj_info);

  fprintf(fp, "\nField data -- %d columns\n\n", numcols);

  if( strcmp(attstr, "__NO_FIELD__") ) {
    use_rec_num = 1;
  }

  else {
    for( i = 0; i < numcols; i++ ) {
      fprintf(fp, "Field %d is %s of type %s\n", i, field_info[i], field_type[i]);
      if( strcmp(attstr, field_info[i]) == 0 ) {
	attnum = i;
	use_rec_num = 0;
      }
    }
  }

  curr_pnt_x = &site0->x[0];
  curr_pnt_y = &site0->y[0];
  curr_line  = &line0->gplines[0];

  /* Write lines and data */

  pnt_cnt = 0; line_cnt  = 0;

  for( i = 0; i < type0->n_chars; i++ ) {

    if(type0->list[i] == DOT) {

      /* Process a point entry */

      k = type0->entities[i];

      xpos = *(curr_pnt_x++);
      ypos = *(curr_pnt_y++);

      if(use_rec_num || !has_mid)
	catval = ++pnt_cnt;
      else
	catval = get_att_field_val( (const d_type *)di, k, attnum, fp);

      if(catval)
	fprintf(att1, "P  %-12f  %-12f  %-8d \n", xpos, ypos, catval);

      tline1.x = (double *)G_malloc( 2 * sizeof(double) );
      tline1.y = (double *)G_malloc( 2 * sizeof(double) );
      tline1.x[0] = tline1.x[1] = xpos;
      tline1.y[0] = tline1.y[1] = ypos;
      tline1.alloc_points = 2;
      tline1.n_points = 2;

      Vect_write_line( &Map1, DOT, &tline1);

      G_free( tline1.x );
      G_free( tline1.y );
      
    }

    else if (type0->list[i] == LINE){

      /* Process a line entry */

      k = type0->entities[i];

      xpos = ( curr_line->x[0] + curr_line->x[1] ) / 2.0;
      ypos = ( curr_line->y[0] + curr_line->y[1] ) / 2.0;

      if(use_rec_num || !has_mid)
	catval = ++line_cnt;
      else
	catval = get_att_field_val( (const d_type *)di, k, attnum, fp);

      if(catval)
	fprintf(att2, "L  %-12f  %-12f  %-8d \n", xpos, ypos, catval);

      Vect_write_line( &Map2, LINE, curr_line++);

    }

    /* else if (type0->list[i] == AREA){} */

  }

  G_get_window(&Region);

  if(with_sites) {

    Map1.head.orig_scale = (long)init_scale;
    G_strncpy( Map1.head.your_name, G_whoami(), 20);
    G_strncpy( Map1.head.date, G_date(), 20);
    G_strncpy( Map1.head.map_name, out_sites, 20);
    Map1.head.W = Region.west;
    Map1.head.S = Region.south;
    Map1.head.E = Region.east;
    Map1.head.N = Region.north;

    Vect_close(&Map1);
    if(att1) fclose(att1);    
  }

  if(with_lines) {

    Map2.head.orig_scale = (long)init_scale;
    G_strncpy( Map2.head.your_name, G_whoami(), 20);
    G_strncpy( Map2.head.date, G_date(), 20);
    G_strncpy( Map2.head.map_name, out_lines, 20);
    Map2.head.W = Region.west;
    Map2.head.S = Region.south;
    Map2.head.E = Region.east;
    Map2.head.N = Region.north;

    Vect_close(&Map2);
    if(att2) fclose(att2);    
  }

  return 0;
}

