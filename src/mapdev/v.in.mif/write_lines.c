/******************************************************************************
 * write_lines.c [v.in.mif]
 * Write out line data imported from Mapinfo interchange file

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 15th. Oct. 2000
 * Last updated 14th. Jan. 2001
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
#include <assert.h>
#include <errno.h>
#include <setjmp.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "local_structs.h"
#include "local_proto.h"
#include "store.h"
#include "cpoly.h"
#include "parts.h"
#include "gbtree.h"

extern int has_mid;
extern d_type version_no;
extern d_type del0;
extern d_type proj_info;
extern field_data field_info;
extern field_data field_type;
extern int numcols;
extern char delchar;
extern type_array *type0;

int line_data_write(site_array *s0, line_array *l0, line_array *a0, ring_offsets *ro0,
		    d_type *di, d_type del, int numsites, int numlines, int numrings,
		    char *vectfile, FILE *fp, char *attstr) {

  /* local */

  struct Map_info Map1, Map2, Map3;
  struct Cell_head Region;
  FILE *att1 = NULL, *att2 = NULL, *att3 = NULL;
  struct line_pnts tline1;
  pntRepos *rep;
  BTREE *hVB;
  line_set *tmp_rings = NULL;
  cpolygon *cp1;

  int i, ja;    /* loop */
  int k;
  int use_rec_num = 1;

  int len;
  char *tmpptr;
  char chdel[2];
  char out_lines[128], out_sites[128], out_areas[128];

  int with_sites = 0, with_lines = 0, with_areas = 0;
  int attnum;
  int init_scale;
  int area1, reg1, reg_mark; 
  int is_start, is_end;

  double *curr_pnt_x, *curr_pnt_y;
  struct line_pnts *curr_line, *curr_area;
  int line_cnt, pnt_cnt, area_cnt, reg_cnt, hull_cnt, catval;
  double xpos, ypos;

  /* Check what kind of data we are to write, and set resp. flags */

  if(numsites == 0 && numlines > 0 && numrings == 0) {
    strcpy(out_lines, vectfile);
    with_sites = 0;
    with_lines = 1;
    with_areas = 0;
  }

  else if(numsites > 0 && numlines == 0 && numrings == 0) {
    strcpy(out_sites, vectfile);
    with_sites = 1;
    with_lines = 0;
    with_areas = 0;
  }

  else if(numsites == 0 && numlines == 0 && numrings > 0) {
    strcpy(out_areas, vectfile);
    with_sites = 0;
    with_lines = 0;
    with_areas = 1;
  }

  else if(numsites > 0) {
    strcpy(out_sites, vectfile);
    strcat(out_sites, "_sites");
    with_sites = 1;
  }

  else if(numlines > 0) {
    strcpy(out_lines, vectfile);
    strcat(out_lines, "_lines");
    with_lines = 1;
  }

  else if(numrings > 0) {
    strcpy(out_areas, vectfile);
    strcat(out_areas, "_rings");
    with_areas = 1;
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

  if(with_areas) {
    if( Vect_open_new(&Map3, out_areas) < 1 ) {
      fprintf(stderr, "Unable to open vector map `%s' to write line data.\n", out_areas);
      return -1;
    }

    if( (att3 = G_fopen_new("dig_att", out_areas)) == NULL ) {
      fprintf(stderr, "Unable to open attribute file `%s' to write site data.\n", out_areas);
      return -1;
    }

    /* atts_list2 = (int *)G_malloc( numlines * sizeof(int) ); */

  }

  /* What is the initial scale value? */

  if( G_process_scale_value(GET_VAL, &init_scale) < 0 ) {
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

  if(with_sites) {
  curr_pnt_x = &s0->x[0];
  curr_pnt_y = &s0->y[0];
  }

  if(with_lines)
    curr_line  = &l0->gplines[0];

  if(with_areas) {
    curr_area  = &a0->gplines[0];
    hVB = (BTREE *)malloc( sizeof(BTREE) );
    btree_create(hVB, btree_compare, 50);
    rep = (pntRepos *)malloc( sizeof(pntRepos) );
    memset(rep, 0, sizeof(pntRepos));
  }

  /* Write lines and data */

  pnt_cnt = 0; line_cnt  = 0; area_cnt = 0; reg_cnt = 0; hull_cnt = 0;

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

    else if (type0->list[i] == AREA){

      /* Process an area entry */

      k = type0->entities[i];

      /* Locate ring and region number */

      area1 = area_cnt++;

      if(area1 == 0) {
	is_start = 1;

	if(area1 == ro0->offs[reg_cnt])
	  is_end = 1;
	else
	  is_end = 0;
      }

      else if(area1 == ro0->offs[reg_cnt] + 1) {
	reg_cnt++;
	reg1 = reg_cnt;
	assert( k == reg1 );
	is_start = 1;

	if(area1 == ro0->offs[reg_cnt])
	  is_end = 1;
	else
	  is_end = 0;
      }

      else {

	is_start = 0;

	if(area1 == ro0->offs[reg_cnt])
	  is_end = 1;
	else
	  is_end = 0;
      }

      if( !tmp_rings ) {
	if( (tmp_rings = (line_set *)malloc( sizeof(line_set) )) == NULL ) {
	  fprintf(stderr, "Error allocating dynamic memory.\n");
	  return -1;
	}
      }

      /* If this is the first ring of a region, but not the last,
	 start assembling a cpolygon
      */

      if(is_start && !is_end) {

	memset(tmp_rings, 0 , sizeof(line_set) );

	if(append_sring(tmp_rings, curr_area++) < 0) {

	  fprintf(stderr, "Error allocating dynamic memory.\n");
	  return -1;
	}	
      }

      else if(is_end) {

	/* This is the end of the current region, add the last ring and
	   continue processing 
	*/

	if(is_start)
	  memset(tmp_rings, 0 , sizeof(line_set) );

	if(append_sring(tmp_rings, curr_area++) < 0) {

	  fprintf(stderr, "Error allocating dynamic memory.\n");
	  return -1;
	}

	/* Create the cpolygon now */

	G_build_cpolygons_struct(&cp1, tmp_rings);


	/* Extract info entries, and find centroids */

	for( ja = 0; ja < cp1->n_hulls; ja++ ) {

	  G__mpolygon_centroid(cp1, ja, &xpos, &ypos);

	  if(use_rec_num || !has_mid)
	    catval = ++hull_cnt;
	  else
	    catval = get_att_field_val( (const d_type *)di, k, attnum, fp);

	  if(catval)
	    fprintf(att3, "A  %-12f  %-12f  %-8d \n", xpos, ypos, catval);
	  
	}

	/* Add the vertices to the vbase */

	mif_import_build_network(hVB, rep, cp1);
	G_destroy_cpolygons_struct(cp1);
	
	/* Dispose of the temp lines */

	if(tmp_rings->lines) free(tmp_rings->lines);

      }

      else {  /* !is_first && !is_end  */
	if(append_sring(tmp_rings, curr_area++) < 0) {

	  fprintf(stderr, "Error allocating dynamic memory.\n");
	  return -1;
	}	
      }

    }

  }

  /* Clean up some temp objects */

  if(tmp_rings) free(tmp_rings);


  /* Remove duplicate line tracks 
     (not working yet)
  */

  /* strip_duplicate_tracks(hVB); */


  /* Extract the lines to the vector map */

  if(with_areas)
    vbase_extract_lines(hVB, &Map3);

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

  if(with_areas) {

    Map3.head.orig_scale = (long)init_scale;
    G_strncpy( Map3.head.your_name, G_whoami(), 20);
    G_strncpy( Map3.head.date, G_date(), 20);
    G_strncpy( Map3.head.map_name, out_areas, 20);
    Map3.head.W = Region.west;
    Map3.head.S = Region.south;
    Map3.head.E = Region.east;
    Map3.head.N = Region.north;

    Vect_close(&Map3);
    if(att3) fclose(att3);    
  }

  fprintf(stderr, "WARNING: You must run `v.support option=build' on\n  this (these) \
map(s) to build the topology.\n");

  return 0;
}


int append_sring(line_set *ls0, struct line_pnts *lp0) {

  sring *srt = NULL;
  int i;
  double maxx, maxy, minx, miny;

  if( (srt = (sring *)malloc( sizeof(sring) )) == NULL ) {
    fprintf(stderr, "Error allocating dynamic memory.\n");
    return -1;
  }

  memset(srt, 0, sizeof(sring));

  srt->points = lp0;

  for( i = 0; i < lp0->n_points; i++ ) {

    if( i == 0 ) {
      maxx = minx = lp0->x[i];
      maxy = miny = lp0->y[i];
    }

    else {
      if(lp0->x[i] > maxx) maxx = lp0->x[i];
      if(lp0->x[i] < minx) minx = lp0->x[i];
      if(lp0->y[i] > maxy) maxy = lp0->y[i];
      if(lp0->y[i] < miny) miny = lp0->y[i];      
    }
  }

  srt->bbox.w = minx;
  srt->bbox.n = maxy;
  srt->bbox.e = maxx;
  srt->bbox.s = miny;


  /* Make sure there is enough space, then append the sring to the line-set */

  if(ls0->alloc_lines == 0) {

    if( (ls0->lines = (sring *)malloc( 128 * sizeof(sring) )) == NULL ) {
      fprintf(stderr, "Error allocating dynamic memory.\n");
      return -1;
    }

    memset(ls0->lines, 0, 128 * sizeof(sring));
    ls0->alloc_lines = 128;
  }

  else if(ls0->n_lines + 1 > ls0->alloc_lines) {

    if( (ls0->lines = (sring *)realloc( ls0->lines, (ls0->alloc_lines + 128)
					* sizeof(sring) )) == NULL ) {
      fprintf(stderr, "Error allocating dynamic memory.\n");
      return -1;
    }
      
    memset(&ls0->lines[ls0->alloc_lines - 128], 0 , 128 * sizeof(sring));
    ls0->alloc_lines += 128;
  }

  memcpy(&ls0->lines[ls0->n_lines++], srt, sizeof(sring) );
  if(srt) free(srt);
  return 0;
}

int mif_import_build_network(BTREE *hbt, pntRepos *pr0, cpolygon *cp0) {

  /* Add the contents of the hulls of the cpolygons to a vbase. 

   * This is unreliable, as we cannot assume that a hole will necessarily be
   * associated with an interior ring.

   */

  int i, j;
  int np;
  int no_links;

  partDescript *pd0 = NULL;
  struct line_pnts *curr_hull;
  pntDescript *curr_pt;


  for( i = 0; i < cp0->n_hulls; i++ ) {

    /* For each hull, build a part structure (as in shapefile) */

    np = cp0->hulls[i].points->n_points;
    curr_hull = cp0->hulls[i].points;

    if(!pd0) {
      if( (pd0 = (partDescript *)malloc( sizeof(partDescript))) == NULL ) {
	fprintf(stderr, "Error allocating dynamic memory.\n");
	return -1;
      }
    }

    memset(pd0, 0, sizeof(partDescript));
    
    if( (pd0->linepnts = (pntDescript *)malloc( np * sizeof(pntDescript))) == NULL ) {
      fprintf(stderr, "Error allocating dynamic memory.\n");
      return -1;
    }

    pd0->numPoints = pd0->allocPoints = np;
    memset(pd0->linepnts, 0, np * sizeof(pntDescript));

    for( j = 0; j < np; j++ ) {

      curr_pt = &pd0->linepnts[j];
      
      curr_pt->xPosn = curr_hull->x[j];
      curr_pt->yPosn = curr_hull->y[j];
    }

    /* Add the part to the repository */

    if(pr0->n_parts + 1 > pr0->alloc_parts) {
      
      if( pr0->n_parts == 0 ) {
	
	if( (pr0->parts = (partDescript *)malloc(1000 * sizeof(partDescript))) == NULL ) {
	  fprintf(stderr, "Error allocating dynamic memory.\n");
	  return -1;
	}

	memset(pr0->parts, 0, 1000 * sizeof(partDescript));
	pr0->alloc_parts = 1000;
      }

      else {

	if( (pr0->parts = (partDescript *)realloc(pr0->parts, (pr0->alloc_parts + 500) *
						  sizeof(partDescript))) == NULL ) {
	  fprintf(stderr, "Error allocating dynamic memory.\n");
	  return -1;
	}

	memset(&pr0->parts[pr0->alloc_parts - 500], 0, 500 * sizeof(partDescript));
	pr0->alloc_parts += 500;
      }
    }

    memcpy(&pr0->parts[pr0->n_parts++], pd0, sizeof(partDescript));
    
    /* Add the points to the vbase */

    no_links = 0;

    for( j = 0; j < np; j++ ) {

      if(!vertRegister(hbt, &pr0->parts[pr0->n_parts - 1], j)) 
	no_links++;
    }
    
    if(no_links == np)
      fprintf(stderr, "WARNING: Duplicate ring added.\n");

  }
}



int vertRegister( BTREE *hDB, partDescript *part1, int pt_indx ) {

  static void *ptr_old = NULL;
  static int currerror = 0;

  static int num_registered = 0;
  static int db_allocated = 0;

  /* local */
  int i;
  int np;
  int res, res1;
  int result = 0;
  char *pkey;
  double snap;
  double fe, fn;
  int idig, declen;

  double angle0, angle1;
  int linked, lnum;

  pntDescript *pf, *pb, *pc; /* Holders for current and flanking vertices */
  pntDescript *pbl;

  char *keyHolder;
  pntDescript **dataHolder, **tmpdataHolder;
  pntDescript **pntPtrPtr;

  jmp_buf startpnt;


  np = part1->numPoints;

  /* Go on if any point should be invalid */

  if( setjmp(startpnt) ) return 0;

  /* Retrieve snap distance for map */
  if( G_process_snap_distance( GET_VAL, &snap ) ) {
    fprintf(stderr, "Could not set snap distance. Aborting." );
    exit(1);
  }

  /* Retrieve the parameters for the vbase key */

  if( G_process_key_params( GET_VAL, &idig, &fe, &fn ) ) {
    fprintf(stderr, "Could not aquire parameters for setting key values. Aborting." );
    exit(1);
  }

  declen = 16 - idig;
  if( declen < 0 || declen > 16 ) {
    fprintf(stderr, "Key parameters outside acceptable range. Aborting." );
    exit(1);
  }


  /* Assign key value */
  pkey = (char *)malloc( 33 );
  strncpy( pkey, calcKeyValue( &part1->linepnts[pt_indx], snap,
			       declen, fe, fn ), 33 );


  /* Is this point registered in the database? */

  pc = &part1->linepnts[pt_indx];

  pntPtrPtr = (pntDescript **)malloc( sizeof( pntDescript *) );
  keyHolder = (char *)malloc( 33 );
  dataHolder = ( pntDescript ** )malloc( sizeof( pntDescript * ));
  tmpdataHolder = ( pntDescript ** )malloc( sizeof( pntDescript * ));
  
  *pntPtrPtr = pc;

  strncpy(keyHolder, pkey, 33 );
  dataHolder = pntPtrPtr;

  res = btree_find( hDB, keyHolder, (void **)&dataHolder );
  if( res == 0 ) btree_update( hDB, keyHolder, 33, dataHolder, 4 );

  if ( res == 1 ) {
    /* Point is already in database. Modify to reflect new links */
    res1 = 0; /* Make this sensible at some point !! */

    /* Get the point */
    /* res1 = hDB->get( hDB, keyHolder, tmpdataHolder, 0 ); */

    if( res1 == -1 ) {
      /* There was an error */
      /* G_fatal_error( "Error retrieving data from database" ); */
    }
    else if( res1 == 1 ) {
      /* No key. This shouldn't happen. Abort! */
      /* G_fatal_error( "Error retrieving data from database" ); */
    }
    else{
      pc = *( (pntDescript **) dataHolder );

      if( pt_indx == 0 )
	ptr_old = NULL;
      else {
	pb = (pntDescript *) ptr_old;

	/* Is this the same vertex. If so skip */

	if( pc == pb ) longjmp( startpnt, 1);

	/* Are we already linked to this? */
	linked = 0;
	for( i = 0; i < pb->linknum; ++i ) {
	  pbl = pb->linkverts[i];
	  if( pc == pbl )
	    linked = 1;
	}

	/* Determine angle of link to previous vertex */

	angle0 = atan2(( pb->yPosn - pc->yPosn ), ( pb->xPosn - pc->xPosn ) );
	if( angle0 < 0 ) angle0 += 2 * M_PI ;
	angle1 = angle0 + M_PI ;
	if( angle1 >= 2 * M_PI ) angle1 -= 2 * M_PI ;

	lnum = pc->linknum;

	if( !linked ) {
	  result = 1;
	  if( lnum == 0 ) {
	    pc->linkdirect = (double *)malloc( sizeof( double));
	    pc->linkverts = (pntDescript **)malloc( sizeof (pntDescript *));
	  }
	  else {
	    pc->linkdirect = (double *)realloc( pc->linkdirect, (lnum + 1) * sizeof( double));
	    pc->linkverts = (pntDescript **)realloc( pc->linkverts, (lnum + 1) *
						  sizeof (pntDescript *));
	  }
	  lnum++;
	  pc->linkdirect[lnum-1] = angle0;
	  pc->linkverts[lnum-1] = pb;
	  pc->linknum = lnum;
	}


	  
	/* Now fill in the fields of the previous link */
	lnum = pb->linknum;

	if( !linked ) {
	  if( lnum == 0 ) {
	    pb->linkdirect = (double *)malloc( sizeof( double));
	    pb->linkverts = (pntDescript **)malloc( sizeof (pntDescript *));
	  }
	  else {
	    pb->linkdirect = (double *)realloc( pb->linkdirect, (lnum + 1) * sizeof( double));
	    pb->linkverts = (pntDescript **)realloc( pb->linkverts, (lnum + 1) *
						  sizeof (pntDescript *));
	  }
	  lnum++;
	  pb->linkdirect[lnum-1] = angle1;
	  pb->linkverts[lnum-1] = pc;
	  pb->linknum = lnum;
	}

      }
    }

    ptr_old = pc;
    
  }
  else {
    /* Point is added: reflect new links */
    pc = *( (pntDescript **) dataHolder );
    num_registered++;
    result = 1;

    /* Determine angle of link to previous vertex */
    if( pt_indx > 0 ) {
      pb = (pntDescript *) ptr_old;


      angle0 = atan2(( pb->yPosn - pc->yPosn ), ( pb->xPosn - pc->xPosn ) );
      if( angle0 < 0 ) angle0 += 2 * M_PI ;
      angle1 = angle0 + M_PI ;
      if( angle1 >= 2 * M_PI ) angle1 -= 2 * M_PI ;

      lnum = 0;

      pc->linkdirect = (double *)malloc( sizeof( double));
      pc->linkverts = (pntDescript **)malloc( sizeof (pntDescript *));
      lnum++;
      pc->linkdirect[lnum-1] = angle0;
      pc->linkverts[lnum-1] = pb;
      pc->linknum = lnum;


	  
      /* Now fill in the fields of the previous link */
      lnum = pb->linknum;

	if( lnum == 0 ) {
	  pb->linkdirect = (double *)malloc( sizeof( double));
	  pb->linkverts = (pntDescript **)malloc( sizeof (pntDescript *));
	}
	else {
	  pb->linkdirect = (double *)realloc( pb->linkdirect, (lnum + 1) * sizeof( double));
	  pb->linkverts = (pntDescript **)realloc( pb->linkverts, (lnum + 1) *
						   sizeof (pntDescript *));
	}
	lnum++;
	pb->linkdirect[lnum-1] = angle1;
	pb->linkverts[lnum-1] = pc;
	pb->linknum = lnum;
    }

    ptr_old = pc;

    
  }

  free( tmpdataHolder );
  return (result);

}

int strip_duplicate_tracks( BTREE *btr ) {

  /* local */

  int i, j;   /* loop */
  int pass_num = 0;
  double tol;
  int res;
  int ntracks, del_failed;

  char *key0;
  void *data0;
  pntDescript **data1, *pc, *pt1, *pt2, *pt3;
  double pt1_len, pt2_len;

  int passing = 1; /* Flag to indicate that we are still executing passes 
		      through the database
		   */
  int do_continue = 1;


  fprintf(stderr, "Processing network: cleaning lines. . .\n");

  if(G_process_colinear_tolerance(GET_VAL, &tol) < 0)
    tol = 2.0 * M_PI / 180.0;

  while(passing) {

    fprintf(stderr, "    Pass Number  %d\n", ++pass_num);

    btree_rewind(btr);
    passing = 0;

    ntracks = 0;
    del_failed = 0;
  
    while(btree_next(btr, &key0, &data0)) {

      /* Examine each node */

      data1 = (pntDescript **)data0;

      pc = *data1;  /* Find current vertex */

      if(pc->linknum < 3)
	continue;

      do_continue = 1;

      for( i = 0; i < pc->linknum; i++ ) {

	if(!do_continue)
	  break;

	for( j = i + 1; j < pc->linknum; j++ ) {

	  if(!do_continue)
	    break;	  

	  if( fabs(pc->linkdirect[i] - pc->linkdirect[j]) < tol ) {

	    /* These two tracks are colinear */
	    passing  = 1;
	    do_continue = 0; /* Do just one pair in each pass for each vertex */

	    /* Track the lines until they move off the current linear trajectory 
	       (or hit a node)
	    */
	    pt1 = track_to_end(pc, i);
	    pt2 = track_to_end(pc, j);

	    /* If the end points are the same we have a degenerate link, simply strip
	       out line 2
	    */

	    if( pt1 == pt2 ) {
	      res = delete_track(pc, pt1, j);

	      ntracks++;

	      if(res < 0) {
		return -1;
	      }

	      else if(res > 0) {
		del_failed++;
	      }
	    }

	    /* If the track deviates at a certain point we have a snap-back line. 
	       Make the snap-back length into a single track.
	    */

	    else {
	      pt1_len = linear_track_length(pc, pt1, i);
	      pt2_len = linear_track_length(pc, pt2, j);

	      if(pt1_len < pt2_len) {
		pt3 = delete_track_in_circle(pc, j, pt2_len);
		add_link(pt1, pt3);
	      }
	      
	      else {
		pt3 = delete_track_in_circle(pc, i, pt1_len);
		add_link(pt2, pt3);
	      }
	    }
	    
	  }
	}
      }
    }

    if(ntracks == del_failed) {
      passing = 0;
    }
  }

  fprintf(stderr, "\nFinished cleaning.\n");

  return 0;
}


int vbase_extract_lines(BTREE *btr, struct Map_info *map) {

  /* local */

  int i, j;   /* loop */

  char *key0;
  void *data0;
  pntDescript **data1, *pc, *pb, *pc_base;
  struct line_pnts tmp_line;
  int np;
  int chaining;

  int lollipop;

  btree_rewind(btr);

  /* Run through the nodes first to extract compound hulls */

  while( btree_next(btr, &key0, &data0)) {

    lollipop = 0;

    data1 = (pntDescript **)data0;

    pc = *data1;  /* Find current vertex */

    if(pc->linknum == 2 || pc->linknum == 0)
      continue;

    else if(pc->linknum == 1)
      lollipop = 1;

    pc_base = pc;

    for( i = 0; i < pc->linknum; i++ ) {

      /* Avoid old tracks */

      if(pc->linkverts[i]->duff)
	continue;

      /* Chain till we hit another link */

      memset(&tmp_line, 0, sizeof(struct line_pnts));

      if( dig_alloc_points(&tmp_line, 200) < 0 ) {
	fprintf(stderr, "Error allocating dynamic memory.\n");
	return -1;
      }

      if( (np = Vect_append_point(&tmp_line, pc->xPosn, pc->yPosn)) == 0 ) {
	fprintf(stderr, "Error allocating dynamic memory.\n");
	return -1;
      }

      pb = pc;
      pc = pc->linkverts[i];

      if(pc->linknum == 1) {
	/* This is a short lollipop. Abort. */
	pc = pc_base;
	continue;
      }

      else if(pc->linknum > 2) {
	/* This is now a node */

	/* A short line so insert an intermediate point */

	if( (np = Vect_append_point(&tmp_line, (pc->xPosn + pb->xPosn) / 2.0,
				    (pc->yPosn + pb->yPosn) / 2.0)) == 0 ) {
	  fprintf(stderr, "Error allocating dynamic memory.\n");
	  return -1;
	}

	if( (np = Vect_append_point(&tmp_line, pc->xPosn, pc->yPosn)) == 0 ) {
	  fprintf(stderr, "Error allocating dynamic memory.\n");
	  return -1;
	}

	/* Write the line and clean up */

	if(!Vect_write_line(map, AREA, &tmp_line)) {
	  fprintf(stderr, "Error writing line.\n");
	  return -1;
	}

	G_free(tmp_line.x);
	G_free(tmp_line.y);

	pc = pc_base;
	continue;

      }

      /* Otherwise start chaining */

      chaining = 1;

      while(chaining) {

	if( pc == pc_base ) {
	  pb = pc;
	  pc = pb->linkverts[i];
	}

	else {

	  for( j = 0; j < 2; j++ ) {
	    if(pc->linkverts[j] == pb)
	      continue;
	    else {
	      pb = pc;
	      pc = pb->linkverts[j];
	      break;
	    }
	  }
	}

	if(pc->linknum == 1) {
	  /* This is a long lollipop. Abort. */
	  G_free(tmp_line.x);
	  G_free(tmp_line.y);
	  chaining = 0;	  
	}

	else if(pc->linknum > 2) {
	  /* Reached the next node. Terminate line and write */
	  if( (np = Vect_append_point(&tmp_line, pc->xPosn, pc->yPosn)) == 0 ) {
	    fprintf(stderr, "Error allocating dynamic memory.\n");
	    return -1;
	  }
	  
	  if(!lollipop) {
	    if(!Vect_write_line(map, AREA, &tmp_line)) {
	      fprintf(stderr, "Error writing line.\n");
	      return -1;
	    }
	  }

	  G_free(tmp_line.x);
	  G_free(tmp_line.y);
	  chaining = 0;
	  /* Don't blank terminal node */
	}

	else {
	  /* Midline, add point, blank point and continue */
	  if( (np = Vect_append_point(&tmp_line, pc->xPosn, pc->yPosn)) == 0 ) {
	    fprintf(stderr, "Error allocating dynamic memory.\n");
	    return -1;
	  }
	  pc->duff = 1;
	}
      }  /* End chaining */

      pc = pc_base;
    }

    pc_base->duff = 1;  /* Now blank the node, since we have finished with it */

  }  /* End `btree_next' */

  btree_rewind(btr);

  /* Run through the pseudonodes to extract simple islands */

  while( btree_next(btr, &key0, &data0)) {

    /* Avoid old tracks and re-tracking nodes */

    data1 = (pntDescript **)data0;

    pc = *data1;  /* Find current vertex */


    if(pc->linknum != 2 || pc->duff)
      continue;

    /* So we have a pseudonode. Only track one way */

    /* Avoid old tracks */

    if(pc->linkverts[0]->duff)
      continue;   /* Shouldn't happen */


    /* Chain till we hit another link */

    pc_base = pc;

    memset(&tmp_line, 0, sizeof(struct line_pnts));

    if( dig_alloc_points(&tmp_line, 200) < 0 ) {
      fprintf(stderr, "Error allocating dynamic memory.\n");
      return -1;
    }

    if( (np = Vect_append_point(&tmp_line, pc->xPosn, pc->yPosn)) == 0 ) {
      fprintf(stderr, "Error allocating dynamic memory.\n");
      return -1;
    }

    pb = pc;
    pc = pc->linkverts[0];

    if( pb == pc ) {
      /* Degenerate island. Ignore and continue. */
      G_free(tmp_line.x);
      G_free(tmp_line.y);
      continue;
    }

    else {
      if( (np = Vect_append_point(&tmp_line, pc->xPosn, pc->yPosn)) == 0 ) {
	fprintf(stderr, "Error allocating dynamic memory.\n");
	return -1;
      }

    }


    /* Start chaining */

    chaining = 1;

    while(chaining) {


      for( j = 0; j < 2; j++ ) {
	if(pc->linkverts[j] == pb)
	  continue;
	else {
	  pb = pc;
	  pc = pb->linkverts[j];
	  break;
	}
      }

      if(pc == pc_base) {
	/* Reached the next node. Terminate line and write */
	if( (np = Vect_append_point(&tmp_line, pc->xPosn, pc->yPosn)) == 0 ) {
	  fprintf(stderr, "Error allocating dynamic memory.\n");
	  return -1;
	}
	  
	if(!Vect_write_line(map, AREA, &tmp_line)) {
	  fprintf(stderr, "Error writing line.\n");
	  return -1;
	}

	G_free(tmp_line.x);
	G_free(tmp_line.y);
	chaining = 0;
	/* Don't blank terminal node */
      }

      else {
	/* Midline, add point, blank point and continue */
	if( (np = Vect_append_point(&tmp_line, pc->xPosn, pc->yPosn)) == 0 ) {
	  fprintf(stderr, "Error allocating dynamic memory.\n");
	  return -1;
	}
	pc->duff = 1;
      }
    }  /* End chaining */

    pc_base->duff = 1;   /* Now blank this pseudonode */

      
  }  /* End `btree_next' */

  return 0;
}


int procSnapDistance( int iswitch, float *sd ) {
  
  /* Set or get the SNAP_DISTANCE variable */

  static float snap_distance = 1.0e-6;

  if( iswitch == SET_VAL ) {
    if(sd) {
      snap_distance = *sd;
      return 0;
    }
    else return 1;
  }
  else if( iswitch == GET_VAL ) {
    *sd = snap_distance;
    return 0;
  }
  else return 1;
}


char *calcKeyValue( pntDescript *pnt1, double sr, int decs, double efalse,
		    double nfalse ) {
  /* local */

  double xtmp, ytmp;
  char xbuf[128], ybuf[128];
  char *retbuf;
  int indx;
  char *indx_ptr;
  int idigits;

  xtmp = ((long long)( (pnt1->xPosn + efalse) / sr )) * sr;
  ytmp = ((long long)( (pnt1->yPosn + nfalse) / sr )) * sr;

  /* To elliminate small negative values */
  if(xtmp < 0.0) xtmp = 0.0;
  if(ytmp < 0.0) xtmp = 0.0;

  if(decs < 0 || decs > 16)
    return NULL;

  idigits = 16 - decs;

  retbuf = (char *)malloc( 33 );
  
  snprintf( xbuf, 127, "%065.20f", xtmp );
  snprintf( ybuf, 127, "%065.20f", ytmp );

  indx_ptr = strchr( xbuf, '.' );
  strncpy( retbuf, indx_ptr - idigits, idigits );
  retbuf[idigits] = '\0';
  strncat( retbuf, indx_ptr + 1, decs );
  retbuf[16] = '\0';

  indx_ptr = strchr( ybuf, '.' );
  strncat( retbuf, indx_ptr - idigits, idigits );
  retbuf[16 + idigits] = '\0';
  strncat( retbuf, indx_ptr + 1, decs );
  retbuf[32] = '\0';

  return retbuf;
}


int btree_compare( char *key1, char *key2 ) {
  /* Just compare lexicographically */

  return strncmp( key1, key2, 32 );
}

/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

   Cleaning routines

   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

pntDescript *delete_track_in_circle(pntDescript *pt0, int track, double radius) {

  /* Delete the first part of a track lying within a given radius 
     of a starting point
  */

  pntDescript *p1, *p2, *pb;

  double rlen;

  p1 = pt0;
  p2 = pt0->linkverts[track];

  delete_link(p1, p2);

  rlen = hypot(p1->xPosn - p2->xPosn, p1->yPosn - p2->yPosn);

  while(rlen <= radius) {

    if(p2->linknum != 2)
      break;

    p1 = p2;
    p2 = p1->linkverts[0];
    delete_link(p1, p2);
    rlen = hypot(pt0->xPosn - p2->xPosn, pt0->yPosn - p2->yPosn);
    
  }

  return p2;
}


int add_link(pntDescript *p1, pntDescript *p2) {

  /* local */

  int i; /* loop */

  double snap;
  double dist, outset;

  int nlinks;
  pntDescript **ppPt;
  double *pd;

  if(G_process_snap_distance(GET_VAL, &snap) < 0)
    snap = 1.0e-7;

  dist = hypot(p1->xPosn - p2->xPosn, p1->yPosn - p2->yPosn);

  if(dist < snap){
    fprintf(stderr, "WARNING: Not building link as the two target points are effectively the same to within the snap distance.\n");
    return 1;
  }

  outset = atan2(p2->yPosn - p1->yPosn, p2->xPosn - p1->xPosn);
  
  if(outset < 0.0)
    outset += 2 * M_PI;


  /* Check if the link already exists */

  for( i = 0; i < p1->linknum; i++ ) {
    
    if(p1->linkverts[i] == p2) {
      
      /* This link already exists. Do not build (or report). Return NO_ACTION. */
      return 1;
    }
    
  }

  /* Might as well check the other way */

  for( i = 0; i < p2->linknum; i++ ) {
    
    if(p2->linkverts[i] == p1) {
      
      /* This link already exists. Do not build (or report). Return NO_ACTION. */
      return 1;
    }
    
  }



  /* Add links */

  nlinks = p1->linknum;

  ppPt = (pntDescript **)realloc(p1->linkverts, ++nlinks * sizeof(pntDescript *));

  if(ppPt == NULL) {
    fprintf(stderr, "Error allocating a new link to a vertex.\n");
    return -1;
  }

  else {
    p1->linkverts = ppPt;
    p1->linkverts[nlinks - 1] = p2;
  }

  pd = (double *)realloc(p1->linkdirect, nlinks * sizeof(double));

  if(pd == NULL) {
    fprintf(stderr, "Error allocating a new directional link to a vertex.\n");
    return -1;
  }

  else {
    p1->linkdirect = pd;
    p1->linkdirect[nlinks - 1] = outset;
  }

  p1->linknum = nlinks;


  outset -= M_PI;
  
  if(outset < 0.0)
    outset += 2 * M_PI;

  nlinks = p2->linknum;

  ppPt = (pntDescript **)realloc(p2->linkverts, ++nlinks * sizeof(pntDescript *));

  if(ppPt == NULL) {
    fprintf(stderr, "Error allocating a new link to a vertex.\n");
    return -1;
  }

  else {
    p2->linkverts = ppPt;
    p2->linkverts[nlinks - 1] = p1;
  }

  pd = (double *)realloc(p2->linkdirect, nlinks * sizeof(double));

  if(pd == NULL) {
    fprintf(stderr, "Error allocating a new directional link to a vertex.\n");
    return -1;
  }

  else {
    p2->linkdirect = pd;
    p2->linkdirect[nlinks - 1] = outset;
  }

  p2->linknum = nlinks;


  return 0;
  
}


int delete_link(pntDescript *p1, pntDescript *p2) {

  /* Remove the link between the two points */

  int i;   /* loop */

  /* local */

  pntDescript **ppPt;
  double *pd;
  int link1, link2;


  /* Find the index of 1->2 */

  link1 = -1;

  for( i = 0; i < p1->linknum; i++ ) {

    if(p1->linkverts[i] == p2)
      link1 = i;
  }

  if(link1 < 0) {
    
    /* The points are not linked: can't unlink */
    return 1;
  }

  /* Find the index of 2->1 */

  link2 = -1;

  for( i = 0; i < p2->linknum; i++ ) {

    if(p2->linkverts[i] == p1)
      link2 = i;
  }

  if(link2 < 0) {
    
    /* The points are not linked: can't unlink */
    return 1;
  }

  
  /* Got here, so the points are linked symmetrically. OK. */

  if(link1 < p1->linknum) {

    for( i = link1; i < p1->linknum; i++ ) {

      p1->linkverts[i] = p1->linkverts[i + 1];
      p1->linkdirect[i] = p1->linkdirect[i + 1];
    }
  }

  if(p1->linknum > 1) {
    ppPt = (pntDescript **)realloc(p1->linkverts, (p1->linknum - 1) * sizeof(pntDescript *));

    if(ppPt == NULL) {
      fprintf(stderr, "Error reallocating space for vertex links.\n");
      return -1;
    }

    else {
      p1->linkverts = ppPt;
    }

    pd = (double *)realloc(p1->linkdirect, (p1->linknum - 1) * sizeof(double));

    if(pd == NULL) {
      fprintf(stderr, "Error reallocating space for directional vertex links.\n");
      return -1;
    }

    else {
      p1->linkdirect = pd;
    }

    p1->linknum--;

  }

  else {
    free(p1->linkverts);
    free(p1->linkdirect);
    p1->linknum = 0;
  }



  if(link2 < p2->linknum) {

    for( i = link2; i < p2->linknum; i++ ) {

      p2->linkverts[i] = p2->linkverts[i + 1];
      p2->linkdirect[i] = p2->linkdirect[i + 1];
    }
  }

  if(p2->linknum > 1) {
    ppPt = (pntDescript **)realloc(p2->linkverts, (p2->linknum - 1) * sizeof(pntDescript *));

    if(ppPt == NULL) {
      fprintf(stderr, "Error reallocating space for vertex links.\n");
      return -1;
    }

    else {
      p2->linkverts = ppPt;
    }

    pd = (double *)realloc(p2->linkdirect, (p2->linknum - 1) * sizeof(double));

    if(pd == NULL) {
      fprintf(stderr, "Error reallocating space for directional vertex links.\n");
      return -1;
    }

    else {
      p2->linkdirect = pd;
    }

    p2->linknum--;
  }

  else {
    free(p2->linkverts);
    free(p2->linkdirect);
    p2->linknum = 0;
  }


  /* Deletion process finished */

  return 0;


}


double linear_track_length(pntDescript *p1, pntDescript *p2, int track) {

  /* Find the track length along a track
     - not the direct distance - 
  */

  /* A negative return indicates that the track is not valid
     or has intervening nodes
  */

  /* local */

  pntDescript *pb, *pc, *p0;
  double dist = 0.0;

  pb = p1;
  pc = p1->linkverts[track];

  dist += hypot(pc->xPosn - pb->xPosn, pc->yPosn - pb->yPosn);

  while(pc != p2) {
    
    if(pc->linknum != 2)
      return -1.0;

    p0 = pb;
    pb = pc;

    if(pb->linkverts[0] == p0)
      pc = pb->linkverts[1];
    else
      pc = pb->linkverts[0];

    dist += hypot(pc->xPosn - pb->xPosn, pc->yPosn - pb->yPosn);
  }

  return dist;
}


pntDescript *track_to_end(pntDescript *pt0, int track) {

  /* Follow a track and stick with it until the track deflects from
     the current colinear 
  */

  /* local */

  int continuing = 1;
  double direct0, direct, tol0;

  pntDescript *pb, *pc, *p0;


  if(G_process_colinear_tolerance(GET_VAL, &tol0) < 0)
    tol0 = 2.0 * M_PI / 180.0;

  pb = pt0;
  pc = pb->linkverts[track];

  direct0 = atan2(pc->yPosn - pb->yPosn, pc->xPosn - pb->xPosn);

  if(direct0 < 0.0)
    direct0 += 2 * M_PI;

  direct = direct0;

  while( continuing ) {

    if(pc->linknum != 2) {
      pb = pc;
      break;
    }

    p0 = pb;
    pb = pc;

    if(pb->linkverts[0] == p0)
      pc = pb->linkverts[1];
    else
      pc = pb->linkverts[0];

    direct = atan2(pc->yPosn - pb->yPosn, pc->xPosn - pb->xPosn);

    if( fabs(direct0 - direct) < tol0 )
      continuing = 0;
  }

  return pb;
}


int delete_track(pntDescript *p1, pntDescript *p2, int track) {

  /* Delete from p1 to p2, along `track' if valid. If the terminal
     vertex is not encountered before a node, no action is taken
  */

  int i; /* loop */

  /* local */

  pntDescript *pb, *pc, *p0;
  pntDescript **ppPt, **ppPt0;

  int size = 1000, incr = 200;
  int track_size = 0;

  ppPt = (pntDescript **)malloc( size * sizeof(pntDescript *));

  if(ppPt == NULL) {
    fprintf(stderr, "Couldn't allocate space for processing.\n");
    return -1;
  }
     

  pb = p1;
  pc = pb->linkverts[track];

  /* Is this a simple single link? */

  if(pc == p2) {

    return delete_link(p1, p2);
  }

  while(pc != p2) {

    if(pc->linknum != 2)
      return 1;

    p0 = pb;
    pb = pc;

    if(pb->linkverts[0] == p0)
      pc = pb->linkverts[1];
    else
      pc = pb->linkverts[0];

    if(track_size + 1 > size) {
      size += incr;
      ppPt0 = (pntDescript **)realloc(ppPt, size * sizeof(pntDescript *));

      if(ppPt0 == NULL) {
	fprintf(stderr, "Error allocating memory for processing.\n");
	return -1;
      }

      else {
	ppPt = ppPt0;
      }
    }

    ppPt[track_size++] = pb;

  }

  /* Transaction successful. Now process. */

  delete_link(p1, ppPt[0]);

  for( i = 0; i < track_size - 1; i++ ) {

    delete_link(ppPt[i], ppPt[i + 1]);
  }

  delete_link(ppPt[track_size - 1], p2);

  free(ppPt);
  return 0;
}
