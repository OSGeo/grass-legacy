/******************************************************************************
 * vmap_import.c [v.in.shape]
 * Import ESRI Shapefile: main import routine
 * 

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 27th. Feb. 2002
 * Last updated 27th. Mar. 2002
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
#include <math.h>
#include "vmap_import.h"
#include "lines.h"
#include "shapefil.h"
#include "basename.h"

/* Static variables for information about this theme */
static int theme_type = SHPT_POLYGON; /* By default */
static int force_line_extract = 0; /* Import polygons as line coverage if set
				      - not by default
				   */
static int num_entries = 0;
static double E_bound = 0.0, N_bound = 0.0, W_bound = 0.0, S_bound = 0.0;

static int acnt = 0;
static int unique_atts = 0;

/* Static routines */

static void set_force_line_extract(int);
static void set_unique_atts(int);
static void trim_space(char *, char *);

/* Main routine. */

int vmap_import(param_import_ctrl *pic0) {

  char shp_basename[512] = "";
  char buffer1[512] = "";
  char cat_title[512] = "";

  int res1, res2;
  int go_out = 0;
  int null_recs = 0;
  int vert_cnt = 0;
  int part_cnt = 0;
  int no_atts = 0;
  int verb = pic0->verbose_level;
  SHPHandle hshp;
  DBFHandle hdbf;
  FILE *hlog, *fatt;
  int att_idx, cat_idx;
  SHPObject *shpobj;
  struct Categories cats1;
  struct line_pnts *lp1;
  struct Map_info Map1;
  polygon_ctrl *pgc1 = NULL;
  ringset *prings;
  repository *vrep;

  /* loop */
  int ia, ib, ic;

  double bounds1[4], bounds2[4];
  double apx, apy;

  /* Do we want to disable polygon deconstruction? */

  set_force_line_extract(pic0->fle);
  set_unique_atts(pic0->unique);

  /* Set log file to more convenient handle */
  hlog = pic0->logptr;

  /* Construct the full path of the base name */
  res1 = get_file_pathdir(shp_basename, pic0->src);
  res2 = get_file_basename(buffer1, pic0->src, "shp:shx:dbf");
  strncat(shp_basename, buffer1, 511);

  /* Try to open the shapefile */
  hshp = SHPOpen(shp_basename, "rb");

  if(hshp == NULL) {
    
    if(verb > 0) {
      fprintf(hlog, "Couldn't open input file. ");
      
      if(res1 == 1) {
	fprintf(hlog, "You did not supply a full path in the input name.\n");
	fprintf(hlog, "Could that be an error?\n");
      }

      else fprintf(hlog, "\n");
    }

    return(-1);
  }

  /* Try to open DBF file */
  hdbf = DBFOpen(shp_basename, "rb");

  if(hdbf == NULL) {
    if(verb > 0) {
      fprintf(hlog, "Couldn't open input database file.\n");
    }
    
    SHPClose(hshp);
    return(-1);
  }

  /* Try to open the vector map file */

  res1 = Vect_open_new(&Map1, pic0->dest);
  if(res1 < 0) {
    if(verb > 0) {
      fprintf(hlog, "Couldn't open vector map for writing.\n");
    }

    SHPClose(hshp);
    DBFClose(hdbf);
    return(-1);
  }

  /* Try to open dig_att file */

  fatt = G_fopen_new("dig_att", pic0->dest);
  if(fatt == NULL) {
    if(verb > 0) {
      fprintf(hlog, " WARNING: Couldn't acquire write permissions for attributes.\n");
      fprintf(hlog, "          Ensure you have write permissions for the database.\n");
      fprintf(hlog, "          If the problem persists please report as a bug.\n\n");
    }
    fprintf(stderr, " WARNING: Couldn't acquire write permissions for attributes.\n");
    fprintf(stderr, "          Ensure you have write permissions for the database.\n");
    fprintf(stderr, "          If the problem persists please report as a bug.\n\n");
    no_atts = 1;
  }

  /* Initialise cat structure if it is likely going to be required */
  
  sprintf(cat_title, "Vector map: %s", pic0->dest);

  G_init_cats(1, cat_title, &cats1);

  /* Find the field numbers for the attribute and category values, if valid */

  shp_get_field_idx(hdbf, pic0, &att_idx, &cat_idx);


  /* Extract overall shapefile info */

  SHPGetInfo(hshp, &num_entries, &theme_type, &bounds1[0], &bounds2[0]);
  W_bound = bounds1[0];
  S_bound = bounds1[1];
  E_bound = bounds2[0];
  N_bound = bounds2[1];


  /* Scan Shapefile. */
  

  if(verb > 0) {
    fprintf(hlog, "\nFile `%s.shp' is of Type ", buffer1);
  }

  if(IS_MULTIPATCH_TYPE(theme_type)) {
    if(verb > 0) {
      fprintf(hlog, "Multipatch.\n\nThis type is not supported at present.\nAborting.\n");
    }
    go_out = 1;
  }

  else if(IS_NULL_TYPE(theme_type)) {
    if(verb > 0) {
      fprintf(hlog, "Null.\n\nNull types only. Apparently, theme has no data.\nAborting.\n");
    }
    go_out = 1;
  }

  else if(IS_POINT_TYPE(theme_type)) {
    if(verb > 0) {
      fprintf(hlog, "Site (Point) ");
    }
  }

  else if(IS_MULTIPOINT_TYPE(theme_type)) {
    if(verb > 0) {
      fprintf(hlog, "Point location classes ");
    }
  }
	
  else if(IS_ARC_TYPE(theme_type)) {
    if(verb > 0) {
      fprintf(hlog, "Line (Arc) ");
    }
  }

  else if(IS_POLYGON_TYPE(theme_type)) {
    if(verb > 0) {
      fprintf(hlog, "Area (Polygon) ");
    }
  }

  else {
    if(verb > 0) {
      fprintf(hlog, "Unknown. \n\nAborting.\n ");
    }
    go_out = 1;
  }	


  if(go_out) {
    SHPClose(hshp);
    DBFClose(hdbf);
    Vect_close(&Map1); 
    if(fatt) fclose(fatt);
    return(-1);
  }

  if(verb > 0) {
    fprintf(hlog, "with %d entries.\n\n", num_entries);
    fprintf(hlog, "Bounds:\nW:%16.6lf, E:%16.6lf, S:%16.6lf, N:%16.6lf.\n\n",
	    W_bound, E_bound, S_bound, N_bound);
  }

  for ( ia = 0; ia < num_entries; ia++ ) {

    if(verb > 1) {
      fprintf(hlog, "Shape No. %d: ", ia + 1);
    }

    shpobj = SHPReadObject(hshp, ia);
    if(shpobj == NULL) {
      if(verb > 1) {
	fprintf(hlog, "\n*WARNING* Couldn't read shape.\n\n");
      }
      null_recs++;
      SHPDestroyObject(shpobj);
      continue;
    }

    else if(shpobj->nSHPType == SHPT_NULL) {
      if(verb > 1) {
	fprintf(hlog, "NULL Shape.\n\n");
      }
      SHPDestroyObject(shpobj);
      continue;
    }

    else {
      if(verb > 1) { 
	if(IS_ARC_TYPE(shpobj->nSHPType)) {
	  fprintf(hlog, "Line Shape with %d vertices.\n", shpobj->nVertices);
	}
	else if(IS_POINT_TYPE(shpobj->nSHPType)) {
	  fprintf(hlog, "Site Shape with %d vertices.\n", shpobj->nVertices);
	}
	else if(IS_MULTIPOINT_TYPE(shpobj->nSHPType)) {
	  fprintf(hlog, "Site Class Shape with %d vertices.\n", shpobj->nVertices);
	}
	else if(IS_POLYGON_TYPE(shpobj->nSHPType)) {
	  fprintf(hlog, "Polygon Shape with %d vertices.\n", shpobj->nVertices);
	}
	else {
	  fprintf(hlog, "Unknown type. Continuing. \n\n");
	  SHPDestroyObject(shpobj);
	  continue;
	}
      }

      if(IS_POLYGON_TYPE(shpobj->nSHPType) && !force_line_extract) {
	pgc1 = polygon_ctrl_init();
      
	if(pgc1 == NULL) {
	  if(verb > 0) {
	    fprintf(hlog, "    ERROR: Memory allocation error.\n\n");
	  }
	  SHPDestroyObject(shpobj);
	  SHPClose(hshp);
	  DBFClose(hdbf);
	  Vect_close(&Map1); 
	  if(fatt) fclose(fatt);
	  return(-1);
	}
      }

      /* Go through parts - only print this at highest verbosity level */

      if(IS_POINT_TYPE(shpobj->nSHPType) || IS_MULTIPOINT_TYPE(shpobj->nSHPType)) {

	for(ib = 0; ib < shpobj->nVertices; ib++) {

	  part_cnt++;

	  if(verb > 2) {
	    fprintf(hlog, "    Vertex: %16.6lf  %16.6lf\n", 
		    shpobj->padfX[ib], shpobj->padfY[ib]);
	  }

	  lp1 = Vect_new_line_struct();
	  if(lp1 == NULL) {
	    if(verb > 0) {
	      fprintf(hlog, "    ERROR: Memory allocation error.\n\n");
	    }
	    Vect_destroy_line_struct(lp1); /* Clean up before exit. */
	    SHPDestroyObject(shpobj);
	    SHPClose(hshp);
	    DBFClose(hdbf);
	    Vect_close(&Map1);
	    if(fatt) fclose(fatt);
	    return(-1);
	  }

	  /* Strange duplication of a point in a vector site file */
	  Vect_append_point(lp1, shpobj->padfX[ib], shpobj->padfY[ib]);
	  Vect_append_point(lp1, shpobj->padfX[ib], shpobj->padfY[ib]);

	  /* Write here */

	  res1 = shp_write_fields(pic0, fatt, hdbf, &cats1, ia, att_idx, cat_idx,
				  shpobj->padfX[ib], shpobj->padfY[ib], 'P');
	  if(verb >0 ) {
	    switch(res1) {
	    case -1:
	      {
		fprintf(hlog, "    WARNING: Could not write attribute for record %d.\n\n",
			ia);
		break;
	      }
	    case -2:
	      {
		fprintf(hlog, "    WARNING: Could not write category for record %d.\n\n",
			ia);
		break;
	      }
	    default:
	      {
	      }
	    }
	  }

	  res1 = Vect_write_line(&Map1, DOT, lp1);
	  if(res1 <= 0) {
	    if(verb > 0) {
	      fprintf(hlog, "    ERROR: Memory allocation error.\n\n");
	    }
	    Vect_destroy_line_struct(lp1); /* Clean up before exit. */
	    SHPDestroyObject(shpobj);
	    SHPClose(hshp);
	    DBFClose(hdbf);
	    Vect_close(&Map1);
	    return(-1);
	  }
	    
	  Vect_destroy_line_struct(lp1);
	}
      }

      else {   

	for(ib = 0; ib < shpobj->nParts; ib++) {

	  part_cnt++;
	
	  if(verb > 2) {
	    fprintf(hlog, "  Part %d\n", ib + 1);
	  }

	  res1 = shp_extract_line(shpobj, ib, &lp1);

	  if(res1 > 0) {
	    if(verb > 2) {
	      fprintf(hlog, "    WARNING: Part %d of shape not found. Ignoring.\n\n", ib + 1);
	    }
	  }

	  else if(res1 < 0) {
	    if(verb > 0) {
	      fprintf(hlog, "    ERROR: Memory allocation error.\n\n");
	    }
	    Vect_destroy_line_struct(lp1); /* Clean up before exit. */
	    SHPDestroyObject(shpobj);
	    SHPClose(hshp);
	    DBFClose(hdbf);
	    Vect_close(&Map1);
	    if(fatt) fclose(fatt);
	    return(-1);
	  }

	  if(lp1->n_points == 2) {
	    shp_add_third_point(lp1);
	  }

	  /* Write the line */

	  if(IS_ARC_TYPE(shpobj->nSHPType) || 
	     (IS_POLYGON_TYPE(shpobj->nSHPType) && force_line_extract)) {

	    if(IS_ARC_TYPE(shpobj->nSHPType)) {
	      res1 = shp_write_fields(pic0, fatt, hdbf, &cats1, ia, att_idx, cat_idx,
				      lp1->x[1], lp1->y[1], 'L');
	      if(verb >0 ) {
		switch(res1) {
		case -1:
		  {
		    fprintf(hlog, "    WARNING: Could not write attribute for record %d.\n\n",
			    ia);
		    break;
		  }
		case -2:
		  {
		    fprintf(hlog, "    WARNING: Could not write category for record %d.\n\n",
			    ia);
		    break;
		  }
		default:
		  {
		  }
		}
	      }
	    }

	    res1 = Vect_write_line(&Map1, LINE, lp1);
	    if(res1 <= 0) {
	      if(verb > 0) {
		fprintf(hlog, "    ERROR: Memory allocation error.\n\n");
	      }
	      Vect_destroy_line_struct(lp1); /* Clean up before exit. */
	      SHPDestroyObject(shpobj);
	      SHPClose(hshp);
	      DBFClose(hdbf);
	      Vect_close(&Map1);
	      if(fatt) fclose(fatt);
	      return(-1);
	    }

	  }

	  else if(IS_POLYGON_TYPE(shpobj->nSHPType) && !force_line_extract) {

	    res1 = polygon_ctrl_append_ring(pgc1, lp1);
	    
	    if(res1 < 0) {
	      if(verb > 0) {
		fprintf(hlog, "    ERROR: Polygon controller contained error.\n\n");
	      }
	      Vect_destroy_line_struct(lp1); /* Clean up before exit. */
	      SHPDestroyObject(shpobj);
	      SHPClose(hshp);
	      DBFClose(hdbf);
	      Vect_close(&Map1);
	      if(fatt) fclose(fatt);
	      return(-1);
	    }

	  }

	  for(ic = 0; ic < lp1->n_points; ic++) {
	    /* Just assume lp2 has the same number of entries */
	  
	    if(verb > 2) {
	      fprintf(hlog, "    %16.6lf  %16.6lf\n", lp1->x[ic], lp1->y[ic]);
	    }

	  } /* ic */

	  Vect_destroy_line_struct(lp1);

	} /* ib */
      }
	
    }

    if(IS_POLYGON_TYPE(shpobj->nSHPType) && !force_line_extract) {

      /* carry out essential import operations here */


      /* Write dig_att file */
      if(!no_atts) {
	for(ic = 0; ic < pgc1->n_hulls; ic++) {
	  prings = polygon_holes_to_ringset(pgc1, ic, &res2);
	  if(! res2) {

	    if(pgc1->holes_per_hull[ic] > 0) {
	      res1 = Vect_get_point_in_poly_isl(&pgc1->hulls[ic], prings->rings,
						prings->n_rings, &apx, &apy);
	    }
	    else {
	      res1 = Vect_get_point_in_poly_isl(&pgc1->hulls[ic], NULL, 0, &apx, &apy);
	    }

	    if(res1 < 0) {
	      if(verb > 0) {
		fprintf(hlog, " WARNING: Couldn't get suitable area point for Shape %d, Ring %d.\n",
			ia + 1, ic + 1);
	      }
	    }

	    shp_write_fields(pic0, fatt, hdbf, &cats1, ia, att_idx, cat_idx,
			     apx, apy, 'A');

	    pgc1->centroid_x = apx;
	    pgc1->centroid_y = apy;
	    pgc1->struct_status |= POLY_WITH_CENTROID;
	  }
	  else if(res2 < 0){
	    /* There was an error creating the ring-set */
	    if(verb > 0)
	      fprintf(hlog, " ERROR: Couldn't create a ringset entity");
	    SHPClose(hshp);
	    DBFClose(hdbf);
	    Vect_close(&Map1);
	    if(fatt) fclose(fatt);
	    return (-1);
	  }
	  /* Else just go on, it is not a critical error, but note with warning. */

	  else if(res2 == 1) {
	    if(verb > 0) {
	      fprintf(hlog, " WARNING: There is no ring %d for shape %d.\n",
		      ic + 1, ia + 1);
	    }
	  }

	  else if(res2 == 2) {
	    if(verb > 0) {
	      fprintf(hlog, " WARNING: Shape record %d is malformatted.\n",
		      ia + 1);
	    }
	  }

	  /* Dispose of the ringset structure if it has been created. */
	  if(prings) {
	    ringset_destroy(prings);
	  }
	}
      }

      /* Import linework */

      res1 = vertices_vload(pgc1, pic0);

      if(res1 < 0) {
	if(verb > 0) {
	  fprintf(hlog, " ERROR: Memory Allocation Error during construction of lines.\n");
	}
	SHPClose(hshp);
	DBFClose(hdbf);
	Vect_close(&Map1);
	if(fatt) fclose(fatt);
	return (-1);
      }
      
      /* Write out polygon structure metadata to log file */
      
      if(verb > 1) {
	polygon_ctrl_dump(hlog, pgc1, ia + 1);
      }

      /* Dispose polygon control structure */
      polygon_ctrl_destroy(pgc1);
    }

    SHPDestroyObject(shpobj);
  } /* ia */


  /* If this is an area coverage, and it is not forced to write lines,
     import the lines from the repository here
  */

  if(IS_POLYGON_TYPE(theme_type) && !force_line_extract ) {

    /* vmap_clean(); */

    vmap_write_area_edges(&Map1);
    close_repository();
    delete_repository();
    clear_keybase();
  }


  /* Finally, write out the header. Maybe polish this up a bit */

  Map1.head.orig_scale = (long)pic0->scale;
  G_strncpy( Map1.head.your_name, G_whoami(), 20);
  G_strncpy( Map1.head.date, G_date(), 20);
  G_strncpy( Map1.head.map_name, pic0->dest, 20);
  Map1.head.W = W_bound;
  Map1.head.S = S_bound;
  Map1.head.E = E_bound;
  Map1.head.N = N_bound;

  /* Close repository etc. from here */

  res1 = G_write_vector_cats(pic0->dest, &cats1);
  if(res1 == -1) {
    if(verb > 0) {
      fprintf(hlog, "WARNING: The vector categories file could not be written.\n");
    }
  }
  G_free_cats(&cats1);

  SHPClose(hshp);
  DBFClose(hdbf);
  Vect_close(&Map1);
  if(fatt) fclose(fatt);
  return (0);
}


/* Extract the X and Y co-ordinates of the shape into a struct line_pnts
   Ignore Z and M as we cannot use them yet.
*/

int shp_extract_line(SHPObject *shp0, int idx, struct line_pnts **lpt0) {

  int offs1, offs2, nPartVertices, vcnt;
  double *vertex1, *vertex2;

  struct line_pnts *lpt1;

  if(idx > shp0->nVertices)
    return 1;

  offs1 = shp0->panPartStart[idx];

  if(idx == shp0->nParts - 1)
    offs2 = shp0->nVertices - 1;
  else
    offs2 = shp0->panPartStart[idx + 1] - 1;

  nPartVertices = offs2 - offs1 + 1;

  /* Get co-ord's */
  vertex1 = &shp0->padfX[offs1];
  vertex2 = &shp0->padfY[offs1];
  vcnt = 0;

  lpt1 = Vect_new_line_struct();

  while(++vcnt <= nPartVertices) {
    Vect_append_point(lpt1, *vertex1, *vertex2);
    if(vcnt <= nPartVertices) {
      vertex1++;
      vertex2++;
    }
  }

  *lpt0 = lpt1;
  return(0);
}


/* Insert a third point (for label) in a two-point line */

int shp_add_third_point(struct line_pnts *lpt0) {

  double xa, xb, ya, yb;

  xa = lpt0->x[1];
  ya = lpt0->y[1];

  xb = (lpt0->x[0] + xa) / 2.0;
  yb = (lpt0->y[0] + ya) / 2.0;

  lpt0->x[1] = xb;
  lpt0->y[1] = yb;

  Vect_append_point(lpt0, xa, ya);

  return (0);

}

int shp_get_field_idx(DBFHandle hDBF, param_import_ctrl *paric0, 
		      int *att0, int *cat0) {

  /* This is minimal, and could probably be better handled. */

  char attname[64] = "";
  char catname[64] = "";
  
  trim_space(attname, paric0->att);
  trim_space(catname, paric0->cat);

  if(strcmp(attname, "")) {
    *att0 = DBFGetFieldIndex(hDBF, attname);
  }

  else *att0 = -1;

  if(strcmp(catname, "")) {
    *cat0 = DBFGetFieldIndex(hDBF, catname);
  }

  else *cat0 = -1;

  return (0);
}


int shp_write_fields(param_import_ctrl *paric0, FILE *att_fp, DBFHandle hDBF, struct Categories *cats,
		     const int rec_num, const int att_num, const int cat_num,
		     const double x_coord0, const double y_coord0, char cov_type) {

  int no_atts = 0, no_cats = 0;
  int attval;
  DBFFieldType dbft;
  int go_out = 0;

  int f_size, f_decs;
  char buffer1[64] = "";
  char buffer2[256] = "";
  char tmp_buf[256] = "";
  const char *str_type;
  int val1, len1;
  long catval1;
  double d1;
  char *chptr;

  int ix;  /* loop */

  /* Is this record NULL? */

  if(att_num >= 0) {
    if(DBFIsAttributeNULL(hDBF, rec_num, att_num)) {
      return (1);
    }
  }

  /* First write atts */

  if(att_num < 0) {
    no_atts = 1;
  }

  if(cat_num < 0) {
    no_cats = 1;
  }

  if(no_atts) {
    if(unique_atts) {
      val1 = ++acnt;
    }
    else {
      val1 = rec_num + 1;
    }
  }

  else {
    dbft = DBFGetFieldInfo(hDBF, att_num, buffer1, &f_size, &f_decs);

    switch(dbft) {
    case FTString:
      {
	/* String: try to convert to a useful number. If this fails just leave
	   unlabelled, as if NULL
	*/

	str_type = DBFReadStringAttribute(hDBF, rec_num, att_num);
	val1 = atoi(str_type);

	if(val1 == 0) {
	  return (2);
	}

	break;

      }

    case FTInteger:
      {
	/* This is just an integer type, so assign directly */

	val1 = DBFReadIntegerAttribute(hDBF, rec_num, att_num);
	if(val1 == 0) {
	  return (2);
	}

	break;
      }

    case FTDouble:
      {
	/* Can't use directly, so round to nearest integer */
	val1 = (int) (DBFReadIntegerAttribute(hDBF, rec_num, att_num) + 0.5);
	
	if(val1 == 0) {
	  return (2);
	}

	break;
      }

    default:
      {
	/* There is an invalid field type */
	return (3);
      }
    }

  }

  fprintf(att_fp, "%c %16.6lf %16.6lf %10d\n", cov_type, x_coord0, y_coord0, val1);


  /* If we got this far it is valid to write cats if they are to be defined */

  if(!no_cats) {

    dbft = DBFGetFieldInfo(hDBF, cat_num, buffer1, &f_size, &f_decs);

    switch(dbft) {

    case FTString:
      {
	strncpy(buffer2, DBFReadStringAttribute(hDBF, rec_num, cat_num), f_size);
	break;
      }

    case FTInteger:
      {
	snprintf(buffer2, f_size + 1, "%d", DBFReadIntegerAttribute(hDBF, rec_num, cat_num));
	break;
      }

    case FTDouble:
      {
	d1 = DBFReadDoubleAttribute(hDBF, rec_num, cat_num);
	catval1 = (long)(d1 * pow(10.0, (double)f_decs) + 0.5);

	if(catval1 == 0) {
	  strcpy(buffer2, "0.");
	  for(ix = 0; ix < f_decs; ix++) {
	    strcat(buffer2, "0");
	  }
	}

	else {
	  sprintf(tmp_buf, "%.ld", catval1);
	  len1 = strlen(tmp_buf) - f_decs;
	  strncpy(buffer2, tmp_buf, len1);
	  chptr = &tmp_buf[len1];
	  strcat(buffer2, ".");
	  strncat(buffer2, chptr, f_decs);
	}

	break;
      }

    default:
      {
	strcpy(buffer2, "");
      }
    }

    G_set_cat( (CELL) val1, buffer2, cats);
  }

  return 0;
}


/* Some helper routines - ie. mainly for other files */

int get_vmap_bounds(double *eb0, double *nb0, double *wb0, double *sb0) {

  *eb0 = E_bound;
  *nb0 = N_bound;
  *wb0 = W_bound;
  *sb0 = S_bound;

  return (0);
}



/* Static routines */

static void set_force_line_extract(int fle_val) {

  force_line_extract = fle_val;
}

static void set_unique_atts(int unique0) {

  unique_atts = unique0;
}

static void trim_space(char *dest, char *src) {

  char *chptr1, *chptr2;
  int len1;

  char tmp_buf[512];

  strcpy(tmp_buf, src);
  chptr1 = &tmp_buf[0];

  while(*chptr1 == ' ') {
    chptr1++;
  }

  len1 = strlen(chptr1);
  chptr2 = &chptr1[len1 - 1];

  while(*chptr2 == ' ') {
    chptr2--;
    chptr2[1] = '\0';
  }

  strcpy(dest, chptr1);
}

