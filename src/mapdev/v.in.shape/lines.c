/******************************************************************************
 * lines.c [v.in.shape]
 * Import lines to a vbase.
 * 

 * @Copyright David D.Gray <ddgray@armadce.demon.co.uk>
 * 27th. Feb. 2002
 * Last updated 22nd. Mar. 2002
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
#include <unistd.h>
#include <string.h>
#include <math.h>
#include "lines.h"
#include "vmap_import.h"

#define VBASE_INITIAL 1000
#define VBASE_INCR 500
#define EXPANSION_FRACT 10
#define MAX_VERTICES_PER_LINE 50000

static repository *rp1 = NULL;
static BTREE *bt1 = NULL;
static param_import_ctrl *paric1 = NULL;

static long v_old = -1;

static double false_easting;
static double false_northing;
static int n_decs;

/* Static routines */

static int vkey_cmp(void *, void *);
static char *get_vkey( double, double, double, int, double, double);
static void set_region_params(void);
static void set_parameter_import_ctrl(param_import_ctrl *);

int vertices_vload(polygon_ctrl *pgc0, param_import_ctrl *par1) {

  /* Load the vertices of the current polygon into the vbase structures
     and create links. Create new rep and btree structs if necessary.
  */

  int res1;

  int ia;  /* loop */

  char rep_tmp_file[512], *rtf;

  /* Set static buffers on first call */

  if(paric1 == NULL) {
    set_parameter_import_ctrl(par1);
    set_region_params();
  }

  /* Create the repository */

  if(rp1 == NULL) {

    /* Get a temp file location for the repository */
    rtf = G_tempfile();
    strncpy(rep_tmp_file, rtf, 511);
    free(rtf);

    res1 = repository_init(rep_tmp_file);

    if(res1 < 0) {
      return (-1);
    }
  }


  v_old = -1; 

  /* Run through the rings of this polygon and add the lines to the vbase */

  /* Hulls */

  for (ia = 0; ia < pgc0->n_hulls; ia++) {

    res1 = vbase_add_line(&pgc0->hulls[ia]);
  }

  /* Holes */

  for (ia = 0; ia < pgc0->n_holes; ia++) {

    res1 = vbase_add_line(&pgc0->holes[ia]);
  }

  return (0);

}

int repository_init(char *rname) {

  int res1;

  rp1 = (repository *) malloc( sizeof(repository) );
  if(rp1 == NULL) {
    fprintf(stderr, "Couldn't assign memory for vertex repository during import of polygon theme.\n");
    return (-1);
  }
  strncpy(rp1->repname, rname, 511);
  rp1->fp = fopen(rp1->repname, "w+b");

  if(rp1->fp == NULL) {
    fprintf(stderr, "Unable to open temp file for writing.\n");
    free(rp1);
    return (-1);
  }

  rp1->open = 1;

  if(bt1 == NULL) {

    bt1 = (BTREE *) malloc( sizeof(BTREE) );
    if(bt1 == NULL) {
      /* Couldn't create btree */
      fprintf(stderr, "Unable to initialise vertex key database.\n");
      return (-1);
    }
      
    res1 = btree_create(bt1, vkey_cmp, VBASE_INCR);

    if(!res1) {
      /* Couldn't create btree */
      fprintf(stderr, "Unable to initialise vertex key database.\n");
      return (-1);
    }
  }


  return (0);
}


int repository_destroy(void) {

  int res;

  if(!rp1->open || (rp1->open && rp1->fp == NULL))
    res = 1;
  else res = 0;

  if(rp1->open && rp1->fp != NULL) {
    fclose(rp1->fp);
  }

  unlink(rp1->repname);

  free(rp1);
  btree_free(bt1);
  free(bt1);
  return (res);

}


/* General manipulation routines, ie. affecting both the repository and
   the indexed search tree
*/


int vbase_add_line(struct line_pnts *lpt0) {

  char *vk1;
  char *data1;
  long *offs1;
  int fres, sres, rres, vres, wres, ures;
  int old_link;
  vertex vt1, *vptr1;

  /* loop */
  int ia, ib;

  /* blank the static `old' vertex field */

  v_old = -1;

  /* Try to retrieve vertices from the database */

  for ( ia = 0; ia < lpt0->n_points; ia++ ) {

    vk1 = get_vkey(lpt0->x[ia], lpt0->y[ia], paric1->snap, n_decs,
		   false_easting, false_northing);

    fres = btree_find(bt1, vk1, &data1);

    /* We need to add a check here to check for near neighbours in
       adjacent cells, to prevent the barrier effect near edges, ie.
       two vertices are indefinitely close, but on separate sides of
       the boundary, so are in different cells: they therefore have
       different keys
    */

    if(!fres) {
      /* The vertex was not present, so add it to both the repository
	 and the search tree
      */

      sres = fseek(rp1->fp, 0, SEEK_END);
      if(sres) {
	return (-1);
      }

      /* Build the vertex */

      vptr1 = vertex_init(lpt0->x[ia], lpt0->y[ia]);
      if(vptr1 == NULL) {
	return (-1);
      }

      offs1 = (long *) malloc( sizeof(long) );
      if(offs1 == NULL) {
	free(vptr1);  
	return (-1);
      }

      *offs1 = ftell(rp1->fp) / sizeof(vertex) ;

      if(v_old > -1) {
	vres = vertex_add_link(vptr1, *offs1);

	if(vres == 1) {
	  /* No previous to link to */

	  /* But not a problem so just go on. */
	}

	else if(vres == -1) {
	  /* Resource allocation error */
	  return (-1);
	}
      }

      v_old = *offs1;

      fseek(rp1->fp, *offs1 * sizeof(vertex), SEEK_SET);

      wres = fwrite(vptr1, sizeof(vertex), 1, rp1->fp);
      if(!wres) {
	free(vptr1);  
	free(offs1);  
	return (-1);
      }

      /* Add the offset value to the tree. (note this is not the file offset, it is the
	 index of the new vertex in the repository file)
      */

      ures = btree_update(bt1, vk1, 33, (char *)offs1, sizeof(long) );

      free(vptr1);  
      free(offs1);

      if(! ures) {
	return (-1);
      }

    }

    else {

      /* The vertex was already present in the database, so modify
	 the existing value to reflect new links
      */

      offs1 = (long *) data1;
    
      sres = fseek(rp1->fp, *offs1 * sizeof(vertex), SEEK_SET);
      if(sres) {
	return (-1);
      }

      rres = fread(&vt1, sizeof(vertex), 1, rp1->fp);
      if(!rres) {
	/* There was a read error */
	return (-1);
      }

      if(v_old > -1) {

	/* We must check that the link does not already exist */
	old_link = 0;
	if(vt1.n_links <= 4) {
	  for (ib = 0; ib < vt1.n_links; ib++) {
	    if(vt1.links[ib] == v_old) {
	      old_link = 1;
	    }
	  }
	}
	else {
	  for (ib = 0; ib < 4; ib++) {
	    if(vt1.links[ib] == v_old) {
	      old_link = 1;
	    }
	  }
	  for(ib = 0; ib < vt1.alloc_extra_links; ib++) {
	    if(vt1.links2[ib] == v_old) {
	      old_link = 1;
	    }
	  }
	}
	
	/* If not, we can go ahead and add the link */
	if(!old_link) {
	  vres = vertex_add_link(&vt1, *offs1);
	}

	if(vres == 1) {
	  /* No previous to link to */

	  /* But not a problem so just go on. */
	}

	else if(vres == -1) {
	  /* Resource allocation error */
	  return (-1);
	}
      }

      v_old = *offs1;  /* Mark this vertex's offset as `previous' */

      /* Now write out the modified structure. Find the current vertex's
	 offset again 
      */

      fseek(rp1->fp, *offs1 * sizeof(vertex), SEEK_SET);

      wres = fwrite(&vt1, sizeof(vertex), 1, rp1->fp);

      if(!wres) {
	/* Resource allocation error */
	return (-1);
      }
      
    }
  }

  return (0);
}


/* Definitions of vertex handling functions. */

vertex *vertex_init(const double xpos0, const double ypos0) {

  vertex *vtx1;

  vtx1 = (vertex *) malloc( sizeof(vertex) );
  if(vtx1 == NULL) {
    return (NULL);
  }

  vtx1->v_status = V_ALLOCATED;
  vtx1->xpos = xpos0;
  vtx1->ypos = ypos0;
  vtx1->v_status |= V_WITH_LOCATION;
  vtx1->n_links = 0;
  vtx1->alloc_extra_links = 0;
  vtx1->links2 = NULL;
  vtx1->direct2 = NULL;
  memset(vtx1->links, 0, 4 * sizeof(long));
  memset(vtx1->direct, 0, 4 * sizeof(double));
  vtx1->v_status |= V_ALLOCATED;

  return vtx1;
}

void vertex_free(vertex *v0) {

  if(v0->alloc_extra_links > 0) {
    free(v0->links2);
    free(v0->direct2);
  }

  free(v0);
}


int vertex_add_link(vertex *vptr0, long offs0) {

  /* Add link between this vertex and the previous stored as
     offset v_old in static buffer
  */

  vertex vt1;
  int res1;

  if(v_old < 0) {
    /* There is nothing to link to: return with a warning (1) */
    return (1);
  }

  /* So try to retrieve the vertex */

  res1 = fseek(rp1->fp, v_old * sizeof(vertex), SEEK_SET);
  if(res1) {
    /* There was an access problem */
    return (-1);
  }

  /* Read vertex */

  res1 = fread(&vt1, sizeof(vertex), 1, rp1->fp);
  if(!res1) {
    /* There was a read access problem */
    return (-1);
  }

  /* Update the links in the two vertices */

  res1 = vertex_links_update(&vt1, vptr0, v_old, offs0);
  if(res1 < 0) {
    /* The links could not be updated - perhaps there was an allocation error
       storing `extra' link data (ie. above the fourth)
    */
    return (-1);
  }

  /* Now return the updated `old' vertex to its file location. */

  fseek(rp1->fp, v_old * sizeof(vertex), SEEK_SET);
  res1 = fwrite(&vt1, sizeof(vertex), 1, rp1->fp);
  if(!res1) {
    /* There was an error writing to the file with random access. */
    return (-1);
  }

  /* If we got this far, tha task is complete. */

  return (0);
}


int vertex_links_update(vertex *vptr0a, vertex *vptr0b, long idx0a, long idx0b) {

  vertex *vptr1, *vptr2;
  long idx1, idx2;
  int n_extra;
  double phi;

  /* loop */

  int i;

  /* can't proceed if there are no values in the co-ordinates */

  if(! ((vptr0a->v_status & V_WITH_LOCATION) && (vptr0b->v_status & V_WITH_LOCATION)) ) {
    return (-1);
  }

  for(i = 0; i < 2; i++ ) {

    if(i  == 0) {
      vptr1 = vptr0a; vptr2 = vptr0b;
      idx1 = idx0a; idx2 = idx0b;
    }

    else {
      vptr1 = vptr0b; vptr2 = vptr0a;
      idx1 = idx0b; idx2 = idx0a;
    }

    /* Does this have four (or more) links already - if so we must create
     `extra' space
    */

    if(vptr1->n_links >= 4) {
      /* Do we already have extra space? */
      if( vptr1->v_status & V_WITH_EXTRA) {
	/* Reallocate */
	n_extra = vptr1->n_links - 3;
	vptr1->links2 = (long *) realloc(vptr1->links2, n_extra * sizeof(long) );
	if(vptr1->links2 == NULL) {
	  /* Allocation error */
	  return (-1);
	}
	vptr1->direct2 = (double *) realloc(vptr1->direct2, n_extra * sizeof(double) );
	if(vptr1->direct2 == NULL) {
	  /* Allocation error */
	  return (-1);
	}
	vptr1->alloc_extra_links = n_extra;
      }

      else {
	/* Just create a single slot */
	n_extra = 1;
	vptr1->links2 = (long *) malloc(sizeof(long) );
	if(vptr1->links2 == NULL) {
	  /* Allocation error */
	  return (-1);
	}
	vptr1->direct2 = (double *) malloc( sizeof(double) );
	if(vptr1->direct2 == NULL) {
	  /* Allocation error */
	  return (-1);
	}
	vptr1->alloc_extra_links = n_extra;
	vptr1->v_status |= V_WITH_EXTRA;
      }

    }

    /* Find the direction */

    phi = atan2(vptr2->ypos - vptr1->ypos, vptr2->xpos - vptr1->xpos);
    if(phi < 0.0) phi += 2 * M_PI;
    if(vptr1->n_links >= 4) {
      vptr1->direct2[n_extra - 1] = phi;
    }
    else {
      vptr1->direct[vptr1->n_links] = phi;
    }

    /* Add the links */

    if(vptr1->n_links >= 4) {
      vptr1->links2[n_extra - 1] = idx2;
    }
    else {
      vptr1->links[vptr1->n_links] = idx2;
    }

    /* Update the number of links */
    vptr1->n_links++;
    
  }

  return (0);
}
  

int vertex_links_remove(vertex *vptr0a, vertex *vptr0b, long idx0a, long idx0b) {

  vertex *vptr1, *vptr2;
  long idx1, idx2;
  int res1;
  int idx_located;
 
  /* loop */

  int i, ix;

  /* can't proceed if there are no values in the co-ordinates */

  if(! ((vptr0a->v_status & V_WITH_LOCATION) && (vptr0b->v_status & V_WITH_LOCATION)) ) {
    return (-1);
  }

  for(i = 0; i < 2; i++ ) {

    if(i  == 0) {
      vptr1 = vptr0a; vptr2 = vptr0b;
      idx1 = idx0a; idx2 = idx0b;
    }

    else {
      vptr1 = vptr0b; vptr2 = vptr0a;
      idx1 = idx0b; idx2 = idx0a;
    }

    /* Find if there is a link with this index */

    idx_located = -1;
    if(vptr1->v_status & V_WITH_EXTRA) {

      for(ix = 0; ix < 4; ix++) {
	if(vptr1->links[ix] == idx2) {
	  idx_located = ix;
	  break;
	}
      }
      
      for(ix = 0; ix < vptr1->alloc_extra_links; ix++) {

	if(vptr1->links2[ix] == idx2) {
	  idx_located = ix + 4;
	  break;
	}
      }

    }

    if(idx_located < 0) {
      if(paric1->verbose_level > 1) {
	fprintf(paric1->logptr, "    WARNING: Link from vertex %ld to vertex %ld absent. Not deleted.\n",
		idx1, idx2);
      }
      continue;
    }

    /* If the link exists, remove it from the vertex. */

    res1 = vertex_delete_link(vptr1, idx_located);

    if(res1) {
      if(paric1->verbose_level > 1) {
	fprintf(paric1->logptr, "    WARNING: Could not delete link %ld to vertex %ld.\n",
		idx1, idx2);
      }
    }
      
  }

  return (0);

}


int vertex_delete_link(vertex *vt0, int idl0) {

  /* Still to implement */

  return (0);
}

repository *get_repository_ptr(void) {

  return (rp1);
}


int close_repository(void) {

  int res1;
  
  res1 = fclose(rp1->fp);

  return (res1);
}


int delete_repository(void) {

  int res1;

  res1 = unlink(rp1->repname);

  return (res1);
}


BTREE *get_keybase_ptr(void) {
  return (bt1);
}


int clear_keybase(void) {

  int res1;

  res1 = btree_free(bt1);

  return (res1);  /* Apparently can only be 0 */
}


int vmap_write_area_edges(struct Map_info *Map0) {

  /* loop */

  int ia;

  /* local */
  char *key1, *data1;
  long *offs1;
  vertex vt1, vt2;
  struct line_pnts *linex;
  FILE *asciimap;

  /* Control */
  int res1, res2;
  int do_write_ascii = 1;

  /* Open a file for dumping ascii lines */

  asciimap = G_fopen_new("dig_ascii", paric1->dest);
  if(asciimap == NULL) {
    if(paric1->verbose_level > 0) {
      fprintf(paric1->logptr, "\n    WARNING: Unable to create ascii file for writing. \n\n");
      do_write_ascii = 0;
    }
  }

  /* Go to the start of the keybase */

  btree_rewind(bt1);

  
  /* Scan the keybase to pick out nodes */

  while(btree_next(bt1, &key1, &data1) != 0) {

    offs1 = (long *)data1;
    res1 = fseek(rp1->fp, *offs1 * sizeof(vertex), SEEK_SET);
    if(res1) {
      /* Critical error. */
      return (-1);
    }
    
    res1 = fread(&vt1, sizeof(vertex), 1, rp1->fp);
    if(!res1) {
      /* Critical read error. */
      return (-1);
    }

    if(vt1.n_links == 2)
      continue;

    ia = 0;
    while(ia < 4) {
      if(ia == vt1.n_links)
	break;

      res1 = fseek(rp1->fp, vt1.links[ia] * sizeof(vertex), SEEK_SET);
      if(res1) {
	/* Critical error. */
	return (-1);
      }
      
      res1 = fread(&vt2, sizeof(vertex), 1, rp1->fp);
      if(!res1) {
	/* Critical read error. */
	return (-1);
      }

      if(vt2.v_status & V_DELETED) {
	ia++;
	continue;
      }

      else {
	linex = Vect_new_line_struct();
	res1 = vbase_extract_line(&vt1, ia, linex, *offs1);

	if(res1 < 0) {
	  /* Encountered a problem */
	}
	else {
	  res2 = Vect_write_line(Map0, AREA, linex);
	  if(do_write_ascii) {
	    vmap_dump_line(linex, asciimap);
	  }
	}

	Vect_destroy_line_struct(linex);
      }

      ia++;
    }

    if(vt1.v_status & V_WITH_EXTRA) {
      for(ia = 0; ia < vt1.n_links - 4; ia++) {
	res1 = fseek(rp1->fp, vt1.links2[ia] * sizeof(vertex), SEEK_SET);
	if(res1) {
	  /* Critical error. */
	  return (-1);
	}
      
	res1 = fread(&vt2, sizeof(vertex), 1, rp1->fp);
	if(!res1) {
	  /* Critical read error. */
	  return (-1);
	}

	if(vt2.v_status & V_DELETED) {
	  ia++;
	  continue;
	}

	else {
	  linex = Vect_new_line_struct();
	  res1 = vbase_extract_line(&vt1, ia, linex, *offs1);

	  if(res1 < 0) {
	    /* Encountered a problem */
	  }
	  else {
	    res2 = Vect_write_line(Map0, AREA, linex);
	    if(do_write_ascii) {
	      vmap_dump_line(linex, asciimap);
	    }
	  }

	  Vect_destroy_line_struct(linex);
	}

	
      }
    }

    /* Mark this node as `deleted' and return to its slot */

    vt1.v_status |= V_DELETED;

    fseek(rp1->fp, *offs1 * sizeof(vertex), SEEK_SET);
    res1 = fwrite(&vt1, sizeof(vertex), 1, rp1->fp);
    if(!res1) {
      /* Critical read error. */
      return (-1);
    }

  }

  /* Now do the second scan to pick out simple islands */

  btree_rewind(bt1);

  while(btree_next(bt1, &key1, &data1) != 0) {

    offs1 = (long *)data1;
    res1 = fseek(rp1->fp, *offs1 * sizeof(vertex), SEEK_SET);
    if(res1) {
      /* Critical error. */
      return (-1);
    }
    
    res1 = fread(&vt1, sizeof(vertex), 1, rp1->fp);
    if(!res1) {
      /* Critical read error. */
      return (-1);
    }

    if(vt1.n_links == 2) {

      if(vt1.v_status & V_DELETED)
	continue;

      linex = Vect_new_line_struct();
      res1 = vbase_extract_simple_island(&vt1, linex, *offs1);

      if(res1 < 0) {
	/* Encountered a problem */
      }
      else {
	res2 = Vect_write_line(Map0, AREA, linex);
	if(do_write_ascii) {
	  vmap_dump_line(linex, asciimap);
	}
      }
      
      Vect_destroy_line_struct(linex);
    }
  }

  if(do_write_ascii) {
    fclose(asciimap);
  }

  return (0);
    
}



int vbase_extract_line(vertex *vptr0, const int idx0, struct line_pnts *lpt0,
		       long offs0) {

  vertex vert1, vert2;
  int v_cnt = 0;
  int res1;
  long offs1, offs2, offs3;

  /* Get the position of the parent point */

  Vect_append_point(lpt0, vptr0->xpos, vptr0->ypos);
  

  /* Move the first two vertices into position: remember that there are
     at least three vertices
  */

  memcpy(&vert1, vptr0, sizeof(vertex) );
  offs1 = offs0;
  
  if(idx0 > 3) {
    offs2 = vert1.links2[idx0 - 4];
  }
  else {
    offs2 = vert1.links[idx0];
  }

  while(++v_cnt <= MAX_VERTICES_PER_LINE) {

    res1 = fseek(rp1->fp, offs2 * sizeof(vertex), SEEK_SET);

    if(res1) {
      /* Critical error. */
      return (-1);
    }

    res1 = fread(&vert2, sizeof(vertex), 1, rp1->fp);
    if(!res1) {
      /* Critical read error. */
      return (-1);
    }    

    Vect_append_point(lpt0, vert2.xpos, vert2.ypos);

    if(vert2.n_links > 2)
      break;

    fseek(rp1->fp, offs2 * sizeof(vertex), SEEK_SET);

    vert2.v_status |= V_DELETED;
    res1 = fwrite(&vert2, sizeof(vertex), 1, rp1->fp);
    if(!res1) {
      /* Critical error. */
      return (-1);
    }
  
    if(vert2.links[0] == offs1)
      offs3 = vert2.links[1];
    else
      offs3 = vert2.links[0];

    offs1 = offs2;
    offs2 = offs3;

    memcpy(&vert1, &vert2, sizeof(vertex));
  }

  if(lpt0->n_points == 2) {
    shp_add_third_point(lpt0); /* Build sometimes fails on a small one-linker */
  }

  if(v_cnt > MAX_VERTICES_PER_LINE) {
    
    /* probably there was a runaway line, or there is a bug */
    fprintf(paric1->logptr, "  WARNING: There was a runaway line (> 50,000 vertices) in open line extraction.\n");
    fprintf(paric1->logptr, "  If this is a possible scenario, consider increasing the vertex limit.\n");
    fprintf(paric1->logptr, "  Otherwise there is an error in the imported file, or else a bug.\n\n");

    return (-1);
  }

  else return (0);

}


int vbase_extract_simple_island(vertex *vptr0, struct line_pnts *lpt0, long offs0) {

  vertex vert1, vert2;
  int v_cnt = 0;
  int res1;
  long offs1, offs2, offs3;

  /* Get the position of the parent point */

  Vect_append_point(lpt0, vptr0->xpos, vptr0->ypos);

  res1 = fseek(rp1->fp, offs0 * sizeof(vertex), SEEK_SET);

  if(res1) {
    /* Critical error. */
    return (-1);
  }

  vptr0->v_status |= V_DELETED;
  res1 = fwrite(vptr0, sizeof(vertex), 1, rp1->fp);
  if(!res1) {
    /* Critical error. */
    return (-1);
  }
  
  /* Move the first two vertices into position: remember that there are
     at least four vertices
  */

  memcpy(&vert1, vptr0, sizeof(vertex) );
  offs1 = offs0;

  offs2 = vert1.links[0]; /* Just choose a direction and keep going */

  while(++v_cnt <= MAX_VERTICES_PER_LINE) {

    res1 = fseek(rp1->fp, offs2 * sizeof(vertex), SEEK_SET);

    if(res1) {
      /* Critical error. */
      return (-1);
    }

    res1 = fread(&vert2, sizeof(vertex), 1, rp1->fp);
    if(!res1) {
      /* Critical read error. */
      return (-1);
    }    

    Vect_append_point(lpt0, vert2.xpos, vert2.ypos);

    if(offs2 == offs0)  /* ie. back to the start */
      break;

    res1 = fseek(rp1->fp, offs2 * sizeof(vertex), SEEK_SET);

    if(res1) {
      /* Critical error. */
      return (-1);
    }

    vert2.v_status |= V_DELETED;
    res1 = fwrite(&vert2, sizeof(vertex), 1, rp1->fp);
    if(!res1) {
      /* Critical error. */
      return (-1);
    }

    if(vert2.links[0] == offs1)
      offs3 = vert2.links[1];
    else
      offs3 = vert2.links[0];

    offs1 = offs2;
    offs2 = offs3;

    memcpy(&vert1, &vert2, sizeof(vertex));
  }

  if(v_cnt > MAX_VERTICES_PER_LINE) {
    
    /* probably there was a runaway line, or there is a bug */
    fprintf(paric1->logptr, "  WARNING: There was a runaway line (> 50,000 vertices) in closed line extraction.\n");
    fprintf(paric1->logptr, "  If this is a possible scenario, consider increasing the vertex limit.\n");
    fprintf(paric1->logptr, "  Otherwise there is an error in the imported file, or else a bug.\n\n");

    return (-1);
  }

  else return (0);
}


void vmap_dump_line(struct line_pnts *lp0, FILE *fp0) {

  int ia;

  fprintf(fp0, "A  %d\n", lp0->n_points);

  for(ia = 0; ia < lp0->n_points; ia++) {
    fprintf(fp0, "%16.6f  %16.6f\n", lp0->y[ia], lp0->x[ia]);
  }  
}

/* Definitions of static routines */


static int vkey_cmp(void *key1, void *key2) {

  /* Routine to provide comparison of two vertex keys */

  /* Just compare lexicographically */

  return strncmp( (char *)key1, (char *)key2, 32 );

}



static char *get_vkey( double xa, double ya, double sr, int decs, double efalse,
		       double nfalse ) {
  /* local */

  double xtmp, ytmp;
  char xbuf[128], ybuf[128];
  char *retbuf;
  char *indx_ptr;
  int idigits;

  xtmp = ((long long)( (xa - efalse) / sr )) * sr;
  ytmp = ((long long)( (ya - nfalse) / sr )) * sr;

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


static void set_region_params(void) {

  double east1, north1, west1, south1, ew_extent, ns_extent;
  double dec1, dec2;
  int whole_part, whole_part1, whole_part2;
  int res1;

  /* What are the boundaries? */

  res1 = get_vmap_bounds(&east1, &north1, &west1, &south1);
  ew_extent = east1 - west1;
  ns_extent = north1 - south1;
  
  /* Expand the keyed region to +10% on all sides of the input region. */

  east1 += (EXPANSION_FRACT / 100.0) * ew_extent;
  north1 += (EXPANSION_FRACT / 100.0) * ns_extent;
  west1 -= (EXPANSION_FRACT / 100.0) * ew_extent;
  south1 -= (EXPANSION_FRACT / 100.0) * ns_extent;
  ew_extent = east1 - west1;
  ns_extent = north1 - south1;

  /* Set the new west and south values to the false easting and northing resp. */

  false_easting = west1;
  false_northing = south1;

  /* Set n_decs */

  /* How many places do we need for the non-decimal part ? */
  dec1 = log10(ew_extent);
  dec2 = log10(ns_extent);
  whole_part1 = (int) dec1 + 2;
  whole_part2 = (int) dec2 + 2;
  whole_part = whole_part1;
  if(whole_part2 > whole_part1) whole_part = whole_part2;
  n_decs = 16 - whole_part;
}



static void set_parameter_import_ctrl(param_import_ctrl *param0) {

  paric1 = param0;
}



/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

