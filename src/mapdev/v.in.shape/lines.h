/******************************************************************************
 * lines.h [v.in.shape]
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

#ifndef IMPORT_LINES_H_
#define IMPORT_LINES_H_

#include "gis.h"
#include "Vect.h"
#include "polygon.h"
#include "import.h"
#include "btree.h"

#define V_ALLOCATED 1
#define V_INITIALISED 2
#define V_WITH_EXTRA 4
#define V_WITH_LOCATION 8
#define V_AS_NODE 16
#define V_DELETED 32

typedef struct vkey_ {
  double x, y;
} vkey ;

typedef struct vertex_ {
  int v_status;
  double xpos, ypos;
  int n_links;
  int alloc_extra_links;
  long links[4];
  long *links2;
  double direct[4];
  double *direct2;
} vertex;

typedef struct repository_ {

  char repname[512];

  FILE *fp;
  int open;
} repository;


/* Prototypes */

int vertices_vload(polygon_ctrl *, param_import_ctrl *);
int repository_init(char *);
int repository_close(void);
int vbase_add_line(struct line_pnts *);
vertex *vertex_init(const double, const double);
void vertex_free(vertex *);
int vertex_add_link(vertex *, long);
int vertex_links_update(vertex *, vertex *, long, long);
repository *get_repository_ptr(void);
int close_repository(void);
int delete_repository(void);
BTREE *get_keybase_ptr(void);
int clear_keybase(void);
int vmap_write_area_edges(struct Map_info *);
int vbase_extract_line(vertex *, const int, struct line_pnts *, long);
int vbase_extract_simple_island(vertex *, struct line_pnts *, long);
void vmap_dump_line(struct line_pnts *, FILE *);

#endif  /* IMPORT_LINES_H_  */

