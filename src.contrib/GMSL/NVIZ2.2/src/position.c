/*	Alex Shevlakov sixote@yahoo.com 02/2000
*	function added to handle postgres queries
*/
#include <stdlib.h>
#include "interface.h"

int Ninit_view_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
     
{
  GS_init_view();
  return (TCL_OK);
}

/* TODO: Need Nset_to_cmd or use viewdir*/

int Nget_to_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  float to[3];
  char x[32], y[32], z[32];
  char  *list[4]; 
  
  GS_get_to(to);
  sprintf (x, "%f", to[0]);
  sprintf (y, "%f", to[1]);
  sprintf (z, "%f", to[2]);
  
  list[0] = x;
  list[1] = y;
  list[2] = z;
  list[3] = NULL;
  
  
  interp->result = Tcl_Merge (3, list);
  interp->freeProc = (Tcl_FreeProc *)free;
  return (TCL_OK);
}

int 
Nget_from_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  float from[3];
  char x[32], y[32], z[32];
  char  *list[4]; 
  
  GS_get_from(from);
  sprintf (x, "%f", from[0]);
  sprintf (y, "%f", from[1]);
  sprintf (z, "%f", from[2]);
  
  list[0] = x;
  list[1] = y;
  list[2] = z;
  list[3] = NULL;
  
  
  interp->result = Tcl_Merge (3, list);
  interp->freeProc = (Tcl_FreeProc *)free;
  
  
  return (TCL_OK);
}

int 
Nlook_here_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  if (argc != 3)
    return (TCL_ERROR);
  GS_look_here(atoi(argv[1]), atoi(argv[2]));
  
  return (TCL_OK);
}

int 
Nhas_focus_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  float realto[3];

  if(GS_get_focus(realto))
      Tcl_SetResult (interp, "1", TCL_VOLATILE);
  else
      Tcl_SetResult (interp, "0", TCL_VOLATILE);

  return (TCL_OK);

}

int 
Nset_focus_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  float realto[3];

  if(argc == 4){
      realto[0]=atof(argv[1]);
      realto[1]=atof(argv[2]);
      realto[2]=atof(argv[3]);
      GS_set_focus(realto);
  }

  return (TCL_OK);

}

int 
Nset_no_focus_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  GS_set_nofocus();
  return (TCL_OK);
}

int 
Nset_focus_map_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  int id;
  
  if (!GS_num_surfs()) {
    GS_set_nofocus();
    return (TCL_OK);
  }

  if (argc == 1) {
    int *surf_list, num_surfs;
    
    surf_list=GS_get_surf_list(&num_surfs);
    id = surf_list[0];
    free(surf_list);
  } else
    id = atoi(argv[1]);

  GS_set_focus_center_map(id);
  return (TCL_OK);
}


int Nmove_to_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  float	ftmp;
  int i;

  if (argc != 4)
    return (TCL_ERROR);
  for(i=1;i<argc;i++) {
    ftmp = atof(*(++argv));
    GS_moveto(&ftmp);
  }
  
  return (TCL_OK);
}

int Nset_fov_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  if (argc != 2)
    return (TCL_ERROR);
  GS_set_fov(atoi(argv[1]));
  
  return (TCL_OK);
}

int 
Nget_region_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  float n, s, e, w;
  char  *list[5]; 
  char north[32], east[32], south[32], west[32];
  
  GS_get_region (&n, &s, &e, &w);
  sprintf (north, "%f", n);
  sprintf (east, "%f", e);
  sprintf (south, "%f", s);
  sprintf (west, "%f", w);
  
  list[0] = north;
  list[1] = east;
  list[2] = south;
  list[3] = west;
  list[4] = NULL;
  
  interp->result = Tcl_Merge (4, list);
  interp->freeProc = (Tcl_FreeProc *)free;
  
  return (TCL_OK);
}

int 
Nget_point_on_surf_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  float x, y, z;
  int sx, sy,id;
  char cx[32], cy[32], cz[32], idname[128];
  char *list[5];
  
  if (argc != 3)
    return (TCL_ERROR);
  
  sx = atoi(argv[1]);
  sy = atoi(argv[2]);

#ifdef DEBUG_MSG
fprintf(stderr,"x= %d  :  y= %d\n", sx, sy);
#endif
  
  if (!GS_get_selected_point_on_surface(sx, sy, &id, &x, &y, &z))
    {
      list[0] = NULL;
      interp->result = Tcl_Merge (0, list);
      interp->freeProc = (Tcl_FreeProc *)free;
      
      return (TCL_OK);
    }
  
  sprintf (cx, "%f", x);
  sprintf (cy, "%f", y);
  sprintf (cz, "%f", z);
  sprintf (idname, "Nsurf%d", id);
  
  list[0] = cx;
  list[1] = cy;
  list[2] = cz;
  list[3] = idname;
  list[4] = NULL;
  
  interp->result = Tcl_Merge (4, list);
  interp->freeProc = (Tcl_FreeProc *)free;
  
  return (TCL_OK);
    
}
int
Nget_point_on_surf_pg_grass (data, interp, argc, argv)
     Nv_data *data;
     Tcl_Interp *interp;                 /* Current interpreter. */
     int argc;                           /* Number of arguments. */
     char **argv;                        /* Argument strings. */
{
  float x, y, z;
  int sx, sy,id;
  char cx[32], cy[32], cz[32], idname[128];
  char *list[6];
  
  char *name, *keytable, *col;
  
  if (argc != 6)
    return (TCL_ERROR);
  
  sx = atoi(argv[1]);
  sy = atoi(argv[2]);
  name = argv[3];
  keytable = argv[4];
  col=argv[5];

#ifdef DEBUG_MSG
fprintf(stderr,"x= %d  :  y= %d\n", sx, sy);
#endif
  
  if (!GS_get_selected_point_on_surface(sx, sy, &id, &x, &y, &z))
    {
      list[0] = NULL;
      interp->result = Tcl_Merge (0, list);
      interp->freeProc = (Tcl_FreeProc *)free;
      
      return (TCL_OK);
    }
  
  sprintf (cx, "%f", x);
  sprintf (cy, "%f", y);
  sprintf (cz, "%f", z);
  sprintf (idname, "Nsurf%d", id);
  
  list[0] = cx;
  list[1] = cy;
  list[2] = cz;
  list[3] = idname;
  list[4] = (char*) query_postgr(name,keytable,col,x,y);
  list[5] = NULL;
  
  interp->result = Tcl_Merge (5, list);
  interp->freeProc = (Tcl_FreeProc *)free;
  
  return (TCL_OK);
    
}
int
Nget_point_on_surf_pg_site (data, interp, argc, argv)
     Nv_data *data;
     Tcl_Interp *interp;                 /* Current interpreter. */
     int argc;                           /* Number of arguments. */
     char **argv;                        /* Argument strings. */
{
  float x, y, z;
  int sx, sy,id;
  char cx[32], cy[32], cz[32], idname[128];
  char *list[6];
  
  char *name, *xcol, *ycol;
  int dist;
  
  if (argc != 7)
    return (TCL_ERROR);
  
  sx = atoi(argv[1]);
  sy = atoi(argv[2]);
  name = argv[3];
  xcol = argv[4];
  ycol=argv[5];
  dist = atoi(argv[6]);

#ifdef DEBUG_MSG
fprintf(stderr,"x= %d  :  y= %d\n", sx, sy);
#endif
  
  if (!GS_get_selected_point_on_surface(sx, sy, &id, &x, &y, &z))
    {
      list[0] = NULL;
      interp->result = Tcl_Merge (0, list);
      interp->freeProc = (Tcl_FreeProc *)free;
      
      return (TCL_OK);
    }
  
  sprintf (cx, "%f", x);
  sprintf (cy, "%f", y);
  sprintf (cz, "%f", z);
  sprintf (idname, "Nsurf%d", id);
  
  list[0] = cx;
  list[1] = cy;
  list[2] = cz;
  list[3] = idname;
  list[4] = (char*) query_pg_site(name,xcol,ycol,dist,x,y);
  list[5] = NULL;
  
  interp->result = Tcl_Merge (5, list);
  interp->freeProc = (Tcl_FreeProc *)free;
  
  return (TCL_OK);
    
}
int 
Nget_dist_along_surf_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  float x, y, px, py, d;
  int id, exag;
  char dist[128];
  
  if (argc != 7)
    return (TCL_ERROR);
  
  id = get_idnum(argv[1]);
  x = atof(argv[2]);
  y = atof(argv[3]);
  px = atof(argv[4]);
  py = atof(argv[5]);
  exag = atoi(argv[6]);
  
  if(!GS_get_distance_alongsurf(id, x, y, px, py, &d, exag))
    return (TCL_ERROR);
  sprintf (dist, "%f", d);
  Tcl_SetResult (interp, dist, TCL_VOLATILE);
  
  return (TCL_OK);
  
}

/*
#define DO_TEST
*/

int 
Nget_cat_at_xy_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  float x, y; 
  int id, att;
  char catstr[1024];
  
  if (argc != 5)
    return (TCL_ERROR);
  
  id = get_idnum(argv[1]);
  att = att_atoi(argv[2]);
  x = atof(argv[3]);
  y = atof(argv[4]);

#ifdef DO_TEST
GS_set_draw(GSD_FRONT);
GS_ready_draw();
GS_draw_flowline_at_xy(id, x, y);
GS_done_draw();
#endif
  
  if(0 > GS_get_cat_at_xy(id, att, catstr, x, y)){
        Tcl_SetResult (interp, "no category", TCL_VOLATILE);
	return (TCL_OK);
  }
  Tcl_SetResult (interp, catstr, TCL_VOLATILE);
  
  return (TCL_OK);
  
}


int 
Nget_val_at_xy_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  float x, y; 
  int id, att;
  char valstr[1024];
  
  if (argc != 5)
    return (TCL_ERROR);
  
  id = get_idnum(argv[1]);
  att = att_atoi(argv[2]);
  x = atof(argv[3]);
  y = atof(argv[4]);
  
  GS_get_val_at_xy(id, att, valstr, x, y);

  Tcl_SetResult (interp, valstr, TCL_VOLATILE);
  
  return (TCL_OK);
  
}

int 
Nget_focus_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  float realto[3];
  char x[32], y[32], z[32];
  char  *list[4]; 
  
  if(GS_get_focus(realto)){
      sprintf (x, "%f", realto[0]);
      sprintf (y, "%f", realto[1]);
      sprintf (z, "%f", realto[2]);
      
      list[0] = x;
      list[1] = y;
      list[2] = z;
      list[3] = NULL;
      
      Tcl_SetResult (interp, Tcl_Merge (3, list), TCL_VOLATILE);
  }
  else{
      Tcl_SetResult (interp, "0", TCL_VOLATILE);
  }
  
  return (TCL_OK);
}


int 
Nget_longdim_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  float dim;
  char buf[128];
  
  GS_get_longdim(&dim);
  sprintf (buf, "%f", dim);
  Tcl_SetResult (interp, buf, TCL_VOLATILE);
  
  return (TCL_OK);
}

int 
Nget_zrange_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  float min, max;
  char *list[3], cmin[32], cmax[32];
  int doexag;
  
  if (argc > 2)
    {
      if (!strcmp (argv[2], "doexag"))
	GS_get_zrange_nz(&min, &max);
      else if (!strcmp (argv[2], "nz"))
	GS_get_zrange_nz(&min, &max);
      else
	return (TCL_ERROR);
    }
  else
    GS_get_zrange_nz(&min, &max);
  
  sprintf (cmin, "%f", min);
  sprintf (cmax, "%f", max);
  list[0] = cmin;
  list[1] = cmax;
  list[2] = NULL;
  
  
  interp->result = Tcl_Merge (2, list);
  interp->freeProc = (Tcl_FreeProc *)free;
  return (TCL_OK);
}

int 
Nget_xyrange_cmd (Nv_data *data, Tcl_Interp *interp, int argc, char **argv)
{
  char temp[40];
  
  if (argc != 1) {
    interp->result="Usage: Nget_xyrange";
    return (TCL_ERROR);
  }

  sprintf(temp,"%f",data->XYrange);
  Tcl_SetResult(interp, temp, TCL_VOLATILE);
  return (TCL_OK);
}

int 
Nget_zextents_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  int id;
  float min, max, mid;
  char *list[4], cmin[32], cmax[32], cmid[32];

  if (argc != 2)
    return (TCL_ERROR);
  id = get_idnum (argv[1]);
  
  GS_get_zextents(id, &min, &max, &mid);
  
  sprintf (cmin, "%f", min);
  sprintf (cmax, "%f", max);
  sprintf (cmid, "%f", mid);
  list[0] = cmin;
  list[1] = cmax;
  list[2] = cmid;
  list[3] = NULL;
  
  interp->result = Tcl_Merge (3, list);
  interp->freeProc = (Tcl_FreeProc *)free;
  
  return (TCL_OK);
}

int 
Nget_exag_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  char buf[128];
  float exag;
  
  exag = GS_global_exag();

  sprintf (buf, "%f",exag);
  Tcl_SetResult (interp, buf, TCL_VOLATILE);
  
  return (TCL_OK);
}

int 
Nset_exag_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  if (argc != 2)
    return (TCL_ERROR);
  GS_set_global_exag(atof(argv[1]));
  return (TCL_OK);
}

/*
 * Nsave_3dview_cmd --
 *
 *	Syntax: Nsave_3dview file_name
 *	Saves the current orientation of Nviz camera position.
 *	Note that GRASS requires a surface ID to reference
 * 	the view to.  By default we choose the first surface
 *	available or 0 if no surfaces have been loaded.
 */
int 
Nsave_3dview_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  char **list_space;
  int list_count, first_surf;
  
  /* Check for correct number of arguments */
  if (argc != 2){
    Tcl_SetResult(interp, "Usage: Nsave_3dview file_name", TCL_VOLATILE);
    return (TCL_ERROR);
  }

  /* Try and find a surface to use as a reference */
  if ((Tcl_Eval(interp, "Nget_surf_list") != TCL_OK) ||
      (Tcl_SplitList(interp, interp->result, &list_count, &list_space) != TCL_OK)) {
    Tcl_SetResult(interp, "Internal Error: Parse failure in Nsave_3dview_cmd",
		  TCL_VOLATILE);
    return (TCL_ERROR);
  } else {
    if (!list_count)
      first_surf=0;
    else
      first_surf=(int)atoi(list_space[0]);
    
    free(list_space);
  }

  /* Finally make the GSF library call */
  GS_save_3dview(argv[1], first_surf);
  
  return (TCL_OK);
}

/*
 * Nload_3dview_cmd --
 *
 *	Syntax: Nload_3dview file_name
 *      Loads a saved view.
 *	Note that GRASS requires a surface ID to reference
 * 	the view to.  By default we choose the first surface
 *	available or 0 if no surfaces have been loaded.
 */
int 
Nload_3dview_cmd (
    Nv_data *data,
    Tcl_Interp *interp,                 /* Current interpreter. */
    int argc,                           /* Number of arguments. */
    char **argv                        /* Argument strings. */
)
{
  char **list_space;
  int list_count, first_surf;
  
  /* Check for correct number of arguments */
  if (argc != 2){
    Tcl_SetResult(interp, "Usage: Nload_3dview file_name", TCL_VOLATILE);
    return (TCL_ERROR);
  }

  /* Try and find a surface to use as a reference */
  if ((Tcl_Eval(interp, "Nget_surf_list") != TCL_OK) ||
      (Tcl_SplitList(interp, interp->result, &list_count, &list_space) != TCL_OK)) {
    Tcl_SetResult(interp, "Internal Error: Parse failure in Nsave_3dview_cmd",
		  TCL_VOLATILE);
    return (TCL_ERROR);
  } else {
    if (!list_count)
      first_surf=0;
    else
      first_surf=(int)atoi(list_space[0]);
    
    free(list_space);
  }

  /* Finally make the GSF library call */
  GS_load_3dview(argv[1], first_surf);
  
  return (TCL_OK);
}
