/* 
 * $Id$ 
 */

#include <stdio.h>
#include <stdlib.h>
#include "tk.h"
#include "interface.h"
#include "gis.h"
#include "coldefs.h"
#include "bitmap.h"

char startup_script[] =
"toplevel .wait_ok\n\
label .wait_ok.wait -text \"Please wait...\" -fg red -bg black\n\
pack .wait_ok.wait -ipadx 20 -ipady 20 -expand 1 -fill both\n\
wm geometry .wait_ok \"+800+50\"\n\
wm geometry . \"+100+100\"\n\
update\n\
grab .wait_ok.wait";

int 
parse_command (
    Nv_data *data,
    Tcl_Interp *interp,			/* Current interpreter. */
    int argc,
    char **argv
)
{
  struct Option *elev, *colr, *tricolr, *vct, *site, *view;
  struct Option *panel_path, *script;
  struct Flag *no_args, *script_kill, *demo;
  char *arglist[3], *autoload;
  int i, c, aload=1;

  /*
   * Flags and Options:
   * -q : quickstart, starts nvwish without querying for the usual maps
   *
   * path : panel path, adds the given directory to the path to
   *        search for panels
   *
   * script : script file, after starting nviz immediately plays the
   *          named script
   *
   * -k : script kill, if this flag is set, then Nviz will exit after completing
   *      a script started from the command line
   *
   * -x : demo mode, the usual "please wait" messages are nuked.
   */	
  
  elev = G_define_option();
  elev->key                    = "elevation";
  elev->type                   = TYPE_STRING;
  elev->required               = NO;
  elev->multiple               = YES;
  elev->gisprompt              = "old,cell,Raster";
  elev->description            = "Raster file(s) for Elevation";
  
  vct = G_define_option();
  vct->key                    = "vector";
  vct->type                   = TYPE_STRING;
  vct->required               = NO;
  vct->multiple               = YES;
  vct->gisprompt              = "old,dig,Vector";
  vct->description            = "Vector overlay file(s)";
  
  site = G_define_option();
  site->key                    = "sites";
  site->type                   = TYPE_STRING;
  site->required               = NO;
  site->multiple               = YES;
  site->gisprompt              = "old,site_lists,Sites";
  site->description            = "Sites overlay file(s)";

  no_args = G_define_flag();
  no_args->key		       = 'q';
  no_args->description         = "No args option";

  script_kill = G_define_flag();
  script_kill->key	       = 'k';
  script_kill->description     = "Script kill option";

  demo = G_define_flag();
  demo->key	               = 'x';
  demo->description            = "Demo mode";

  panel_path = G_define_option();
  panel_path->key              = "path";
  panel_path->type             = TYPE_STRING;
  panel_path->required         = NO;
  panel_path->description      = "Alternative panel path";

  script = G_define_option();
  script->key              = "script";
  script->type             = TYPE_STRING;
  script->required         = NO;
  script->description      = "Startup script file";

  if (G_parser (argc, argv))
    exit (-1);
  
  {
    float defs[MAX_ATTS];
    
    defs[ATT_TOPO] = 0;
    defs[ATT_COLOR] = DEFAULT_SURF_COLOR;
    defs[ATT_MASK] = 0;
    defs[ATT_TRANSP] = 0;
    defs[ATT_SHINE] = 60;
    defs[ATT_EMIT] = 0;
    GS_set_att_defaults(defs, defs);
  }	

  /* Put in the "please wait..." message unless we are in demo mode */
  if ((strstr(argv[0],"nviz") != NULL) &&
      (!demo->answer)) {
    if (Tcl_Eval(interp, startup_script) != TCL_OK) {
      fprintf(stderr,"ERROR: %s\n", interp->result);
      exit(-1);
    }
  }

    fprintf (stderr, "\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Version: GRASS5.0 beta, last update: July 1999\n");
    fprintf (stderr, "updated to OPENGL, LINUX, Tcl/Tk 8.0\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Authors: Bill Brown, Terry Baker, Mark Astley, David Gerdes\n");
    fprintf (stderr, "\tmodifications: Jaro Hofierka\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Please cite one or more of the following references in publications\n");
    fprintf (stderr, "where the results of this program were used:\n");
    fprintf (stderr, "Brown, W.M., Astley, M., Baker, T., Mitasova, H. (1995).\n");
    fprintf (stderr, "GRASS as an Integrated GIS and Visualization System for\n");
    fprintf (stderr, "Spatio-Temporal Modeling, Proceedings of Auto Carto 12, Charlotte, N.C.\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Mitasova, H., W.M. Brown, J. Hofierka, 1994, Multidimensional\n");
    fprintf (stderr, "dynamic cartography. Kartograficke listy, 2, p. 37-50.\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Mitas L., Brown W. M., Mitasova H., 1997, Role of dynamic\n");
    fprintf (stderr, "cartography in simulations of landscape processes based on multi-variate\n");
    fprintf (stderr, "fields. Computers and Geosciences, Vol. 23, No. 4, pp. 437-446\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "http://www2.gis.uiuc.edu:2280/modviz/viz/nviz.html\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "The papers are available at\n");
    fprintf (stderr, "http://www2.gis.uiuc.edu:2280/modviz/\n");
  

  /* Look for quickstart flag */
  if (no_args->answer) 
    elev->answers=vct->answers=site->answers=NULL;

  /* Look for scriptkill flag */
  if (script_kill->answer) {
    if (Tcl_VarEval(interp,"set NvizScriptKill 1 ", NULL) != TCL_OK) {
      fprintf(stderr,"ERROR: %s\n", interp->result);
      exit(-1);
    }
  }
  
  /* See if an alternative panel path is specified */
  if (panel_path->answer) {
    /* If so then set the variable NvizAltPath to the alternative path
     */
    if (Tcl_VarEval(interp,"set NvizAltPath ", panel_path->answer,
		    NULL) != TCL_OK) {
      fprintf(stderr,"ERROR: %s\n", interp->result);
      exit(-1);
    }
  }
  
  /* See if a script file was specified */
  if (script->answer) {
    /* If so then set the variable NvizPlayScript to the file */
    if (Tcl_VarEval(interp,"set NvizPlayScript ", script->answer,
		    NULL) != TCL_OK) {
      fprintf(stderr,"ERROR: %s\n", interp->result);
      exit(-1);
    }
  }

#ifdef XSCRIPT
  AUTO_FILE = aut->answer;
  /* either file name or NULL */
  
  Write_script = swrit->answer;
#endif

  /* Consult the user's .grassrc file to see if we should
   * automatically set the colormap of loaded surfaces to be
   * the same as the raster used for topography.  The appropriate
   * resource is:
   *     Nviz_AutoSurfColors
   * If this resource isn't specified, it defaults to true.
   */
  autoload=G__getenv("Nviz_AutoSurfColors");
  if ((autoload != NULL) && (!strcmp(autoload,"false")))
    aload=0;
  
  /* Parse answeres from user */
  if(elev->answers){
    char tmp[30];
    
    for (i = 0 ; elev->answers[i] ; i++){
      arglist[1] = "surf";
      arglist[2] = elev->answers[i];
      Nnew_map_obj_cmd (data, interp, 3, arglist);

      /* See if we should autoload the color file */
      if (aload) {
	strncpy(tmp, interp->result, 29);
	if (Tcl_VarEval(interp, tmp, " set_att color ",
			elev->answers[i], NULL) != TCL_OK) {
	  fprintf(stderr, "ERROR: %s\n", interp->result);
	  exit(-1);
	}
      }
    }

    if(i > 1)
      set_default_wirecolors(data, i);
  }

  if(vct->answers){
    for (i = 0 ; vct->answers[i] ; i++){
      arglist[1] = "vect";
      arglist[2] = vct->answers[i];
      Nnew_map_obj_cmd (data, interp, 3, arglist);
    }
  }

  if(site->answers){
    for (i = 0 ; site->answers[i] ; i++){
      arglist[1] = "site";
      arglist[2] = site->answers[i];
      Nnew_map_obj_cmd (data, interp, 3, arglist);
    }
  }
  
}


/*
   Ngetargs: gets command line args from tcl. Tcl stores argv[0] by
   itself and the rest of the args as a single string so Ngetargs goes 
   through some string manipulation to put all the args back into a single array
   so that G_parser can deal with them without getting sick. 
   */

int 
Ngetargs (
    Tcl_Interp *interp,			/* Current interpreter. */
    char ***args
)
{
  int i, n;
  char *tmp, *tmp2, *argv0;
  int argc;
  
  argv0 = Tcl_GetVar (interp, "argv0", TCL_LEAVE_ERR_MSG);
  tmp = Tcl_GetVar (interp, "argv", TCL_LEAVE_ERR_MSG);
  tmp2  = (char *) malloc ((strlen(argv0) + strlen(tmp) +2)*(sizeof (char)));
  sprintf (tmp2, "%s %s", argv0, tmp);
  
  if (TCL_ERROR == Tcl_SplitList (interp, tmp2, &argc, args))
    exit(-1);
    
  return (argc);
}

int make_red_yellow_ramp (int *ramp, int num, int minval, int maxval)
{
  int g, i, incr;
  
  incr = (maxval - minval)/(num-1);
  for(i=0; i<num; i++){
    g = minval + incr * i;
    RGB_TO_INT(maxval,g,0,ramp[i]);
  }
  
  return 0;
}


/* Sorts surfaces by mid elevation, lowest to highest.
   Puts ordered id numbers in id_sort, leaving surfs unchanged.
   Puts ordered indices of surfaces from id_orig in indices.
   */
int sort_surfs_mid (int *id_sort, int *indices, int num)
{
  int i, j;
  float midvals[MAX_SURFS];
  float tmp, max, tmin, tmax, tmid;
  int *surf_list;

  surf_list=GS_get_surf_list(&i);
  for(i=0; i < num; i++){
    GS_get_zextents(surf_list[i], &tmin, &tmax, &tmid);
    if(i ==0) max = tmax;
    else max = max < tmax? tmax: max;
    midvals[i] = tmid;
  }

  for(i=0; i < num; i++){
    tmp = midvals[0];
    indices[i] = 0;
    for(j=0; j < num; j++){
      if(midvals[j] < tmp){
	tmp = midvals[j];
	indices[i] = j;
      }
    }
    midvals[indices[i]] = max+1;
    id_sort[i] = surf_list[indices[i]];
  }
  
}

int set_default_wirecolors (Nv_data *dc, int surfs)
{

#ifdef DO_GREYSCALE
  int *surf_list;
  int i, color, greyincr, greyval;
  
  greyincr = 200/(surfs+1); /* just use upper values */
  
  surf_list=GS_get_surf_list(&i);
  for(i = 0; i < surfs; i++){
    greyval = 55 + greyincr*(i +1);
    RGB_TO_INT(greyval,greyval,greyval,color);
    GS_set_wire_color(surf_list[i], color);
  }
  free(surf_list);

#else
  
  int i, ramp[MAX_SURFS];
  int sortSurfs[MAX_SURFS], sorti[MAX_SURFS];
  make_red_yellow_ramp(ramp, surfs, 30, 255);
  sort_surfs_mid(sortSurfs, sorti, surfs);
  
  for(i = 0; i < surfs; i++) {
    GS_set_wire_color(sortSurfs[i], ramp[i]);
  }

#endif
  return 0;
}

int Ninit(Tcl_Interp *interp, Tk_Window w)
{
  static Nv_data data;

  
  init_commands(interp, &data);
  Ninitdata(interp, &data);

  /* compile in the home directory */
  Tcl_SetVar(interp, "src_boot", getenv("GISBASE"), TCL_GLOBAL_ONLY);
}  

void swap_togl();

int Ninitdata(
     Tcl_Interp *interp,			/* Current interpreter. */
     Nv_data *data)
{
  char rescmd[120], *string, **argv;
  int argc;
  int i;
  
  argc = Ngetargs(interp, &argv);
  
  G_gisinit (argv[0]);
  GS_libinit();
  GS_set_swap_func(swap_togl);
  data->NumCplanes = 0;
  data->CurCplane = 0;
  parse_command(data, interp, argc, argv);

}
