/*
 * Library support for placing labels.
 */

#include "interface.h"
#include <stdlib.h>

extern int get_idnum();
extern void free();

int 
Nplace_label_cmd (
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
  int bold=0, italic=0, font=1;
  
  if (argc != 8) {
    Tcl_SetResult(interp, "Error: should be Nplace_label xpos ypos text {times | helvetica | courier} font_size font_style_list color", TCL_STATIC);
    return (TCL_ERROR);
  }
  
  sx = atoi(argv[1]);
  sy = atoi(argv[2]);

  /* Set the font */
  if (Tcl_VarEval(interp,"lsearch ",argv[6]," italic",NULL) != TCL_OK)
    return (TCL_ERROR);
  if (atoi(interp->result) != -1)
    italic=1;
  if (Tcl_VarEval(interp,"lsearch ",argv[6]," bold",NULL) != TCL_OK)
    return (TCL_ERROR);
  if (atoi(interp->result) != -1)
    bold=1;
  if (!strncmp(argv[4],"times",5))
    font=1;
  else if (!strncmp(argv[4],"helvetica",9))
    font=2;
  else if (!strncmp(argv[4],"courier",7))
    font=3;
  else {
    Tcl_SetResult(interp,"Nplace_label: font must be one of times, helvetica or courier",
		  TCL_STATIC);
    return (TCL_ERROR);
  }
  
  gs_set_font(font,bold,italic,atof(argv[5]));
  
  /* Print the label */
  gs_put_label(sx, sy, argv[3]);
  
  return (TCL_OK);
}


/* Just a stub */
void G_site_destroy_struct(void *foo) {
}
