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
  int id;
  int pt[2];
  int color;
  char text[120];
  char font[100];
 
  if (argc != 6) {
    Tcl_SetResult(interp, "Error: should be Nplace_label text font color xpos ypos", TCL_STATIC);
    return (TCL_ERROR);
  }

 sprintf(text, "%s", argv[1]);
 sprintf(font, "%s", argv[2]);
 
 color = (int)tcl_color_to_int (argv[3]);

  pt[0] = (int)atoi(argv[4]);
  pt[1] = (int)atoi(argv[5]); 

  /* Print the label */
  gs_put_label(text, font, color, pt);
  
   
  return (TCL_OK);
}


/* Just a stub */
void G_site_destroy_struct(void *foo) {
}
