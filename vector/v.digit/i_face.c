#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <tcl.h>
#include <tk.h>
#include "gis.h"
#include "raster.h"
#include "Vect.h"
#include "global.h"
#include "proto.h"

/* Interface functions between GUI and C:
*  i_*() functions in GUI,  called from C 
*/

/* Set GUI promt to given string */
int
i_prompt (char *str)
{
    Tcl_SetVar(Toolbox, "prompt", str, TCL_GLOBAL_ONLY);  
    return 1;
}

/* Set GUI promt to given string */
int
i_prompt_buttons (char *l, char *m, char *r)
{
    Tcl_SetVar(Toolbox, "prompt_left", l, TCL_GLOBAL_ONLY);  
    Tcl_SetVar(Toolbox, "prompt_middle", m, TCL_GLOBAL_ONLY);  
    Tcl_SetVar(Toolbox, "prompt_right", r, TCL_GLOBAL_ONLY);  
    return 1;
}

/* This function should be regularly called from C to get GUI requests */
int i_update ( void ) {
    G_debug (3, "i_update");
    Tcl_Eval ( Toolbox, "update" );
    return 1;
}
