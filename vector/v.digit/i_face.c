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

/* Set GUI coordinates to given values */
int
i_coor ( double x, double y)
{
    char buf[100];

    if ( x == COOR_NULL || y == COOR_NULL )
        buf[0] = '\0';
    else
        sprintf ( buf, "%.2f, %.2f", x, y);
    
    Tcl_SetVar(Toolbox, "coor", buf, TCL_GLOBAL_ONLY);  
    return 1;
}

/* Set symbology color */
int i_set_color ( char * name, int r, int g, int b ) 
{
    char var[50], col[20];    
    
    G_debug (2, "i_set_color(): %s : %d %d %d", name, r, g, b );

    sprintf (col, "#%02x%02x%02x", r, g, b);
    
    G_debug (2, " -> color = %s", col );
    
    sprintf (var, "symb(%s,color)", name);
    Tcl_SetVar(Toolbox, var, col, TCL_GLOBAL_ONLY);  
    
    return 1;
}

/* Set symbology on/off */
int i_set_on ( char * name, int on ) 
{
    char var[50], val[20];    
    
    G_debug (2, "i_set_on(): %s : %d", name, on );

    sprintf (var, "symb(%s,on)", name);
    sprintf (val, "%d", on);
    Tcl_SetVar(Toolbox, var, val, TCL_GLOBAL_ONLY);  
    
    return 1;
}
    
/* Set snappping */
void i_set_snap ( void ) 
{
    char val[20];
    
    G_debug (2, "i_set_snap()");

    sprintf (val, "%d", Snap);
    Tcl_SetVar(Toolbox, "snap", val, TCL_GLOBAL_ONLY);  
    
    sprintf (val, "%d", Snap_mode);
    Tcl_SetVar(Toolbox, "snap_mode", val, TCL_GLOBAL_ONLY);  
    
    sprintf (val, "%d", Snap_screen);
    Tcl_SetVar(Toolbox, "snap_screen", val, TCL_GLOBAL_ONLY);  
    
    sprintf (val, "%f", Snap_map);
    Tcl_SetVar(Toolbox, "snap_map", val, TCL_GLOBAL_ONLY);  
}

/* This function should be regularly called from C to get GUI requests */
int i_update ( void ) {
    G_debug (5, "i_update");
    Tcl_Eval ( Toolbox, "update" );
    return 1;
}

/* create: 1 - create, 0 - destroy */
void i_new_line_options ( int create ) {
    int i;
    char val[1000];
    
    G_debug (5, "i_new_line_options");

    if ( create ) {
	Tcl_Eval ( Toolbox, "new_line_options 1" );

	/* Set cat mode */
	sprintf (val, ".lineopt.cmode configure -values [list" );
	for ( i = 0; i < CAT_MODE_COUNT; i++ ) {
	    sprintf (val, "%s \"%s\"", val, CatModeLab[i] );
	}
	sprintf (val, "%s]", val);
	
	G_debug (2, "Cat modes: %s", val);
	Tcl_Eval ( Toolbox, val );

	sprintf (val, ".lineopt.cmode setvalue @%d", CatMode );
	G_debug (2, "Cat mode: %s", val);
	Tcl_Eval ( Toolbox, val );

	i_new_line_field_set ( FieldCat[0][0] );
	i_new_line_cat_set ( FieldCat[0][1] );
    } else { 
	Tcl_Eval ( Toolbox, "new_line_options 0" );
    }
    
    sprintf (val, "%d", FieldCat[0][0]);
    Tcl_SetVar(Toolbox, "field", val, TCL_GLOBAL_ONLY);  
    i_set_cat_mode ();
}

/* set category options */
void i_set_cat_mode ( void ) {
    
    G_debug (5, "i_set_cat_mode");

    if ( CatMode == CAT_MODE_NO ) {
	Tcl_Eval ( Toolbox, ".lineopt.fval configure -state disabled" );
    } else {
	Tcl_Eval ( Toolbox, ".lineopt.fval configure -state normal" );
    }

    if ( CatMode == CAT_MODE_MAN ) {
	Tcl_Eval ( Toolbox, ".lineopt.cval configure -state normal" );
    } else {
	Tcl_Eval ( Toolbox, ".lineopt.cval configure -state disabled" );
    }

    /* set nex not used category if necessary */
    i_new_line_cat_set_next();
}

/* set category */
void i_new_line_cat_set ( int cat ) {
    char val[20];    
    
    G_debug (5, "i_new_line_cat_set");
    sprintf (val, "%d", cat);
    Tcl_SetVar(Toolbox, "cat", val, TCL_GLOBAL_ONLY);  
}

/* set category to next if mode is next */
void i_new_line_cat_set_next ( void ) {
    int cat;
    
    G_debug (5, "i_new_line_cat_set_next");
    
    if ( CatMode == CAT_MODE_NEXT ) { /* set nex not used category */
	cat = cat_max_get ( FieldCat[0][0] ) + 1;
	FieldCat[0][1] = cat; /* This shoud not be here, in cat.c ? */
        i_new_line_cat_set ( cat );
    }
}

/* set category */
void i_new_line_field_set ( int field ) {
    char val[20];    
    
    G_debug (5, "i_new_line_field_set");
    sprintf (val, "%d", field);
    Tcl_SetVar(Toolbox, "field", val, TCL_GLOBAL_ONLY);  
}
