/* ***************************************************************
 * *
 * * MODULE:       v.digit
 * * 
 * * AUTHOR(S):    Radim Blazek
 * *               
 * * PURPOSE:      Edit vector
 * *              
 * * COPYRIGHT:    (C) 2001 by the GRASS Development Team
 * *
 * *               This program is free software under the 
 * *               GNU General Public License (>=v2). 
 * *               Read the file COPYING that comes with GRASS
 * *               for details.
 * *
 * **************************************************************/
#define MAIN
#include <stdio.h>
#include <stdlib.h>
#include <tcl.h>
#include <tk.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "Vect.h"
#include "global.h"
#include "proto.h"

int Tcl_AppInit(Tcl_Interp* interp)
{
    int ret;
    char buf[1024];
    
    ret = Tcl_Init(interp);
    if (ret != TCL_OK) { return TCL_ERROR; }
    ret = Tk_Init(interp);
    if (ret != TCL_OK) { return TCL_ERROR; }

    Toolbox = interp;

    Tcl_CreateCommand(interp, "c_tool_centre", (Tcl_CmdProc*) c_tool_centre, (ClientData) NULL, (Tcl_CmdDeleteProc*) NULL);
    Tcl_CreateCommand(interp, "c_next_tool", (Tcl_CmdProc*) c_next_tool, (ClientData) NULL, (Tcl_CmdDeleteProc*) NULL);
    Tcl_CreateCommand(interp, "c_cancel", (Tcl_CmdProc*) c_cancel, (ClientData) NULL, (Tcl_CmdDeleteProc*) NULL);

    sprintf(buf,"%s/etc/v.digit/toolbox.tcl", G_gisbase());
    ret = Tcl_EvalFile(interp, buf);
    if ( ret == TCL_ERROR) {
        if (interp->result != NULL) 
            sprintf(buf,"Cannot open toolbox: %s\n", interp->result);
	else 
            sprintf(buf,"Cannot open toolbox.\n");
        G_fatal_error(buf) ;
    }
    return TCL_OK;
}

int 
main (int argc, char *argv[])
{
    int level;
    struct GModule *module;
    struct Option *map_opt;
    char   *mapset;
    
    map_opt = G_define_standard_option(G_OPT_V_MAP);
    
    /*
    opt = G_define_option();
    opt->key = "display";
    opt->type =  TYPE_STRING;
    opt->required = NO;
    opt->multiple = NO;
    opt->description  = "d.* commands to be use for background separated by ';'";
    */
    
    G_gisinit(argv[0]);
    if (G_parser (argc, argv))
	exit(-1); 
   
    module = G_define_module(); 
    module->description = "Edit GRASS vector.";
 
    Tool_next = TOOL_NOTHING;
    G_get_window(&Region);
    G_debug (1, "Region: N = %f S = %f E = %f W = %f", Region.north, Region.south, Region.east, Region.west);
    
    /* Check driver */
    if (R_open_driver() != 0) G_fatal_error ("No graphics device selected");
    R_close_driver();

    G_debug (0, "Driver opened");
    
    /* Open map */
    Vect_set_open_level(2);
    mapset = G_find_vector2 (map_opt->answer, G_mapset()); 
    level = Vect_open_update (&Map, map_opt->answer, mapset);

    if ( level < 2 ) G_fatal_error ("Cannot open the map");
    G_debug (0, "Map opened");

    /* Display the map */
    open_driver ();
    display_map ();
    close_driver ();

    /* Open toolbox */
    Tk_Main(0, argv, Tcl_AppInit);
    
    /* Not reached */
    exit(0) ;
}

int
end ( void ) 
{
    G_debug (0, "end()");
    /* Enable this later (not usefull for debugging) */
    Vect_build ( &Map, stdout );
    Vect_close (&Map);
    
    return 1;
}
