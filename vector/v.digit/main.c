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

    Tcl_CreateCommand(interp, "c_tool_centre", (Tcl_CmdProc*) c_tool_centre, (ClientData) NULL, 
	                      (Tcl_CmdDeleteProc*) NULL);
    Tcl_CreateCommand(interp, "c_next_tool", (Tcl_CmdProc*) c_next_tool, (ClientData) NULL, 
	                      (Tcl_CmdDeleteProc*) NULL);
    Tcl_CreateCommand(interp, "c_cancel", (Tcl_CmdProc*) c_cancel, (ClientData) NULL, 
	                      (Tcl_CmdDeleteProc*) NULL);
    Tcl_CreateCommand(interp, "c_set_color", (Tcl_CmdProc*) c_set_color, (ClientData) NULL, 
	                      (Tcl_CmdDeleteProc*) NULL);
    Tcl_CreateCommand(interp, "c_set_on", (Tcl_CmdProc*) c_set_on, (ClientData) NULL, 
	                      (Tcl_CmdDeleteProc*) NULL);
    Tcl_CreateCommand(interp, "c_create_table", (Tcl_CmdProc*) c_create_table, (ClientData) NULL, 
	                      (Tcl_CmdDeleteProc*) NULL);
    Tcl_CreateCommand(interp, "c_table_definition", (Tcl_CmdProc*) c_table_definition, (ClientData) NULL, 
	                      (Tcl_CmdDeleteProc*) NULL);
    Tcl_CreateCommand(interp, "c_var_set", (Tcl_CmdProc*) c_var_set, (ClientData) NULL, 
	                      (Tcl_CmdDeleteProc*) NULL);
    Tcl_CreateCommand(interp, "c_create_bgcmd", (Tcl_CmdProc*) c_create_bgcmd, (ClientData) NULL, 
	                      (Tcl_CmdDeleteProc*) NULL);
    Tcl_CreateCommand(interp, "c_set_bgcmd", (Tcl_CmdProc*) c_set_bgcmd, (ClientData) NULL, 
	                      (Tcl_CmdDeleteProc*) NULL);
    Tcl_CreateCommand(interp, "c_add_blank_bgcmd", (Tcl_CmdProc*) c_add_blank_bgcmd, (ClientData) NULL, 
	                      (Tcl_CmdDeleteProc*) NULL);
    Tcl_CreateCommand(interp, "c_del_cat", (Tcl_CmdProc*) c_del_cat, (ClientData) NULL, 
	                      (Tcl_CmdDeleteProc*) NULL);
    Tcl_CreateCommand(interp, "c_add_cat", (Tcl_CmdProc*) c_add_cat, (ClientData) NULL, 
	                      (Tcl_CmdDeleteProc*) NULL);

    /* Init variables */
    var_init ();

    /* Init snap */
    var_seti ( VAR_SNAP, 1 );
    var_seti ( VAR_SNAP_MODE, SNAP_SCREEN );
    var_seti ( VAR_SNAP_SCREEN, 10 );
    var_setd ( VAR_SNAP_MAP, 10 );
    
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
    int    i;
    struct GModule *module;
    struct Option *map_opt, *bgcmd_opt;
    struct Flag *new_f;
    char   *mapset;
    char   **tokens; 
    
    G_gisinit(argv[0]);

    map_opt = G_define_standard_option(G_OPT_V_MAP);
    
    bgcmd_opt = G_define_option();
    bgcmd_opt->key = "bgcmd";
    bgcmd_opt->type =  TYPE_STRING;
    bgcmd_opt->required = NO;
    bgcmd_opt->multiple = NO;
    bgcmd_opt->answer = "";
    bgcmd_opt->description  = "d.* commands to be used for background separated by ';'";
    
    new_f = G_define_flag ();
    new_f->key             = 'n';
    new_f->description     = "Create new file if it does not exist.";
    
    if (G_parser (argc, argv)) exit(-1); 
   
    module = G_define_module(); 
    module->description = "Edit GRASS vector.";

    G_debug (2, "Variable = %p", Variable );
 
    /* Read background commands */
    if ( bgcmd_opt->answer ) {
        tokens = G_tokenize (bgcmd_opt->answer, ";");
        for (i = 0; tokens[i] != NULL ; i++) {
	    G_debug (2, "cmd %d : %s", i, tokens[i]);
	    bg_add ( tokens[i] ); 
	}
        G_free_tokens (tokens);
    }
    
    for ( i = 0; i < nbgcmd; i++ ) G_debug (2, "cmd %d : %s", i, Bgcmd[i]);
    
    Tool_next = TOOL_NOTHING;
    G_get_window(&Region);
    G_debug (1, "Region: N = %f S = %f E = %f W = %f", Region.north, Region.south, Region.east, Region.west);
    
    /* Check driver */
    if (R_open_driver() != 0) G_fatal_error ("No graphics device selected");
    R_close_driver();

    G_debug (1, "Driver opened");
    
    /* Open map */
    mapset = G_find_vector2 (map_opt->answer, G_mapset()); 
    if ( mapset == NULL ) {
       fprintf (stderr, "Map does not exist.\n");	
       if ( new_f->answer ) {
           fprintf (stderr, "New empty map created.\n");	
	   Vect_open_new (&Map, map_opt->answer, 0 ); 
           Vect_build ( &Map, NULL );
           Vect_close (&Map);
           Vect_open_update (&Map, map_opt->answer, G_mapset());
       } else {
	   exit ( 1 );
       }
    } else {
        Vect_set_open_level(2);
        Vect_open_update (&Map, map_opt->answer, mapset);
	Vect_set_category_index_update ( &Map );
    }
    Vect_hist_command ( &Map );

    G_debug (1, "Map opened");

    /* Init maximum categories */
    cat_init ();

    /* Init symbology for lines and nodes */
    symb_lines_init (); 
    symb_nodes_init (); 

    /* Display the map */
    symb_init ();
    G_get_window(&window);
    driver_open ();
    display_erase ();
    display_bg ();
    display_map ();
    driver_close ();

    G_get_window(&window);
    
    /* Open toolbox */
    Tk_Main(0, argv, Tcl_AppInit);
    
    /* Not reached */
    exit(0) ;
}

int
end ( void ) 
{
    G_debug (1, "end()");
    Vect_build_partial (&Map, GV_BUILD_NONE, NULL);
    Vect_build ( &Map, stdout );
    Vect_close (&Map);
    
    return 1;
}
