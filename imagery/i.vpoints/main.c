/*
    REVISION of i.points to work with vector map layers
	and 2nd/3rd order transformations

	BY:  Bill Enslin & Brian Buckley
	     Center for Remote Sensing
	     Michigan State University
	     302 Berkey Hall
             East Lansing, MI 48824-1111

	     (517) - 353-7195

$Id$

VERSION DATE: 6-30-92          

*/

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <grass/gis.h>
#include <grass/raster.h>
#include <grass/glocale.h>
#define GLOBAL
#include "globals.h"

int main (int argc, char *argv[])
{
    char name[GNAME_MAX], mapset[GMAPSET_MAX], xmapset[GMAPSET_MAX];
    struct Cell_head cellhd;
    struct Option *grp;
    struct GModule *module;

    /* must run in a term window */
    setenv("GRASS_UI_TERM","1",TRUE);

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description =
      _("Set registration points for an imagery group from a vector map "
        "or keyboard entry.");

    grp = G_define_option();
    grp->key		 = "group";
    grp->type		 =  TYPE_STRING;
    grp->required	 =  YES;
    grp->gisprompt	  = "old,group,group";
    grp->description	 = _("Name of imagery group to be registered");

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);


    G_suppress_masking();    /* to do this for target location */

    interrupt_char = G_intr_char();
    tempfile1 = G_tempfile();
    tempfile2 = G_tempfile();
    cell_list = G_tempfile();
    vect_list = G_tempfile();
    group_list = G_tempfile();
    digit_points = G_tempfile();
    digit_results = G_tempfile();

    R_open_driver();

    /* parse group name */
    /* only enforce local-mapset-only due to I_get_group_ref() not liking "@mapset" */
    if( G__name_is_fully_qualified(grp->answer, group.name, xmapset) ) {
	if( 0 != strcmp(G_mapset(),xmapset) )
	    G_fatal_error(_("[%s] Only local groups may be used"), grp->answer);
    }
    else {
	strncpy(group.name, grp->answer, GNAME_MAX-1);
	group.name[GNAME_MAX-1] = '\0'; /* strncpy() doesn't null terminate on overflow */
    }

    if (!I_get_group_ref (group.name, &group.ref))
	G_fatal_error(_("Group [%s] contains no maps, run i.group"), group.name);

    if (group.ref.nfiles <= 0)
	G_fatal_error(_("Group [%s] contains no maps, run i.group"), group.name);

/* write group files to group list file */
    prepare_group_list();

/* get target info and enviroment */
    get_target();
    find_target_files();

/* read group control points, if any */
    G_suppress_warnings(1);
    if (!I_get_control_points (group.name, &group.points))
	group.points.count = 0;
    G_suppress_warnings(0);

/* determine tranformation equation */
    CRS_Compute_equation(1);

    signal (SIGINT, SIG_IGN);
/*  signal (SIGQUIT, SIG_IGN); */

    Init_graphics();
    display_title (VIEW_MAP1);
    select_target_env ();
    display_title (VIEW_MAP2);
    select_current_env ();

    Begin_curses();
    G_set_error_routine (error);

/*
#ifdef SIGTSTP
    signal (SIGTSTP, SIG_IGN);
#endif
*/


/* ask user for group file to be displayed */
    do
    {
	if(!choose_groupfile (name, mapset))
	    quit(0);
/* display this file in "map1" */
    }
    while (G_get_cellhd (name, mapset, &cellhd) < 0);

    G_adjust_window_to_box (&cellhd, &VIEW_MAP1->cell.head, VIEW_MAP1->nrows, VIEW_MAP1->ncols);
    Configure_view (VIEW_MAP1, name, mapset, cellhd.ns_res, cellhd.ew_res);

    drawcell(VIEW_MAP1,1);
    display_points(1);

    Curses_clear_window (PROMPT_WINDOW);

/* determine initial input method. */

use_digitizer = 0;

/*    DELETED 6-25-92 if want to permanently delete, delete digit.c
and other references to digitizer stuff
  
setup_digitizer();
    if (use_digitizer)
    {
	from_digitizer = 1;
	from_keyboard  = 0;
	from_flag = 1;
    }
*/
 
/*go do the work */
    cellmap_present=0;
    driver();

    quit(0);
}

int quit (int n)
{
    char command[1024];

    End_curses();
    R_close_driver();
    if (use_digitizer)
    {
	sprintf (command, "%s/etc/geo.unlock %s",
	    G_gisbase(), digit_points);
	system (command);
    }
    unlink (tempfile1);
    unlink (tempfile2);
    unlink (cell_list);
    unlink (group_list);
    unlink (vect_list);
    unlink (digit_points);
    unlink (digit_results);

    system("d.frame -s full_screen -e");

    exit(n);
}

int error (char *msg, int fatal)
{
    char buf[200];
    int x,y,button;

    Curses_clear_window (PROMPT_WINDOW);
    Curses_write_window (PROMPT_WINDOW,1,1, "LOCATION:\n");
    Curses_write_window (PROMPT_WINDOW,1,12,G_location());
    Curses_write_window (PROMPT_WINDOW,2,1, "MAPSET:\n");
    Curses_write_window (PROMPT_WINDOW,2,12,G_location());

    Beep();
    if (fatal)
	sprintf (buf, _("ERROR: %s"), msg);
    else
	sprintf (buf, _("WARNING: %s (click mouse to continue)"), msg);

    Menu_msg (buf);

    if (fatal)
	quit(1);

    Mouse_pointer (&x, &y, &button);
    Curses_clear_window (PROMPT_WINDOW);

    return 0;
}
