#define GLOBAL
#include "globals.h"
#include <signal.h>

main(argc, argv) char *argv[];
{
    char *name, *blockname, *subblock, *mapset, *camera, msg[100];
    struct Cell_head cellhd;
    int ok, error();
    int nfiles, subnfiles;
    struct Block_Image_Group_Ref Block_Ref;

    if (argc != 3)
    {
	fprintf (stderr, "usage: %s blockname subblock\n", argv[0]);
	exit(1);
    }

    G_gisinit (argv[0]);
    G_suppress_masking();	/* need to do this for target location */
    name     = (char *) G_malloc (40 * sizeof (char));
    blockname= (char *) G_malloc (40 * sizeof (char));
    subblock = (char *) G_malloc (40 * sizeof (char));
    mapset   = (char *) G_malloc (40 * sizeof (char));
    camera   = (char *) G_malloc (40 * sizeof (char));

    interrupt_char = G_intr_char();
    tempfile1 = G_tempfile();
    tempfile2 = G_tempfile();
    tempfile_dot = G_tempfile();
    cell_list = G_tempfile();
    vect_list = G_tempfile();
    group_list = G_tempfile();
    digit_points = G_tempfile();

    R_open_driver();

/* get block ref */
    blockname = argv[1];
    strcpy (block.name, blockname);
    if (!I_find_block (blockname))
    {
	fprintf (stderr, "** Block [%s] not found\n", blockname);
	exit(1);
    }

    subblock = argv[2];
    I_get_subblock_Block_Image_Group_Ref (blockname, subblock, &block.ref);
    nfiles = block.ref.nfiles;

    if (!ask_one_file(block.name, subblock, group.name, block.ref))
    {  
/*        sprintf (msg, "group selected [%s]", group.name);
	  G_fatal_error(msg);
*/
        exit(0);
    }

/* get the group ref */    
    I_get_group_ref (group.name, &group.ref);
    nfiles = group.ref.nfiles;
/***
    G_get_cellhd (group.ref.file[0].name, group.ref.file[0].mapset, &cellhd);
***/
/* write block files to block list file */
    prepare_group_list();

/** look for camera info  for this block **/
    G_suppress_warnings(1);
    if (!I_get_block_camera (block.name,camera))
    {   sprintf (msg, "No camera reference file selected.\nRun i.build.block to select a camera file for block [%s]\n", blockname);
	G_fatal_error(msg);
    }
    if (!I_get_cam_info (camera, &group.cam_info))
    {   sprintf (msg, "Bad format in camera file for block [%s]\n",blockname);
	G_fatal_error(msg);
    }
    G_suppress_warnings(0);

/* get initial camera exposure station, if any*/
    if (! (ok = I_find_initial(group.name)))
    {
       sprintf (msg, "No initial camera exposure station for group[%s]\n",group.name);
       G_warning(msg);
    }

    if (ok)
      if (!I_get_init_info (group.name, &group.init_info))
      {
         sprintf (msg, "Bad format in initial camera exposure station for group [%s]\n", group.name);
         G_warning (msg);
      }

/* get target info and enviroment */
    G_suppress_warnings(1);
    get_target();
    find_target_files();
    G_suppress_warnings(0);

/* read group reference points, if any */
    G_suppress_warnings(1);
    if (!I_get_ref_points (group.name, &group.ref_points))
      {
        G_suppress_warnings(0);
        if (group.ref_points.count == 0)
           sprintf (msg, "No reference points for group [%s].\n  Please run OPTION (2).", group.name);
        else if (group.ref_equation_stat == 0)
           sprintf (msg, "Poorly placed reference points for group [%s].\n  Please run OPTION (2) agian!", group.name);
        G_fatal_error (msg);
      }
    G_suppress_warnings(0);

/* determine tranformation equation */
    Compute_ref_equation();

/* read group control points, format image E,N,cfl, target E,N,Z*/
    G_suppress_warnings(1);
    if (!I_get_con_points (group.name, &group.con_points))
	group.con_points.count = 0;
    G_suppress_warnings(0);

/* compute photo coordinates of image control points  */
    I_convert_con_points (group.name, &group.con_points, &group.photo_points,  group.E12, group.N12);

/* determine tranformation equation */
    fprintf (stderr,"Computing equations ...\n");
    if (group.con_points.count > 0)
       Compute_ortho_equation();


/*   signal (SIGINT, SIG_IGN); */
/*   signal (SIGQUIT, SIG_IGN); */

    select_current_env();
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

    drawcell(VIEW_MAP1);
    display_conz_points(1);

    Curses_clear_window (PROMPT_WINDOW);

/* determine initial input method. */
    setup_digitizer();
    if (use_digitizer)
    {
	from_digitizer = 1;
	from_keyboard  = 0;
	from_flag = 1;
    }

/* go do the work */
    driver();

    quit(0);
}

quit(n)
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
    unlink (tempfile_elev);
    unlink (tempfile_dot);
    exit(n);
}

error (msg, fatal)
    char *msg;
{
    char buf[200];
    int x,y,button;

Curses_clear_window (PROMPT_WINDOW);
Curses_write_window (PROMPT_WINDOW,1,1, "LOCATION:\n");
Curses_write_window (PROMPT_WINDOW,1,12,G_location());
Curses_write_window (PROMPT_WINDOW,2,1, "MAPSET:\n");
Curses_write_window (PROMPT_WINDOW,2,12,G_mapset());
    Beep();
    if (fatal)
	sprintf (buf, "ERROR: %s", msg);
    else
	sprintf (buf, "WARNING: %s (click mouse to continue)", msg);
    Menu_msg (buf);

    if (fatal)
	quit(1);
    Mouse_pointer (&x, &y, &button);
Curses_clear_window (PROMPT_WINDOW);
}





