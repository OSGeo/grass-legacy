#define GLOBAL
#include "globals.h"
#include <signal.h>

main(argc, argv) char *argv[];
{
    char *name, *blockname, *subblock, *mapset, *camera, msg[200];
    struct Cell_head cellhd;
    int error();
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
    tempfile3 = G_tempfile();
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
        sprintf (msg, "group selected [%s]", group.name);
	G_fatal_error(msg);
        exit(0);
    }

/*  sprintf (msg, "group selected [%s]", group.name); */
/*  G_warning(msg);   */

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
    if (!I_get_block_camera (blockname,camera))
    {   
        sprintf (msg, "No camera reference file selected.\nRun i.build.block to select a camera file for block [%s]\n", blockname);
	G_fatal_error(msg);
    }

    if (!I_get_cam_info (camera, &block.camera))
    {  
        sprintf (msg, "Bad format in camera file for block [%s]\n",blockname);
	G_fatal_error(msg);
    }
    G_suppress_warnings(0);

/*fprintf (stderr, "Looking for reference points\n");*/
/* read block reference points, if any */
    G_suppress_warnings(1);
    if (!I_get_ref_points(group.name, &group.ref_points))
	group.ref_points.count = 0;
    G_suppress_warnings(0);

/*fprintf (stderr, "Found reference points\n");*/
/* determine tranformation equation */
    Compute_equation();

    signal (SIGINT, SIG_IGN);
    signal (SIGQUIT, SIG_IGN); 

    Init_graphics();
    display_title (VIEW_MAP1);
    select_current_env ();

    Begin_curses();
    G_set_error_routine (error);

/*
#ifdef SIGTSTP
    signal (SIGTSTP, SIG_IGN);
#endif
*/


/* ask user for cell file to be displayed */
    do
    {  if(!choose_groupfile (name, mapset)) 
	    quit(0);
    } while (G_get_cellhd (name, mapset, &cellhd) < 0);

/* display this file in "map1" */
    G_adjust_window_to_box (&cellhd, &VIEW_MAP1->cell.head, VIEW_MAP1->nrows, VIEW_MAP1->ncols);
    Configure_view (VIEW_MAP1, name, mapset, cellhd.ns_res, cellhd.ew_res);

    drawcell(VIEW_MAP1);
    display_ref_points(1);
    Curses_clear_window (PROMPT_WINDOW);

/* determine initial input method. */
    if (setup_camera_file() < 0) quit(0); 
    if (use_camera_file)
      { from_keyboard  = 0;
        from_screen = 1;
	from_flag = 1;
      }
    else
      { from_keyboard  = 1;
        from_screen = 0;
	from_flag = 0;
      }


/* go do the work */
    driver();
/* leave */
    quit(0);
}

quit(n)
{
    char command[1024];

    End_curses();
    R_close_driver();
    unlink (tempfile1);
    unlink (tempfile2);
    unlink (tempfile3);
    unlink (cell_list);
    unlink (group_list);
    unlink (vect_list);
    unlink (digit_points);
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
Curses_write_window (PROMPT_WINDOW,2,12,G_location());
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



