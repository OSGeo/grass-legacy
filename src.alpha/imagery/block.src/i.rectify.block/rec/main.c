#define GLOBAL
#include "global.h"

main(argc, argv) char *argv[];
{
    char *name, *blockname, *subblock, *mapset, *camera, msg[100];
    int n, nfiles, subnfiles, ok;
    struct Block_Image_Group_Ref Block_Ref;
    char tl[100];
    char math_exp[100];
    char units[100];
    char nd[100];

/*Debug*/
    Bugsr = fopen ("/dev/null","w");
    setbuf (stdout, NULL);
    setbuf (stderr, NULL);

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
    elev_layer  = (char *) G_malloc (40*sizeof (char),1);
    mapset_elev = (char *) G_malloc (40*sizeof (char),1);

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
    sprintf (msg, "group selected [%s]", group.name);
    G_warning(msg);   

/* get the group ref */    
    I_get_group_ref (group.name, &group.ref);
    nfiles = group.ref.nfiles;
    if (nfiles <= 0)
    {
	sprintf (msg, "No files in this group!\n");
	G_fatal_error (msg);
    }
    ref_list = (int *) G_malloc (nfiles * sizeof(int));
    new_name = (char **) G_malloc (nfiles * sizeof(char *));
    for (n = 0; n < nfiles; n++)
	ref_list[n] = -1;

/* get the target */
    get_target(block.name);

/* ask for files to be rectified */
    ask_files (group.name);

/* get the block elevation layer cell file  in target location */
/*
fprintf (stderr,"Looking for elevation file in block: %s\n", block.name);
sleep (3);
*/
       I_get_block_elev (block.name, elev_layer, mapset_elev, tl, math_exp, units, nd);

/*
fprintf (stderr,"Block elevation: %s in %s\n", elev_layer, mapset_elev);
sleep (3);
*/

/* get the elevation layer header in target location */
    select_target_env(); 
    G_get_cellhd (elev_layer, mapset_elev, &elevhd);
    select_current_env();

/** look for camera info  for this block **/
    if (!I_get_block_camera (block.name,camera))
    {   sprintf (msg, "No camera reference file selected for block [%s]\n", block);
	G_fatal_error(msg);
    }
    if (!I_get_cam_info (camera, &group.cam_info))
    {   sprintf (msg, "Bad format in camera file for block [%s]\n",block);
	G_fatal_error(msg);
    }

/* get initial camera exposure station, if any*/
    if (I_find_initial(group.name))
    {
       if (!I_get_init_info (group.name, &group.init_info))
       {
	  sprintf (msg, "Bad format in initial exposusre station file for group [%s]\n", group.name);
	  G_warning (msg);
       }
    }

/* read the reference points for the group, compute image-to-photo trans. */
    get_ref_points ();

/* read the control points for the group, convert to photo coords. */
    get_conz_points ();

/* ask for window to be used in target location */
    select_current_env();
    get_target_window();

/* modify elevation values if needed */
/***
    select_target_env();
    ask_elev_data();
    select_current_env();
***/ 

/*  go do it */
    exec_rectify ();
}




