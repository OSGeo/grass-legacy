/*=======================================================================
				i.points
  main.c --
         Highly interactive program for marking control points for 
	 image rectification.

	 Usage: i.points (must be run interactiely for now)


	 Imagery groups are built using the GRASS command i.group
	 imagery group data is stored in $LOCATION/$MAPSET/group/group_name

	 The program i.target must also be run prior to i.points.

	 Finally, since i.points uses the graphics display, the user
	 will have to start a display using the d.mon command.

	 The imagery group data directory may contain the following files:
	 REF         - name and mapsets of raster files in this group
	 TARGET      - name of target location for rectification.
	 
	 POINTS      - earlier version of a control points file, now replaced
	               with the following file.

	 POINTS_LL   - control points file containg (source pixel and line)
	               and target coordinates as (lat and lon).


	 The following files have been added for ortho-photo and LandSat TM
	 rectification programs.

	 ELEV        - contains elevation data file name and mapset within
	               the target location.
	 TRANS       - current type of transformation being preformed:
	               { POLY1, POLY2, POLY3, PHOTO, LAND_TM }
	 CAMERA      - camera data for ortho-photo rectification

	 INIT_EXPOSE - initial exposure infomation for ortho-photo.

	 INIT_TM     - initial data extracted from TM tapes.

  Changes: 
      (1) removed all Curses calls.   

=======================================================================*/


/** TODO -- clean up include files               **/
/* "globals.h" includes the following headers:    */
/*     - "defs.h"  which includes:                */
/*	   -  "ortho_image.h" which includes:     */
/*		 - "gis.h"                        */
/*		 - "imagery.h"                    */
/*         -  <curses.h>                          */

#include <stdlib.h>
#include <unistd.h>
#include <curses.h>
#include <signal.h>
#define GLOBAL
#include "globals.h"
#include "raster.h"

/*---------------------------------------------------------------------*/

/* internal function prototypes */
#ifdef _NO_PROTO
    int quit();
    int error();
#else
    int quit  (int n);
    int error (char *msg, int fatal);
#endif

/*---------------------------------------------------------------------*/

int main(int  argc, char *argv[])
{
    int  ret;                   /* tmp return code values */


    /* Initial the GRASS program */
    G_gisinit (argv[0]);
    G_suppress_masking();	/* need to do this for target location ?? */


    /* these are all defined in globals.h */
    /* used when asking user to select a file */
    interrupt_char  = G_intr_char();
    tempfile1       = G_tempfile();
    tempfile2       = G_tempfile();
    cell_list       = G_tempfile();
    vect_list       = G_tempfile();
    group_list      = G_tempfile();
    color_list      = G_tempfile();
    tempfile_camera = G_tempfile();


    /*** TODO -- digit stuff waiting for GRASS4.1 to interface with geo.reg ***/
    digit_points    = G_tempfile();
    digit_results   = G_tempfile();


    /** TODO -- use parser **/
    /* ask user for an imagery group to rectify */
    if (!I_ask_group_old ("Enter imagery group for i.points3", group.name))
      G_fatal_error("Can't open imagery group\n");

    /* get the imagery group referce structure */
    if (!I_get_group_ref (group.name, &group.ref))
      G_fatal_error("Can't get imagery group\n");
    if (group.ref.nfiles <= 0)
      {
	fprintf (stderr, "Group [%s] contains no files\n", group.name);
	sleep(2);
	G_fatal_error("Imagery group contains no files\n");
      }

             /* write group files to group list file */
    prepare_group_list();

             /* get target info and enviroment, built by i.target */
    get_target();

             /* get a list of available cell and vector data in target location */
    find_target_files();

             /** TODO -- use standad color list **/
             /* a simple list of vector colors */
    find_vector_colors();


    /* supprest warnings about getting files */
    G_suppress_warnings(1); 

    /* determine tranformation equation */
    ret = I_get_group_trans (&group);
    if (ret == -1 ) {
      /* set transtype to POLY if TRANS file didn't exist yet */
      group.trans_type = POLY1;
    }
    else if (ret == -2 ) {
      /* error reading TRANS file */
       G_fatal_error ("Bad format in group transformation file\n");
    }


    /* initialize the transformation functions for the group */
    if (I_init_group_trans_funcs (&group) < 0) {
       G_fatal_error ("Can't initialize group transformation functions\n");
    }


    group.stat = 0;
    /* initialize the group points data */
    if (group.get_points != NULL) {
      if (group.get_points (&group) <0 ) {
	G_warning ("Can't get points for group\n");
	group.stat = -1;
      }
    }

    /* get the groups auxilarry data */
    if (group.get_auxil != NULL) {
      if (group.get_auxil (&group) != 1 ) {
	G_warning ("Can't get auxilary data for group\n");
	group.stat = -1;
      }
    }

    /* get the groups coeffs data */
    if (group.get_coefs != NULL) {
      if (group.get_coefs (&group) <0 ) {
	G_warning ("Can't get coefs data for group \n");
	group.stat = -1;
      }
    }


    /* do an initial calculation of the transformation parameters */
    if (group.calculate_trans (&group) < 0 ) {
       G_warning ("Problem running inital transfromation\n");
    }

    /* turn warnings messages back on */
    G_suppress_warnings(0); 


         /* TODO -- chatch all interupt signals HELP  */
    /*  signal (SIGINT,  SIG_IGN); */
    /*  signal (SIGQUIT, SIG_IGN); */



    /* initialize display list */
    /* display list struct is defined in "defs.h" */
    /* and contains names and mapsets of all cell and vect */
    /* files that have been displayed.  Used to redraw  */
    /* everything when a zoom occures */
    display_list.num_raster  = 0;
    display_list.num_vectors = -1;


    /*** TODO -- waiting for 4.1 digitizer interface ****/
    /* determine if digitizer is to be used */
    setup_digitizer();
    if (use_digitizer)
    {
	from_digitizer = 1;
	from_keyboard  = 0;
	from_flag = 1;
    }


    /* default is zoom_box style */
    use_zoom_box = 1;
    use_zoom_pnt = 0;


    /* open the raster graphics driver */
    if (R_open_driver() != 0)
       G_fatal_error ("No graphics device selected");
           
    /* initialize the graphics screens into VIEWS */
    Init_graphics();
    display_title (VIEW_MAP1);
    select_target_env ();
    display_title (VIEW_MAP2);
    select_current_env ();


    /* set the curses routines for ascii writing to terminal */
    /* Begin_curses(); */

    /* set the error routines that will be called */
    /* from G_warning and G_fatal_error */
    G_set_error_routine (error);

    /*** TODO  -- signal catch stuff HELP ***/
/***
 ***#ifdef SIGTSTP
 ***    signal (SIGTSTP, SIG_IGN);
 ***#endif
****/


    /* ask user for an imagery group file to be displayed */
    plotimg();

    /* clear the text window */  
    /* Curses_clear_window (PROMPT_WINDOW); */

    /* go do the work */
    driver();

    quit(0);
}

/*-----------------------------------------------------------------------*
 * Quit routine ends any Raster drawing operations, closes down the 
 * digitizer, and close the tempfiles, then exits.
 *-------------------------------------------------------------------------*/
int quit(int n)
{
    char command[1024];

    /* End_curses(); */
    R_close_driver();

    /*** TODO -- digitizer closing in 4.1  ***/
    if (use_digitizer)
    {
	sprintf (command, "%s/etc/geo.unlock %s",
	    G_gisbase(), digit_points);
	system (command);
    }

    /* unlink all tempfiles created */
    unlink (tempfile1);
    unlink (tempfile2);
    unlink (cell_list);
    unlink (group_list);
    unlink (vect_list);
    unlink (color_list);
    unlink (digit_points);
    unlink (digit_results);
    unlink (tempfile_camera);
    exit(n);
}


/*-------------------------------------------------------------------------*/
/* All G_warnings and G_fatal error are processed throught this routine.   */
/* Display location, and mapset of the enviroment where the error occured  */
/* If not fatal error, request the user to click the mouse before continuing */
/*-------------------------------------------------------------------------*/
int error (
    char *msg,          /* message to print */
    int  fatal)         /* 1= fatal, other= warning */
{
    char error_buf[200];       
    int x,y,button;

          /* print location and mapset */
/**    Curses_clear_window (PROMPT_WINDOW);
 **    Curses_write_window (PROMPT_WINDOW,1,1, "LOCATION:\n");
 **    Curses_write_window (PROMPT_WINDOW,1,12,G_location());
 **    Curses_write_window (PROMPT_WINDOW,2,1, "MAPSET:\n");
 **    Curses_write_window (PROMPT_WINDOW,2,12,G_location());
**/

    /** Beep();  **/
    if (fatal)
	sprintf (error_buf, "ERROR: %s", msg);
    else
	sprintf (error_buf, "WARNING: %s: (click mouse to continue)", msg);
    Menu_msg (error_buf);

    if (fatal)
	quit(1);
    
        /* get a mouse mouse click in graphics window */
    Mouse_pointer (&x, &y, &button);
    /** Curses_clear_window (PROMPT_WINDOW); **/

    return 0;
}


