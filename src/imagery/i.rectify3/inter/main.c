/*======================================================================
                             i.rectify

  main.c --
         Highly interactive program for image rectification.

	 Usage: i.rectify (must be run interactiely for now)

	 The programs i.group, i.target, and i.points  
	 must also be run prior to i.rectify

======================================================================*/


#define GLOBAL
#include <unistd.h>
#include "global.h"
/** TODO -- elim crs.h **/
#include "crs.h" 
#include "protodefs.h" 


/*-------------------------------------------------------------------*/
int main(int argc,char *argv[])
{
    int n;
    int ret;                    /* return code */


    /* was used for debuging */
    setbuf (stdout, NULL);
    setbuf (stderr, NULL);

    /* initialize GRASS */
    G_gisinit (argv[0]);

    
/** TODO -- use parser **/

/* ask user for an imagery group to rectify */
    if (!I_ask_group_old ("Enter imagery group to be rectified", group.name))
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

/* alloc a list of names for the rectified imagery */
    ref_list = (int *)   G_malloc (group.ref.nfiles * sizeof(int));
    new_name = (char **) G_malloc (group.ref.nfiles * sizeof(char *));
    for (n = 0; n < group.ref.nfiles; n++)
	ref_list[n] = -1;


    /* supprest warnings about getting files */
    G_suppress_warnings(1); 


/* get the target LOCATION infomation */
/* needs to be done before getting the control points */
    get_target(group.name);


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

/* initialize the group points data */
    if (group.get_points != NULL) {
      if (group.get_points (&group) <0 ) {
	G_fatal_error ("Can't get points for group\n");
      }
    }

/* get the groups auxilarry data */
    if (group.get_auxil != NULL) {
      if (group.get_auxil (&group) <0 ) {
	G_fatal_error ("Can't get auxilary data for group\n");
      }
    }

/* get the groups coeffs data */
    if (group.get_coefs != NULL) {
      if (group.get_coefs (&group) <0 ) {
	G_fatal_error ("Can't get coefs data for group \n");
      }
    }


/* do an initial calculation of the transformation parameters */
    if (! (group.calculate_trans (&group))) {
       G_fatal_error ("Problem running initial transformation\n");
    }

    /* turn warnings messages back on */
    G_suppress_warnings(0); 

/* ask user for the files to be rectified */
    ask_files (group.ref);

/* ask user for target window to use for rectification */
    get_target_window(group.ref);

/* do the rectification */
    exec_rectify (group.ref);

    return 0;
}



/** TODO -- photo_init asigns group.mark_points() and group.anal_points() */
/**         which are only needed by i.points and not i.rectify */
/** Big hack **/
int 
mark_photo (void)
{
  return 0;
}

/** Big hack **/
int 
mark_control (void)
{
  return 0;
}

/** Big hack **/
int 
Menu_msg (void)
{
  return 0;
}

/** Big hack **/
int 
analyze_photo (void)
{
  return 0;
}

/** Big hack **/
int 
analyze_poly (void)
{
  return 0;
}
