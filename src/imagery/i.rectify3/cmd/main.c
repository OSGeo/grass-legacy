/*======================================================================
Filename: main.c
Moudle:   i.rectify3  (cmd)
Author:   Mike Baba


     Command version of i.rectify3

     Usage: i.rectify3 
              group=group 
              source=name
	      target=name

	      TODO
	      [trans=[poly1 | poly2 | poly3 | photo | landtm]]

	      TODO - make separte options
	      [region=n,s,e,w,ns_res,ew_res]

	      FLAGS:
	      -o   Overwrite target images
	      -c   Use current region in target location as region
	      -m   Calculate and use minimal target region 
	           (i.e. Minimal target region large enough to hold
		         entire rectified image)
	      -q   quit    
	      -v   verbose  

     NOTES:
	    Default action is to rectify each image of the group from the 
	    source location to the target location.  The default transformation
	    will be the last one selected in i.points3.  The default region
	    will be the current region in the TARGET location.  The default 
	    action for naming will be to use the original name with the
	    transformation type appended.

            The region parameter, -c, and -f flags are mutually exclusive.

	    The programs i.group, i.target, and i.points3  
	    must also be run prior to i.rectify3.

Modifications:
30 Oct 93    - mbaba       - original 


======================================================================*/


#define GLOBAL
#include <string.h>
#include "global.h"
#include "parse.h"

/** TODO -- elim crs.h **/
#include "crs.h" 
#include "protodefs.h"


/*-------------------------------------------------------------------*/
int main( int  argc, char *argv[])
{
  int n;                      /* group file counter */
  int ret;                    /* return code */
  tRect_Data   rect_data;     /* Hold rectification data (names, region)  */
  char *error;                /* Parse error message */


  /** TODO remove these **/
  /* was used for debuging */
  setbuf (stdout, NULL);
  setbuf (stderr, NULL);


  /* initialize GRASS */
  G_gisinit (argv[0]);

  /* Parse the arguments */
  if ( error = (char *) Parse(argc, argv, &rect_data))   {
    G_fatal_error (error);
  }
    

  /* get the target LOCATION infomation */
  /* needs to be done before getting the control points */
  strcpy (group.name, rect_data.group);
  get_target(group.name);


  /* Open the imagery group and get the refernce structure */
  if (!I_get_group_ref (group.name, &group.ref)) {
    G_fatal_error("Can't open imagery group %s.\n", group.name);
  }

  /* Check that the imagery group has files in it */
  if (group.ref.nfiles <= 0) {
    G_fatal_error("Imagery group [%s] contains no files\n", group.name);
  } 


  /* alloc a list of names for the rectified imagery */
  ref_list = (int *)   G_malloc (group.ref.nfiles * sizeof(int));
  new_name = (char **) G_malloc (group.ref.nfiles * sizeof(char *));
  for (n = 0; n < group.ref.nfiles; n++)
    ref_list[n] = -1;

  /* check the files to be rectified and target names */
  if (check_files (group.ref, &rect_data) < 0) {
    G_fatal_error("");
  }


  /* supprest warnings about getting files */
  G_suppress_warnings(1); 

  /* determine tranformation equation */
  ret = I_get_group_trans (&group);
  if (ret == -1 ) {          /* TRANS file does not exist */
    G_fatal_error ("No transformation file found for group %s.\n", group.name);
  }
  else if (ret == -2 ) {     /* BAD format of trans file */
    G_fatal_error ("Bad format in transformation file for group %s.\n", 
	     group.name);
  }

  /** TODO */
  /** Allow user to select transformation **/


  /* initialize the transformation functions for the group */
  if (I_init_group_trans_funcs (&group) < 0) {
    G_fatal_error ("Can't initialize transformation function for group %s.\n", 
	      group.name);
  }

  /* initialize the group control points data */
  if (group.get_points != NULL) {
    if (group.get_points (&group) <0 ) {
      G_fatal_error ("Can't get control points for group %s.\n",
		group.name);
    }
  }

  /* get the groups auxilarry data */
  if (group.get_auxil != NULL) {
    if (group.get_auxil (&group) <0 ) {
      G_fatal_error ("Can't get auxilary infomation for group %s.\n", 
		group.name);
    }
  }

  /* get the groups coeffs data */
  if (group.get_coefs != NULL) {
    if (group.get_coefs (&group) <0 ) {
      G_fatal_error ("Can't get transformation coeffient data for group %s.\n",
		group.name);
    }
  }


  /* do an initial calculation of the transformation parameters */
  if (! (group.calculate_trans (&group))) {
    G_fatal_error ("Can't perform a <%s> transformation for group %s.\n",
	     "TEST", group.name);
  }

  /* turn warnings messages back on */
  G_suppress_warnings(0); 


  /* set the target window to use for rectification */
  get_window(group.ref, &rect_data); 
  
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


int 
mark_control (void)
{
  return 0;
}

int 
Menu_msg (void)
{
  return 0;
}

int 
analyze_photo (void)
{
  return 0;
}


int 
analyze_poly (void)
{
  return 0;
}

