#include "globals.h"
#include <string.h>
#include "crs.h"

static int use = 1;

/* internal prototypes */
int get_poly_order();
int ortho_photo();
int landsat_tm();

static int cancel(void);
static int do_1st(void);
static int do_2nd(void);
static int do_3rd(void);
static int do_op(void);
static int do_tm(void);

int choose_elev_photo(); 
int setup_camera(); 
int setup_expose();

int choose_elev_tm(); 
int setup_landsat(); 

/** mbaba -- clled from analyze option */
int ask_transform()
{
  static int use_p1, use_p2, use_p3, use_op, use_tm;

  char msg[80]; /* error message */
  static char *trans_text[7] = {" ",
                              "Polynomial_1",
                              "Polynomial_2",
                              "Polynomial_3",
                              "Ortho_Photo",
                              "LandSat_TM",
                              "" };
 
  static Objects objects[] =
    {
      MENU ("CANCEL", cancel ,&use),
      TITLE("<TRANS>", &use),
      MENU ("POLY 1",     do_1st,  &use_p1),
      MENU ("POLY 2",     do_2nd,  &use_p2),
      MENU ("POLY 3",     do_3rd,  &use_p3),
      MENU ("ORTHO_PHOTO",do_op,   &use_op),
      MENU ("LANDSAT TM",  do_tm,   &use_tm),
      INFO ("Select Transformation", &use),
      {0}
    };
  
  
  /* determine which transformations can be used */
  use_p1 = use_p2 = use_p3 = use_op = use_tm = 1;

   do {
 
       Input_pointer (objects);
       Menu_msg ("");
 
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

      /* get the groups auxilary data */
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

      /* is something went wrong dont run transformation */
/**       if (status != 1) {
**           sprintf (msg, "Problem selecting  %s transformation, select another\n",
**		trans_text[group.trans_type]);
**           G_warning (msg);
**	   goto skip_trans;
**       }
**/


      /* do an initial calculation of the transformation parameters */
       if (group.stat = group.calculate_trans (&group) < 0 ) {
           sprintf (msg, "Problem running %s transformation, select another\n",
		trans_text[group.trans_type]);
           G_warning (msg);
       }
      
  } while (group.stat < 0); 

  return 0;
}

/******************************************************************/
int get_poly_order()
{

  static Objects objects[]=
    {
      MENU ("CANCEL", cancel ,&use),
      TITLE("<TRANS-POLY>", &use),
      MENU ("POLY 1", do_1st, &use),
      MENU ("POLY 2", do_2nd, &use),
      MENU ("POLY 3", do_3rd, &use),
      INFO ("Select order of polynomial transformation", &use),
      {0}
    };

   if(Input_pointer(objects) < 0)
      return -1;

   return 1;
}

static int do_1st() {
   strcpy(order_msg,"POLY1");
   group.trans_type = POLY1;
   I_put_group_trans(&group);

   /* initialize the transformation functions for the group */
   if (I_init_group_trans_funcs (&group) < 0) {
     G_fatal_error ("Can't initialize group transformation functions\n");
   }

   return 0;
}

static int do_2nd() {
   strcpy(order_msg,"POLY2");
   group.trans_type = POLY2;
   I_put_group_trans(&group);

   /* initialize the transformation functions for the group */
   if (I_init_group_trans_funcs (&group) < 0) {
     G_fatal_error ("Can't initialize group transformation functions\n");
   }

   return 0;
}

static int do_3rd() {
   strcpy(order_msg,"POLY3");
   group.trans_type = POLY3;
   I_put_group_trans(&group);

   /* initialize the transformation functions for the group */
   if (I_init_group_trans_funcs (&group) < 0) {
     G_fatal_error ("Can't initialize group transformation functions\n");
   }

   return 0;
}

static int do_op() {
   strcpy(order_msg,"PHOTO");
   group.trans_type = PHOTO;
   I_put_group_trans(&group);

   /* initialize the transformation functions for the group */
   if (I_init_group_trans_funcs (&group) < 0) {
     G_fatal_error ("Can't initialize group transformation functions\n");
   }

   return 0;
}

static int do_tm() {
   strcpy(order_msg,"LAND_TM");
   group.trans_type = LAND_TM;
   I_put_group_trans(&group);

   /* initialize the transformation functions for the group */
   if (I_init_group_trans_funcs (&group) < 0) {
     G_fatal_error ("Can't initialize group transformation functions\n");
   }

   return 0;
}


/******************************************************************/
int ortho_photo()
{
  static Objects objects[] =
    {
      MENU("CANCEL", cancel ,&use),
      TITLE("<TRANS-PHOTO>", &use),
      MENU("ELEVATION",   choose_elev_photo, &use),
      MENU("CAMERA",      setup_camera,&use),
      MENU("MARK FID",    mark_fiducial,&use),
      MENU("ANAL FID",    anal_fiducial,&use),
      MENU("EXPOSURE",    setup_expose,&use),
      INFO("Select Ortho Photo Function", &use),
      {0}
    };
  
    /* set the initial transformation routine for ortho photo */
    do_op();

    /* get the groups auxilarry data for ortho photo trans  */
    /* turn off warnings about file not existing yet. */
    G_suppress_warnings(1); 

    if (group.get_auxil != NULL) {
      if (group.get_auxil (&group) <0 ) {
	G_fatal_error ("Can't get auxilary data for group\n");
      }
    }

    /* turn warning back on */
    G_suppress_warnings(0); 


    Input_pointer (objects);
    Menu_msg ("");
    return 0;
}



/*======================================================================
			  choose_elev_photo

Prompt user for elevation file to use for ortho-rectification.  Store in
group file ELEVATION.
======================================================================*/
int choose_elev_photo() {
    int result;
    GrassEnv old_env;
    
    Auxillary_Photo  *auxil;


    /* make visiable */
    auxil = (Auxillary_Photo *) group.auxil;

    old_env = new_environment(TargetEnv);
    /**    select_target_env(); **/

    I_initialize_group_elev(&auxil->elev);
    Menu_msg("Enter elevation map name in terminal window.");
    result = ask_elevation(group.name, auxil->elev.elev_map,
      auxil->elev.elev_mapset, auxil->elev.units);

    new_environment(old_env);
    /**    select_current_env(); **/

    /* save if vailid result */
    if (result == 0)
       I_put_group_elev(group.name, auxil->elev);

    Menu_msg("");
    return 0;
}

int setup_camera() {
/* TODO */
    Auxillary_Photo  *auxil;
    int have_old;
    int result;

    /* make visiable */
    auxil = (Auxillary_Photo *) group.auxil;

    Menu_msg("Enter camera data in terminal window.");

    /* turn off curses to get user input */
    /** End_curses(); **/

    /* show the camera infor for modification */
    if (I_find_camera(group.name))
          {  have_old = 1;
             I_get_group_camera (group.name, &auxil->camera);  
          }


    result = mod_cam_info(have_old, &auxil->camera);

    if (result == 0)
       I_put_group_camera(group.name, &auxil->camera);

    /* now resume curses stuff */
    /** Resume_curses(); **/

    return 0;
}

int setup_expose() {
  Auxillary_Photo  *auxil;
  int have_old;
  int result;

    /* make visiable */
    auxil = (Auxillary_Photo *) group.auxil;

    /* turn off curses to get user input */
    /** End_curses(); **/

    /* get initial camera exposure infor */ 
    if (I_find_initial(group.name))
    {  have_old = 1;
       I_get_group_expose (group.name, &auxil->camera_expose); 
    } 

    Menu_msg("Enter initial exposure information in terminal window.");

    result = mod_init_info(have_old, &(auxil->camera_expose));

    /* save info */
    if (result == 0)
       I_put_group_expose (group.name, &auxil->camera_expose);

    /* now resume curses stuff */
    /** Resume_curses(); **/

    return 0;
}



/******************************************************************/
int landsat_tm()
{

  static Objects objects[] =
    {
      MENU("CANCEL", cancel ,&use),
      TITLE("<TRANS-PHOTO>", &use),
      MENU("ELEVATION",    choose_elev_tm, &use),
      MENU("LANDSAT PAR.", setup_landsat,&use),
      INFO("Select Land_TM Function", &use),
      {0}
    };

  /* set the transformation */
  do_tm();

  /* get the groups auxilarry data for ortho photo trans  */
  /* turn off warnings about file not existing yet. */
  G_suppress_warnings(1); 

  if (group.get_auxil != NULL) {
    if (group.get_auxil (&group) <0 ) {
      G_fatal_error ("Can't get auxilary data for group\n");
    }
  }
  
  /* turn warning back on */
  G_suppress_warnings(0); 
  
  Input_pointer (objects);
  Menu_msg ("");
  return 0;
}

/*======================================================================
			  choose_elev_tm

Prompt user for elevation file to use for ortho-rectification.  Store in
group file ELEVATION.
======================================================================*/
int choose_elev_tm() {
    int result;
    GrassEnv old_env;
    
    Auxillary_Ltm  *auxil;


    /* make visiable */
    auxil = (Auxillary_Ltm *) group.auxil;

    old_env = new_environment(TargetEnv);
    /**    select_target_env(); **/
    I_initialize_group_elev(&auxil->elev);
    Menu_msg("Enter elevation map name in terminal window.");
    result = ask_elevation(group.name, auxil->elev.elev_map,
      auxil->elev.elev_mapset, auxil->elev.units);
    new_environment(old_env);
    /**    select_current_env(); **/
    I_put_group_elev(group.name, auxil->elev);
    Menu_msg("");
    return 0;
}


int setup_landsat() {
    return 0;
}


/******************************************************************/

/* Polynomial */
int compute_transformation()
{
    return 0;
}


static int cancel() {
    return 1;
}
