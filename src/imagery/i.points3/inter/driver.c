/*=======================================================================
				i.points
  driver.c --
    void driver(void)

       The main menu for the i.points program.  "objects" is an array
       of  type Objects which define the menu options and text.
       Input_pointer() is called with objects.  See "input.c" 

       The main menu contains the following options:
       
           QUIT      -  really_quit()       contained internally
           ZOOM      -  zoom()              contained internally
	   PLOT      -  plot()              contained internally
	   MARK      -  mark_control()              mark_control.c
	   ANALYZE   -  analyze_control()           anal_control.c
	   OVERLAY   -  overlay()           contained internally
	   TRANS     -  transform()         contained internally

       The routine Input_pointer() loops untill the select option
       returns a non-zero status.  That is it quits only when the 
       selected routine returns a non-zero.

=======================================================================*/


#include "globals.h"

         /* internal function prototypes */
#ifdef _NO_PROTO
   int    zoom();
   static plot();
   static Overlay();
   static transform();
   static really_quit();
   static dont_stop();
   static stop();
   static cancel();
#else
   int zoom(void);
   static int plot(void);
   static int Overlay(void);
   static int transform(void);
   static int really_quit(void);
   static int dont_stop(void);
   static int stop(void);
   static int cancel(void);
#endif


static int use = 1;
static int use_cell = 1;
static int use_vect = 1;


/*---------------------------------------------------------------------*/
void
driver( )
{

  static Objects objects[] =
    {
      MENU("QUIT",really_quit,&use),
      TITLE("<MAIN>",   &use),
      MENU("ZOOM",      zoom,&use),
      MENU("PLOT",      plot,&use),
      MENU("MARK",      mark_control, &use), 
      MENU("ANALYZE",   anal_control,&use), 
      MENU("OVERLAY",   Overlay,&use), 
/*      MENU("AUXIL",     transform,&use), */
      INFO("Select Function.", &use),
      {0}
    };

  
  /* loop continuiously untill zero return status      */
  /* zero only occurs by calling "really_quit -> yes"  */
  Input_pointer (objects);
  Menu_msg ("");

}


/*---------------------------------------------------------------------*
 *   int zoom (void)
 *       The zoom menu.
 *
 *           CANCEL    - contained internally
 *           other     - zoom_which()
 *
 *       OPTIONS - set use_zoom_box or use_xoom_pnt
 *           BOX       - zoom_box.c
 *	     POINT     - zoom_point.c
 *
 *       The routine Input_pointer loops untill the select option
 *       retuns a non-zero status.
 *
 *       RETURNS:
 *          0     - always so that main driver() does NOT quit
 *---------------------------------------------------------------------*/
int zoom()
{
  static int which_zoom();
  
  static Objects objects[]=
    {
      MENU("CANCEL",cancel,&use),
      TITLE("<ZOOM>",&use),
      INFO("Current ZOOM Type.",&use),
      OPTION("BOX",   2, &use_zoom_box),
      OPTION("POINT", 2, &use_zoom_pnt),
      OTHER(which_zoom, &use),
      {0}
    };
  
  Input_pointer (objects);
  return 0;	/* return, but don't QUIT */
}

static int 
which_zoom(int x,int y,int button)
{

  /* Button one to set point, Button 2 & 3 return location */
  if (button != 1)
    return where (x,y);


  if (use_zoom_box == 1)
     zoom_box(x,y);
  else zoom_point(x,y);
  
  return 0;
}


/*---------------------------------------------------------------------*
 *    static int plot (void)
 *       The plot menu.
 *
 *           CANCEL     - contained internally
 *           IMAGERY    - plotimg()      plot.c
 *	     RASTER     - plotcell()     plotcell.c
 *	     VECTOR     - plotvect()     plot_vect.c
 *	     REFRESH    - plot_refresh() view_refresh.c
 *	     CLEAR      - plot_clear()   view_clear.c
 *
 *       The routine Input_pointer loops untill the select option
 *       retuns a non-zero status.
 *
 *       RETURNS:
 *          0     - always so that main driver() does NOT quit
 *---------------------------------------------------------------------*/
static int plot()
{
    static Objects objects[] =
    {
	MENU ("CANCEL", cancel, &use),
	TITLE("<PLOT>", &use),
	MENU ("IMAGERY",plotimg, &use),
	MENU ("RASTER",plotcell, &use_cell), 
	MENU ("VECTOR",plotvect, &use_vect), 
	MENU ("REFRESH", plot_refresh, &use), 
	MENU ("CLEAR",   plot_clear, &use), 
	INFO ("Select Plot Function.", &use),
	{0}
    };

    Input_pointer (objects);
    return 0;     /* so that Input_pointer in driver() doesnt quit */
}


/*---------------------------------------------------------------------*
 *    static int overlay (void)
 *       The overlay menu.
 *
 *           CANCEL      - contained internally
 *           VECTORS     - _warp_vect()   overlay.c
 *           GRID        - _warp_grid()   overlay.c
 *           TIN         - NOT USED
 *           REFRESH     - plot_refresh() view_refresh.c
 *
 *       The routine Input_pointer loops untill the select option
 *       retuns a non-zero status.
 *
 *       RETURNS:
 *          0     - always so that main driver() does NOT quit
 *---------------------------------------------------------------------*/
static int Overlay()
{
    static int use = 1;

    static Objects objects[] =  {
      MENU ("CANCEL", cancel, &use),
      TITLE("<OVERLAY>", &use),
      MENU ("VECTORS", _warp_vect, &use),
      MENU ("GRID",    _warp_grid, &use),
      /*** MENU ("TIN",     _warp_vect, &use), ***/
      MENU ("REFRESH", plot_refresh, &use),
      INFO ("Select Overlay Function", &use),
      {0}
    };

    Input_pointer(objects);
    return 0;
}

/*---------------------------------------------------------------------*
 *    static int transform (void)
 *       The transformation options menu.
 *
 *           CANCEL      - contained internally
 *           POLYNOMIAL  - get_order()   transform.c
 *           ORTHO_PHOTO - ortho_photo() transform.c
 *           LANDSAT TM  - landsat_tm()  transform.c
 *
 *       The routine Input_pointer loops untill the select option
 *       retuns a non-zero status.
 *
 *       The routines "get_order", "ortho_photo", and "landsat_tm"
 *           allow a user to provide transformation related info.
 *       After returning from any of these functions.  This routine
 *           reinitializes the transformation info, gets any of the
 *           auxillary data, and recalcualetes the parameters.
 *
 *       RETURNS:
 *          0     - always so that main driver() does NOT quit
 *---------------------------------------------------------------------*/
static int transform()
{
  char msg[80]; /* error message */
  int  status = 1;

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
      /** MENU ("POLYNOMIAL", get_poly_order, &use),  **/
      MENU ("ORTHO_PHOTO",ortho_photo,&use),
      MENU ("LANDSAT TM",  landsat_tm,&use),
      /** MENU ("PROJECTION", get_poly_order, &use),  /** TODO **/
      INFO ("Select Transformation Type", &use),
      {0}
    };


/*************/
/**   do {  **/
/*************/
 
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

#ifdef NULL
/**      /* is something went wrong dont run transformation */
/**       if (status != 1) {
/**           sprintf (msg, "Problem selecting  %s transformation, select another\n",
/**		trans_text[group.trans_type]);
/**           G_warning (msg);
/**	   goto skip_trans;
/**       }
**/
#endif


      /* do an initial calculation of the transformation parameters */
       if (status = group.calculate_trans (&group) < 0 ) {
           sprintf (msg, "Problem running %s transformation, select another\n",
		trans_text[group.trans_type]);
           G_warning (msg);
       }
      
/********************************/
/**  } while (group.stat < 0); **/
/********************************/

  return 0;
}



/*---------------------------------------------------------------------*
 *    static int really_quit (void)
 *       The transformation options menu.
 *
 *           CANCEL      - cancel()     contained internally
 *           NO          - dont_stop()  contained internally
 *           YES         - stop()       contained internally
 *
 *        RETURNS:
 *           -1     - if users chooese to quit
 *            0     - otherwise
 *---------------------------------------------------------------------*/
static int really_quit()
{
    static Objects objects[] =
    {
	TITLE("<QUIT>",&use),
	INFO("Really Quit? ",&use),
	MENU("NO",dont_stop,&use),      /* returns  1 */
	MENU("YES",stop,&use),          /* returns -1 */
	{0}
    };
    if (Input_pointer (objects) < 0)
	return -1;   /* Yes, have main driver Input_pointer() quit */
    return 0; /* don't quit */
}

/*---------------------------------------------------------------------*/
static int dont_stop()
{
    return 1;
}

/*---------------------------------------------------------------------*/
static int stop()
{
    return -1;
}


/*---------------------------------------------------------------------*
 *    static int cancel (void)
 *
 *        Returns non-zero number to the calling Input_pointer routine.
 *
 *        RETURNS:
 *            1     - allways
 *---------------------------------------------------------------------*/
static int cancel()
{
    return 1;  
}
