                        /********************************/
                        /*     r.le.setup/main.c   	*/
                        /*                              */
                        /*             2.1              */
                        /*                              */
                        /*       07/25/94 version       */
                        /*                              */
                        /*      Programmer: Baker       */
                        /*      Univ. of Wyoming        */
                        /********************************/

#include "setup.h"


				/* MAIN PROGRAM */

main(argc, argv)
int  argc;
char *argv[];
{

				/* setup a structure to store the window
				   parameters */

  struct Cell_head  window;
  int               bot, right, t0, b0, l0, r0, respond;
  double 	    Rw_l, Rscr_wl;
  char 		    tmp[20], *v_name=NULL, *s_name=NULL;

  char              name[20];
  char              map_name[20];
  char              info[20];
  char              *mapset;

  setbuf (stdout, NULL);	/* unbuffered */
  setbuf (stderr, NULL);
  G_gisinit(argv[0]); 

  G_sleep_on_error(0);


				/* get the user input */

  user_input(argc,argv) ; 

				/* query for the map to be setup */

  printf("\n") ;
  printf("OPTION   Raster map to use to setup sampling\n") ;
  printf("     key:map\n") ;
  printf("required:YES\n") ;
  printf("\n") ;
  mapset = G_ask_cell_old("enter raster map name: ",map_name) ;
  if (mapset == NULL) exit(0) ;


				/* query for a vector map overlay */

  printf("\n") ;
  printf("OPTION   Vector map to overlay.\n") ;
  printf("     key:vect\n") ;
  printf("required:NO\n") ;
  printf("\n") ;
  if ( G_yes("Overlay a vector file ?",0)) {
      G_ask_vector_old("enter vector file name to be overlaid: ",name) ;
      v_name = G_malloc(30);
      strcpy(v_name, name);
     }

				/* query for a site map overlay */

  printf("\n") ;
  printf("OPTION   Site map to overlay.\n") ;
  printf("     key:site\n") ;
  printf("required:NO\n") ;
  printf("\n") ;
 
   if ( G_yes("Overlay a site file ?",0)) {
      mapset=G_ask_sites_old("enter site map to overlay", info) ;
      if (mapset == NULL) exit(0) ;
      G_fopen_sites_old(info,mapset) ;
      s_name = G_malloc(30);
      strcpy(s_name, info);
   }


  get_pwd();

  				/* setup the current window for 
				   display */

  G_system(" d.colormode fixed");
  G_system(" d.frame -e");
  Rw_l = (double)G_window_cols()/G_window_rows();
  R_open_driver(); 
  R_font("romant");
  G_get_set_window(&window);
  t0 = R_screen_top();
  b0 = R_screen_bot();
  l0 = R_screen_left();
  r0 = R_screen_rite();
  Rscr_wl = (double)(r0 - l0) / (b0 - t0);

  if(Rscr_wl > Rw_l){
    bot = b0;
    right = l0 + (b0 - t0) * Rw_l; 
  } 

  else {
    right = r0;
    bot = t0 + (r0 - l0) / Rw_l; 
  }

  D_new_window("a", t0, bot, l0, right);
  D_set_cur_wind("a");
  D_show_window(D_translate_color("green"));
  D_setup(0);
  R_close_driver();

  				/* invoke the setup modules */

  set_map(map_name, v_name, s_name, window, t0, bot, l0, right);
}






				/* SETUP THE R.LE.PARA DIRECTORY */

void   get_pwd()
{
  DIR    *dp;

  if(!(dp = opendir("r.le.para")))      
     G_system("mkdir r.le.para");
  else 
     closedir(dp);
 
}
