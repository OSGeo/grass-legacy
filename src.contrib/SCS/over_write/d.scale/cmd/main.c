#include "gis.h"
#define MAIN
#include "options.h"

main(argc, argv)
     int argc ;
     char **argv ;
{
  char buff[128] ;
  char window_name[64] ;
  struct Cell_head window ;
  int i ;
  int t, b, l, r ;
  struct Option *opt1, *opt2, *opt3, *opt4 ;
  struct Flag *mouse ;

  mouse = G_define_flag() ;
  mouse->key        = 'm';
  mouse->description= "Use mouse to interactively place scale" ;

  opt1 = G_define_option() ;
  opt1->key        = "background" ;
  opt1->type       = TYPE_STRING ;
  opt1->answer     = "black" ;
  opt1->required   = NO ;
  opt1->options="red,orange,yellow,green,blue,indigo,violet,gray,brown,magenta,white,black,";
  opt1->description= "Color used for the background" ;

  opt2 = G_define_option() ;
  opt2->key        = "foreground" ;
  opt2->type       = TYPE_STRING ;
  opt2->answer     = "white" ;
  opt2->required   = NO ;
  opt2->options="red,orange,yellow,green,blue,indigo,violet,gray,brown,magenta,white,black,";
  opt2->description= "Color used for the offset" ;

  opt3 = G_define_option() ;
  opt3->key        = "coordinate";
  opt3->key_desc   = "x,y";
  opt3->type       = TYPE_STRING;
  opt3->answer     = "0.0,0.0";
  opt3->required   = NO;
  opt3->description= "the map E and N grid coordinates of a starting point";

  opt4 = G_define_option() ;
  opt4->key        = "units";
  opt4->type       = TYPE_STRING;
  opt4->required   = NO;
  opt4->description= "m(eters),f(eet)";

/* Initialize the GIS calls */
  G_gisinit(argv[0]);

  coord_inp = 0;

  if (G_parser(argc, argv) < 0)
	  exit(-1);

  color1 = D_translate_color(opt1->answer) ;

  color2 = D_translate_color(opt2->answer) ;

  units = 1;
  if (opt4->answer)
     if (strncmp(opt4->answer,"feet",1) == 0)  units = 0;

  G_scan_easting(opt3->answers[0], &easting, G_projection());
  coord_inp++;
  G_scan_northing(opt3->answers[1], &northing, G_projection());
  coord_inp++;

  R_open_driver();

  if (D_get_cur_wind(window_name))
    G_fatal_error("No current window") ;

  if (D_set_cur_wind(window_name))
    G_fatal_error("Current window not available");

  /* Read in the map window associated with window */
  G_get_window(&window);

  if (D_check_map_window(&window))
    G_fatal_error("Setting map window");

  if (G_set_window(&window) == -1) 
    G_fatal_error("Current window not settable");

  /* Determine conversion factors */
  if (D_get_screen_window(&t, &b, &l, &r))
    G_fatal_error("Getting screen window") ;
  if (D_do_conversions(&window, t, b, l, r))
    G_fatal_error("Error in calculating conversions") ;

  /* Draw the scale */
  draw_scale(color1,color2,easting,northing,coord_inp,mouse->answer,units) ;

  /* Add this command to list */
  strcpy(buff, argv[0]) ;
  for(i=1; i<argc; i++)
    {
      strcat(buff, " ") ;
      strcat(buff, argv[i]) ;
    }
  D_add_to_list(buff) ;

  R_close_driver();
}
