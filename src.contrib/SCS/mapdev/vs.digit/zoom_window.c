/*
*  Written by R.L. Glenn  4/1992
*  USDA Tech. Infor. Sys. Division
*/

#include "digit.h"
#include "graphics.h"
#include "wind.h"
#include "gis.h"
#include "popup.h"

#define SCALE_FACTOR 0.8
static char region_name[100];
static char region_mapset[100];

zoom_window ()
{
    int command, background_color, text_color, div_color;
    int	screen_x, screen_y ;
    int menu_left, menu_top, button;
    int zoom=1, ret, chr;
    char buff[60];
    double ux1, uy1 ;
    double ux2, uy2 ;
    double N, S, E, W;
    double tmp;
    struct Cell_head Window, temp_wind;

    menu_left = Next_l + 1;
    menu_top = Next_t;

    screen_x = screen_y = 1;
    while(zoom)
    {
    options[0] = "   Zoom Menu";
    options[1] = "";
    options[2] = "Select new window";
    options[3] = "Zoom In";
    options[4] = "Zoom Out";
    options[5] = "Specify new Window Center";
    options[6] = "Select window from database";   
    options[7] = "Restore previous window"; 
    options[8] = "";
    options[9] = "Return to Main menu";
    options[10] = '\0';

    background_color = D_translate_color(BC_MAIN) ;
    text_color      = D_translate_color(TC_MAIN) ;
    div_color       = D_translate_color(DC_MAIN) ;

    ret = popup_menu(
		    background_color,
		    text_color,
		    div_color,
		    menu_top,
		    menu_left,
		    MEN_SIZE,
		    "zoom",
		    _zoom
		    );
   if (_zoom) _zoom = 0;

	   switch (ret) {
		case 2:           /* save the existing window */
		       if (G__get_window (&Window, "", "WIND", G_mapset()) < 0)
			  {      /* current window failed, use default window */
			  G_get_default_window (&Window);
		   	  G_put_window (&Window);
			  }
		       G_copy (&temp_wind, &Window, sizeof(Window));
		       wind_savd = 1;
				       /* select New window */
		       set_window_w_mouse ();
		       clear_window ();
		       replot(CM); 
                       Dchoose(MEN.name) ;
		       break ;
		case 3:                     /* ZOOM IN */
                       Dchoose(DIG.name) ;
                       W = U_west  + (U_east - U_west)   * (1. - SCALE_FACTOR);
                       E = U_east  - (U_east - U_west)   * (1. - SCALE_FACTOR);
                       S = U_south + (U_north - U_south) * (1. - SCALE_FACTOR);
                       N = U_north - (U_north - U_south) * (1. - SCALE_FACTOR);

	               window_rout (N, S, E, W);
                       clear_window ();
                       replot(CM); 
                       Dchoose(MEN.name) ;
		       break;
	        case 4:                     /* ZOOM OUT */
                       Dchoose(DIG.name) ;
                       W = U_west  - (U_east - U_west)   * (1. - SCALE_FACTOR);
                       E = U_east  + (U_east - U_west)   * (1. - SCALE_FACTOR);
                       S = U_south - (U_north - U_south) * (1. - SCALE_FACTOR);
                       N = U_north + (U_north - U_south) * (1. - SCALE_FACTOR);

	               window_rout (N, S, E, W);
                       clear_window ();
                       replot(CM);
                       Dchoose(MEN.name) ;
	               break ;
	        case 5:                    /* PAN */
	               sprintf(buff, "Select new window center") ;
                       message[0] = (char *) malloc (strlen (buff) + 1);
                       sprintf(message[0],"%s", buff);
                       message[1] = '\0';

                       Dchoose(MEN.name) ;
                       popup_messg( "info", 1) ;
                       Dchoose(DIG.name) ;
	               R_get_location_with_pointer (&screen_x, &screen_y, &button);
	               flush_keyboard (); /*ADDED*/
	               erase_popup("info");
		       screen_to_utm ( screen_x, screen_y, &ux1, &uy1) ;
		       tmp =  (ux1 - ((U_east + U_west) / 2));
		       W = U_west + tmp;
		       E = U_east + tmp;

		       tmp =  (uy1 - ((U_north + U_south) / 2));
		       S = U_south + tmp;
		       N = U_north + tmp;

                       clear_window ();
	               window_rout (N, S, E, W);
	               replot(CM);
                       Dchoose(MEN.name) ;
		       break;
		case 6:                   /* save the existing window */
		       {
		       register int ret;
		       if (G__get_window (&Window, "", "WIND", G_mapset()) < 0)
			  {      /* current window failed, use default window */
			  G_get_default_window (&Window);
		   	  G_put_window (&Window);
			  }
		       G_copy (&temp_wind, &Window, sizeof(Window));
		       wind_savd = 1;
		       ret = ask_region();
		       if (ret)
			 {
  	                 if (G__get_window (&Window, "windows", region_name, region_mapset) < 0)
                           {
	                   fprintf(stderr,"can't read region <%s> in <%s>", region_name, region_mapset);
                           sleep(5);
                           break;
                           }
                         clear_window ();
	                 window_rout (Window.north, Window.south, Window.east, Window.west);
	                 replot(CM);
  			 G_clear_screen();    
                         Dchoose(MEN.name) ;
			 }
		       }
		       break;
		case 7:
		      if (wind_savd)
			 {
		         G_copy (&Window, &temp_wind, sizeof(temp_wind));
                         clear_window ();
	                 window_rout (Window.north, Window.south, Window.east, Window.west);
	                 replot(CM);
                         Dchoose(MEN.name) ;
			 }
		       break;
		case 9:
		       zoom = 0;
		       break;
		default:
		       break ;
	   } 
		
    } /* end while */

    return (0);
}

ask_region ()
{
    char buf[1024];
    char *mapset;

    sprintf(buf,"Terminal input required");
    message[0] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[0],"%s", buf);
    message[1] = '\0';

    Dchoose(MEN.name) ;
    popup_messg( "region", 1) ;
   
    G_clear_screen();
    mapset = G_ask_old ("", region_name, "windows", "window");
    erase_popup("region");
    if (mapset == NULL) return (0);

    strcpy (region_mapset, mapset);

    N_region = region_name;
    N_region_mapset = mapset;
    sprintf (buf, "%s/%s/%s", N_path, "windows", N_region);
    N_region_file = G_store (buf); 
    got_region = 1;

    return (1);
}
