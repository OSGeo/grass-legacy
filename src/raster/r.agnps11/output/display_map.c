
	/*---------------------------------------------------------*
	 *               AGNPS/GRASS Interface Project             *
	 *  Developed in the Agriculture Engineering Department    *
	 *                at Purdue University                     *
	 *                        by                               *
	 *         Raghavan Srinivasan and Bernard Engel           *
	 *                                                         *
	 *   (c)Copyright, 1992 Purdue Research Foundation, West   *
	 *   Lafayette, Indiana 47907. All Rights Reserved. Unless *
	 *   permission is granted, this material shall not be     *
	 *   copied, reproduced or coded for reproduction by any   *
	 *   electrical, mechanical or chemical processes,  or     *
	 *   combinations thereof, now known or later developed.   *
	 *---------------------------------------------------------*/

#include "map_gen.h"

int display_map(win_name, window, map_name,label, type)
char	*win_name, *map_name, *label;
int	type;
struct Cell_head window;
{
  char	*mapset, buf[512];
  int R_open_driver(), R_standard_color(), R_close_driver();
  int G_fatal_error(), G_set_window();
  int Dchoose(), D_translate_color(), Derase(), D_check_map_window(), Dcell();
  int d_number(), d_arrow(), win_label();
	

	mapset = G_find_file("cell",map_name, "") ;
	if (! mapset) {
                sprintf(buf,"%s Cell Map mapset not found to open",map_name);
                G_fatal_error(buf) ;
                }


	R_open_driver();

	Dchoose(win_name);

	R_standard_color(D_translate_color("black"));

        Derase("black") ;	

	G_set_window(&window);
	if (D_check_map_window(&window))
                G_fatal_error("Setting graphics window") ;

        Dcell(map_name,mapset,0);

	R_close_driver();

	if(type == 4) d_number();
	else if (type == 5) d_arrow(2);

	win_label(label);
        return 0;
}
