#include <string.h>
#include "gis.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

static int max(int,int);

int make_window_box ( struct Cell_head *window, double magnify)
{
    char buffer[64] ;
    int screen_x, screen_y ;
    double px, py, ux1, uy1, ux2, uy2 ;
    double north,south,east,west, ns, ew;
    int button ;
    int cur_screen_x, cur_screen_y ;
    int quitonly;  /* required if user just wants to quit d.zoom */
    int prebutton; /* which previous button was pressed? */
    int mode; /* 1, 2 */
    int resetwin;
    struct Cell_head defwin;
    int printmenu = 1;
    int limit;
    
    G_get_default_window(&defwin);
    
    mode = 1;
    while (1) {
	resetwin = 0;
	if ( printmenu ) {
	    fprintf(stderr, "\n\nButtons:\n") ;
	    fprintf(stderr, "%s 1. corner\n", LBTN) ;
	    fprintf(stderr, "%s Unzoom\n", MBTN) ;
	    fprintf(stderr, "%s Main menu\n\n", RBTN) ;
	    printmenu = 0;
	}

	if ( mode == 1 ) {
            R_get_location_with_pointer(&screen_x, &screen_y, &button);
	    cur_screen_x = screen_x;
	    cur_screen_y = screen_y;
	}
	else
	    R_get_location_with_box(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;
	
	/* For print only */
	px = D_d_to_u_col((double)screen_x)  ;
	py = D_d_to_u_row((double)screen_y)  ;
        print_coor ( window, py, px );
	
	switch(button) {
	    case LEFTB:
		if ( mode == 1 ) {
	            fprintf(stderr, "\n\nButtons:\n") ;
		    fprintf(stderr, "%s 1. corner (reset)\n", LBTN) ;
		    fprintf(stderr, "%s 2. corner\n", MBTN) ;
		    fprintf(stderr, "%s Main menu\n\n", RBTN) ;
		}
		if ( mode == 2 ) {
		  cur_screen_x = screen_x ;
		  cur_screen_y = screen_y ;
		}
		mode = 2;
		break ;
	    case MIDDLEB:
		if ( mode == 1 ) { /* unzoom */
	            ux2 = D_d_to_u_col((double)screen_x)  ;
	            uy2 = D_d_to_u_row((double)screen_y)  ;
		    ew = (window->east - window->west)/magnify;
		    ns = (window->north - window->south)/magnify;

		    ux1 = window->east + ew/2;
		    ux2 = window->west - ew/2;
		    uy1 = window->north + ns/2;
		    uy2 = window->south - ns/2;
		} else {
	            ux1 = D_d_to_u_col((double)cur_screen_x)  ;
	            uy1 = D_d_to_u_row((double)cur_screen_y)  ;
	            ux2 = D_d_to_u_col((double)screen_x)  ;
	            uy2 = D_d_to_u_row((double)screen_y)  ;
	            printmenu = 1;
		    mode = 1;
	            fprintf(stderr, "\n") ;
		}
		resetwin = 1;
		break;
	    case RIGHTB:
		return 1;
		break;
	}
	if ( resetwin ) {
	    north = uy1>uy2?uy1:uy2 ;
	    south = uy1<uy2?uy1:uy2 ;
	    west  = ux1<ux2?ux1:ux2 ;
	    east  = ux1>ux2?ux1:ux2 ;
	    
            limit = 0;
	    if ( north > defwin.north ) {
	       /* north = defwin.north; */
	       fprintf(stderr, "\nNorth limit of region reached") ;
               limit = 1;
	    } 
	    if ( south < defwin.south ) {
	       /* south = defwin.south; */
	       fprintf(stderr, "\nSouth limit of region reached") ;
               limit = 1;
	    } 
	    if ( east > defwin.east ) {
	       /* east = defwin.east; */
	       fprintf(stderr, "\nEast limit of region reached") ;
               limit = 1;
	    } 
	    if ( west < defwin.west ) {
	       /* west = defwin.west; */
	       fprintf(stderr, "\nWest limit of region reached") ;
               limit = 1;
	    } 
	    if ( limit ) {
		printmenu = 1;
                fprintf(stderr, "\n\n") ;
	    }
	    
	    window->north = north;
	    window->south = south;
	    window->east  = east ;
	    window->west  = west ;
	    
	    print_win ( window, north, south, east, west );

	    G_put_window(window);
	    G_set_window(window);
	    redraw();

	}
    } ;

    return 1; /* not reached */
}


