/*
** Written by Bill Brown, Fall 1992
** Modified for X Terry Baker Spring 1993
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/

#include "digit.h"
#include "color.h"

double distance();


/***********************************************************************/
    
pull_it()
{
short val;
long dev;
long *screen_buf;
double  px, py, vect[2];
int i;
static int first=1;
int screen_x, screen_y;
int button;
int npoints = 0;
int xmin, ymin, xmax, ymax;
int tmpx, tmpy;

    redisplay_current_edit();
    check_changes();
    show_select_dialog("accept", "abort", "Pull line", 1);

    standard_color (dcolors[XD_WHITE]);

    tmpx = tmpy = 0;
    vect[0] = vect[1] = 0.0;    
    display_plseg (CurrPL, XD_WHITE);
    display_anchors (Anchor);
	while (TRUE)
	{
	action = Check_for_action(&screen_x, &screen_y);
	if (action)
	    switch (action) {
		case FIND:
		case DRAW:
			Editing = 1;
			if(first){
			    tmpx = screen_x;
			    tmpy = screen_y;
	    		    screen_to_utm (screen_x, screen_y, &px, &py);
			    
			    if (XmToggleButtonGetState (Afillfunc4))
			    {
				fill_pbuf_arc2(px, py); 
			    }
			    else if(!set_pull_point(px, py)) 
			    {
			    /* only put up error message on button release */
				if (action == FIND)
				{
			    	    ringbell();
				    make_monolog(1,
				        "Error selecting pull point.");
				}
				break;
			    }
			    first = 0;
			    PLtop = copy_pbuf_tolist (PLtoken);
			    get_corners (PLtop, &xmin, &ymin, &xmax, &ymax);
			}
	    		screen_to_utm (screen_x, screen_y, &px, &py);
		        vect[0] = px - Pullpoint[0];
			vect[1] = py - Pullpoint[1];

			copy_pix();
				  
			npoints = display_pullseg(vect, gc, xbuf);
		    break;

		case  ACCEPT:
		    if (first)
		        return(0);
		    recalc_pullbuf (vect);
		    first = 1;
		    if (Numppts <= 0)
		        return(0);
		    
		    empty_pl (PLtoken, CurrPL);
		    CurrPL = copy_pbuf_tolist (PLtoken);
		    empty_pl (PLtoken, PLtop);
		    PLtop = copy_pl (PLtoken, CurrPL);
		    
		    display_anchors (Anchor);
		    Editing = 0;
		    EditChanges = 1;
		    return(1);
		    break;
		case DONE:
		    if (first)
		        return(0);
		    first = 1;
		    erase_pullpoint (Pullpoint);
			
		    copy_pix();
		    empty_pl (PLtoken, CurrPL);
		    empty_pl (PLtoken, PLtop);
		    fill_pbuf_orig ();
		    CurrPL = copy_pbuf_tolist (PLtoken);
		    PLtop = copy_pl (PLtoken, CurrPL);
		    display_plseg (CurrPL, XD_WHITE);
		    display_anchors (Anchor);
		    Editing = 0;
		    return(1);
		    break;
	    }
    }

}

/***********************************************************************/
/***********************************************************************/
