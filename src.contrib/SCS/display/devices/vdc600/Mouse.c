/* Functions: Get_location_w_pointer, Get_location_w_box, Get_location_w_line

   P. W. Carlson	1/90 */

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

#define XHAIR	1
#define RUBBOX	2
#define RUBLINE	3

static struct {			/* ioctl structure */
    int x;			/* x coordinate    */
    int y;			/* y coordinate    */
    int button;			/* button pressed  */
    int status;			/* status: 0 = disabled, 1 = enabled */
} mouse_data;

static int x_fixed, y_fixed;	/* fixed coords. of box or line */
static int oldx, oldy;		/* x and y coordinates before move */

Get_location_with_pointer(float_x, float_y, button)
int *float_x, *float_y, *button;
{
    /* keep xhair on screen */
    if (*float_x < 0) *float_x = 0;
    else if (*float_x >= H_RES) *float_x = H_RES - 1;
    if (*float_y < 0) *float_y = 0;
    else if (*float_y >= V_RES) *float_y = V_RES - 1;

    /* put the coordinates in the ioctl structures */
    oldx = mouse_data.x = *float_x;
    oldy = mouse_data.y = *float_y;

    /* activate the mouse */
    do_mouse(XHAIR, float_x, float_y, button);
}


Get_location_with_box(fixed_x, fixed_y, float_x, float_y, button)
int fixed_x, fixed_y, *float_x, *float_y, *button;
{
    /* keep the box on the screen */
    if (fixed_x < 0) fixed_x = 0;
    else if (fixed_x >= H_RES) fixed_x = H_RES - 1;
    if (fixed_y < 0) fixed_y = 0;
    else if (fixed_y >= V_RES) fixed_y = V_RES - 1;
    if (*float_x < 0) *float_x = 0;
    else if (*float_x >= H_RES) *float_x = H_RES - 1;
    if (*float_y < 0) *float_y = 0;
    else if (*float_y >= V_RES) *float_y = V_RES - 1;

    /* put the coordinates in the ioctl structures */
    x_fixed = fixed_x;
    y_fixed = fixed_y;
    oldx = mouse_data.x = *float_x;
    oldy = mouse_data.y = *float_y;

    /* activate the mouse */
    do_mouse(RUBBOX, float_x, float_y, button);
}


Get_location_with_line(fixed_x, fixed_y, float_x, float_y, button)
int fixed_x, fixed_y, *float_x, *float_y, *button;
{
    /* keep the line on the screen */
    if (fixed_x < 0) fixed_x = 0;
    else if (fixed_x >= H_RES) fixed_x = H_RES - 1;
    if (fixed_y < 0) fixed_y = 0;
    else if (fixed_y >= V_RES) fixed_y = V_RES - 1;
    if (*float_x < 0) *float_x = 0;
    else if (*float_x >= H_RES) *float_x = H_RES - 1;
    if (*float_y < 0) *float_y = 0;
    else if (*float_y >= V_RES) *float_y = V_RES - 1;

    /* put the coordinates in the ioctl structures */
    x_fixed = fixed_x;
    y_fixed = fixed_y;
    oldx = mouse_data.x = *float_x;
    oldy = mouse_data.y = *float_y;

    /* activate the mouse */
    do_mouse(RUBLINE, float_x, float_y, button);
}


static do_mouse(action, float_x, float_y, button)
int action, *float_x, *float_y, *button;
{

    /* display initial position of xhair, box, or line */
    switch (action)
    {	case XHAIR:	setxhair(*float_x, *float_y);  break;
	case RUBBOX:	setrbox (x_fixed, y_fixed, *float_x, *float_y);  break;
	case RUBLINE:	setrline(x_fixed, y_fixed, *float_x, *float_y);  break;
    }

    /* initialize the mouse */
    mouse_data.button = 0;
    ioctl(mousfd, MOUSE_INIT, &mouse_data);

    /* loop until a button is pressed */
    while (!mouse_data.button)
    {	
	/* read the mouse */
    	mouse_data.status = 0;
    	ioctl(mousfd, GET_MOUSE, &mouse_data);
	while (mouse_data.status == 0);

	/* if the mouse has moved... */
	if (mouse_data.x != oldx || mouse_data.y != oldy)
    	{   
	    /* check the screen boundaries */
    	    if (mouse_data.x >= H_RES) mouse_data.x = H_RES - 1;
    	    if (mouse_data.y >= V_RES) mouse_data.y = V_RES - 1;

	    /* erase and redisplay the xhair, box, or line */
	    oldx = *float_x = mouse_data.x;
    	    oldy = *float_y = mouse_data.y;
    	    switch (action)
    	    {	case XHAIR:	xhair  (*float_x, *float_y, 1);  break;
		case RUBBOX:	rubbox (*float_x, *float_y, 1);  break;
		case RUBLINE:	rubline(*float_x, *float_y, 1);  break;
    	    }
	}
    }

    /* get the coordinates and button pressed */
    if (mouse_data.x >= H_RES) mouse_data.x = H_RES - 1;
    if (mouse_data.y >= V_RES) mouse_data.y = V_RES - 1;
    *float_x = mouse_data.x;
    *float_y = mouse_data.y;
    *button = mouse_data.button;

    /* debounce the mouse */
    while (mouse_data.button) ioctl(mousfd, GET_MOUSE, &mouse_data);

    /* remove the xhair, box, or line */
    switch (action)
    {	case XHAIR:	xhair  (*float_x, *float_y, 0);  break;
	case RUBBOX:	rubbox (*float_x, *float_y, 0);  break;
	case RUBLINE:	rubline(*float_x, *float_y, 0);  break;
    }
}
