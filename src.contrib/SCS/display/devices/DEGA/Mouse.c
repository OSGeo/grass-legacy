/* Functions: Get_location_w_pointer, Get_location_w_box, Get_location_w_line

   P. W. Carlson	5/89 */

#include "ega_io.h"

static struct {			/* ioctl structure */
    int x;			/* x coordinate    */
    int y;			/* y coordinate    */
    int button;			/* button pressed  */
    int status;			/* status: 0 = disabled, 1 = enabled */
} mouse_data;

static int mouse_set_cmd;	/* ioctl command to display initial
                                   position of xhair, box, or line */
static int mouse_mov_cmd;	/* ioctl command to erase and redisplay
                                   xhair, box, or line */
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
    oldx = args.arg3 = mouse_data.x = *float_x;
    oldy = args.arg4 = mouse_data.y = *float_y;

    /* set the ioctl commands for xhair */
    mouse_set_cmd = EGA_SETXHAIR;
    mouse_mov_cmd = EGA_XHAIR;

    /* activate the mouse */
    do_mouse(float_x, float_y, button);
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
    args.arg1 = fixed_x;
    args.arg2 = fixed_y;
    oldx = args.arg3 = mouse_data.x = *float_x;
    oldy = args.arg4 = mouse_data.y = *float_y;

    /* set the ioctl commands for box */
    mouse_set_cmd = EGA_SETRBOX;
    mouse_mov_cmd = EGA_RUBBOX;

    /* activate the mouse */
    do_mouse(float_x, float_y, button);
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
    args.arg1 = fixed_x;
    args.arg2 = fixed_y;
    oldx = args.arg3 = mouse_data.x = *float_x;
    oldy = args.arg4 = mouse_data.y = *float_y;

    /* set the ioctl commands for line */
    mouse_set_cmd = EGA_SETRLINE;
    mouse_mov_cmd = EGA_RUBLINE;

    /* activate the mouse */
    do_mouse(float_x, float_y, button);
}


static do_mouse(float_x, float_y, button)
int *float_x, *float_y, *button;
{

    /* display initial position of xhair, box, or line */
    args.arg5 = 1;
    ioctl(egafd, mouse_set_cmd, &args);

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
	    oldx = args.arg3 = mouse_data.x;
    	    oldy = args.arg4 = mouse_data.y;
    	    ioctl(egafd, mouse_mov_cmd, &args);
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
    args.arg5 = 0;
    ioctl(egafd, mouse_mov_cmd, &args);
}
