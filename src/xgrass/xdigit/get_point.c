#include "digit.h"

get_point (x, y, str)
    double *x, *y;
    char *str;
{
    if (Point_Device == DIGITIZER)
	return new_point_with_digitizer (x, y, str);
    else
    {
	new_point_with_mouse (x, y, str);
	if (0.0 == *x && 0.0 == *y)
	    return -2;
	else
	    return -1;
    }
}

/* get an x,y pair from digitizer AFTER user presses a button
** if D_cursor_buttons() then wait for any cursor key pad press
**  otherwise, prompt for them to press ENTER and then get point
**
** returns the key pressed (0-9?)  or -1 for ENTER -2 for 'q' ENTER 
*/
new_point_with_digitizer (x, y, str)
    double *x, *y;
    char *str;
{
    int button, i;
    char buf[80];

#ifdef CURSORKEYS
    if (D_cursor_buttons())
#endif
    {
	i = D_start_button();
	sprintf (buf, "Press any key <%d> - <%d>:", i, i+4);
	make_monolog(0, buf);
	button = get_digitizer_button_xy (x, y);
	end_message();
	return (button);
    }
}
