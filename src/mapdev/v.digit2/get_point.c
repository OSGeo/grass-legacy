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
    int button;

    _Clear_base ();
    /*
    _Clear_info ();
    */
    if (D_cursor_buttons())
    {
	_Write_base (13, "Press any key on DIGITIZER:");
	Write_base (14, str);
	button = get_digitizer_button_xy (x, y);
	return (button);
    }
    else
    {
	char buf[BUFSIZ];

	_Write_base (13, str);
	Write_base (14, "Press ENTER to collect point:");
	Get_curses_text (buf);
	if (*buf == 'q' || *buf == 'Q')
	    return (-2);
	_coll_a_pnt (x, y);
	return (-1);
    }
}
