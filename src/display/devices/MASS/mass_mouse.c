/*
 * user button interrupt routine expects only the button number
 * must set up an intermediate routine which actually catches the
 * button interrupt and calls the users button routine with the
 * button number
 */
static int (*userbuttonint)();	/* users button interrupt routine */
static int xcenter;
static int ycenter;

#define MOUSE_MODE 04

_Buttonint (row, col, button)
{

	switch (button)
	{
		case 01: button = 3; break;
		case 02: button = 2; break;
		case 04: button = 1; break;
		default: button = 0; break;
	}
	userbuttonint (button);
}

Mouse_location (x, y)
	int *x ;
	int *y ;
{

	mgigetcursxy(0, MOUSE_MODE, x, y) ;
}


Mouse_reference_location (x, y)

	int *x;
	int *y;
{
	mgicursmode(0);
	mgicursxy(0, MOUSE_MODE, *x = xcenter, *y = ycenter) ;
	mgicursmode(MOUSE_MODE);
}

Mouse_on (buttonint)

	int (*buttonint)();
{
	int _Buttonint();

	mgicursmode(MOUSE_MODE) ;
	userbuttonint = buttonint;
	mgibuttonint(_Buttonint) ;
}

Mouse_init()
{
	int left, right, top, bottom, dummy;

	/*
	mgicursmode(MOUSE_MODE) ;
	*/

	mgigetvcoor (0, &left, &bottom, &right, &top, &dummy) ;
	xcenter = (left+right)/2;
	ycenter = (top+bottom)/2;
}

Mouse_off ()
{
	mgibuttonint(0);
	/* may need to reset cursor mode */
}
