/*
 * Using mouse device, get a new screen coordinate and button number.
 * Button numbers must be the following values which correspond to the
 * following software meanings:
 *   1 - left button
 *   2 - middle button
 *   3 - right button
 *
 * This is called directly by the application programs.
 *
 * A pointer is used. This can be a crosshair, pointer, or cursor. 
 * It starts out at (*wx, *wy) and then tracks the mouse.
 * Upon button depression, the current coordinate is returned in (*wx, *wy) and
 * the button pressed in returned in *button.
 */


#include <usercore.h> 
extern int SCREEN_BOTTOM ;

Get_location_with_pointer(wx, wy, button)
	int *wx, *wy ;
	int *button ;
{
	float fwx, fwy ;
	float x, y ;

	fwx = (float)*wx ;
	fwy = (float)(SCREEN_BOTTOM - *wy) ;

	map_world_to_ndc_2(fwx, fwy, &x, &y) ;
	set_locator_2(1, x, y) ;
	set_echo_position(LOCATOR, 1, x, y) ;
	set_echo(LOCATOR, 1, 1) ;

	await_any_button_get_locator_2( 1000000000, 1, button, &x, &y);
	map_ndc_to_world_2(x, y, &fwx, &fwy) ;

	*wx = (int)fwx ;
	*wy = SCREEN_BOTTOM - (int)fwy ;
}
