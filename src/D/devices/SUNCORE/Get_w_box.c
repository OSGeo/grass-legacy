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
 * A "rubberband" box is used.  One corner is fixed at the (cx, cy) coordinate.
 * The opposite coordinate starts out at (*wx, *wy) and then tracks the mouse.
 * Upon button depression, the current coordinate is returned in (*wx, *wy) and
 * the button pressed in returned in *button.
 */

#include <usercore.h> 
extern int SCREEN_BOTTOM ;

Get_location_with_box(cx, cy, nx, ny, button)
	int cx, cy ;    /* current x and y */
	int *nx, *ny ;  /* new x and y */
	int *button ;
{
	float wcx, wcy ;  /* world current x and y */
	float wnx, wny ;  /* world new x and y */
	float ncx, ncy ;  /* normalized current x and y */
	float nnx, nny ;  /* normalized new x and y */
	float x, y ;

	wcx = (float)cx ;
	wcy = (float)(SCREEN_BOTTOM - cy) ;
	wnx = (float)*nx ;
	wny = (float)(SCREEN_BOTTOM - *ny) ;

	map_world_to_ndc_2(wcx, wcy, &ncx, &ncy) ;
	map_world_to_ndc_2(wnx, wny,  &nnx,  &nny) ;

	set_echo(LOCATOR, 1, 6) ;
	set_echo_position(LOCATOR, 1, ncx, ncy) ;

	await_any_button_get_locator_2( 1000000000, 1, button, &nnx, &nny);
	map_ndc_to_world_2(nnx, nny, &wnx, &wny) ;

	*nx = (int)wnx ;
	*ny = SCREEN_BOTTOM - (int)wny ;
}
