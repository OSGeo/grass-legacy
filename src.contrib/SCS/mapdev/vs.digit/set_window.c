/*  @(#)set_window.c	2.1  6/26/87  */
/**  Re-Written by Ron Glenn  12/1991
**  USDA Tech. Infor. Sys. Division
*/
#include "digit.h"
#include "dig_head.h"

#define BEEP putchar ('\007')

init_window ()
{
/*DEBUG*/ debugf ("Seting window to default\n");
	window_rout (CM->head.N, CM->head.S, CM->head.E, CM->head.W) ;
}

set_window()
{
	set_window_w_mouse ();
	return (0);
}
