/*
 * Close down the graphics processing.  This gets called only at driver
 * termination time.
 */

#include <usercore.h>

Graph_Close()
{
	extern struct vwsurf *suncolor ;
	close_temporary_segment() ;
	deselect_view_surface(suncolor) ;
	terminate_core() ;
	return(0) ;
}
