#include "gis.h"
#include "raster.h"
#include "graph.h"

/*!
 * \brief get mouse location using a box
 *
 * Identical to <i>R_get_location_with_line</i> except a rubber-band box is
 * used instead of a rubber-band line.
 * <b>button</b> is set to: 1 - left, 2 - middle, 3 - right 
 *
 *  \param cx
 *  \param cy
 *  \param wx
 *  \param wy
 *  \param button
 *  \return 0 function was canceled by R_set_cancel (1)
 */


int R_get_location_with_box(
	int cx,int cy,
	int *wx,int *wy,
	int *button)
{
	int z, ret;
	R_set_cancel ( 0 ); 

	_send_ident(GET_LOCATION_WITH_BOX) ;
	z = cx ;
	_send_int(&z) ;
	z = cy ;
	_send_int(&z) ;
	z = *wx ;
	_send_int(&z) ;
	z = *wy ;
	_send_int(&z) ;

	while ( 1 ) {
	    _get_int(wx) ;
	    _get_int(wy) ;
	    _get_int(button) ;
	    
            G_debug (5, "button = %d wx = %d  wy = %d", *button, *wx, *wy); 

	    if ( *button > 0 ) break;

	    /* call user function */
	    R_call_update_function ( *wx, *wy  );
	    
	    /* check if continue or stop */ 
	    ret = R_get_cancel ();
	    if ( ret ) { 
	       	z = 1;
                _send_int(&z) ;
		*button = 0;
		break;
	    } else  { 
		z = 0; 
                _send_int(&z) ;
	    }
	}
        G_debug (3, "button = %d wx = %d  wy = %d", *button, *wx, *wy); 

	R_flush();
	R_set_cancel ( 0 ); 
        R_set_update_function ( NULL );
	
	return 0;
}
