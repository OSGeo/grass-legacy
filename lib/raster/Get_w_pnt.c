#include "gis.h"
#include "raster.h"
#include "graph.h"

int R_get_location_with_pointer(int *wx,int *wy,int *button )
{
	int z, ret;
        G_debug (4, "R_get_location_with_pointer()"); 
	
	*button = 0; /* ?, how button = -1 is used (see driver) */
	R_set_cancel (0);
	    
	_send_ident(GET_LOCATION_WITH_POINTER) ;
	z = *wx ;
	_send_int(&z) ;
	z = *wy ;
	_send_int(&z) ;
	z = *button ;
	_send_int(&z) ;

	while ( 1 ) {
	    _get_int(wx) ;
	    _get_int(wy) ;
	    _get_int(button) ;
	    
            G_debug (4, "button = %d wx = %d  wy = %d", *button, *wx, *wy); 

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
        G_debug (4, "button = %d wx = %d  wy = %d", *button, *wx, *wy); 

	R_flush();
	R_set_cancel ( 0 ); 
	R_set_update_function ( NULL );
	
	return 0;
}
