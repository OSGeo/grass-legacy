#include "gis.h"
#include "lproto.h"
int tell_em_to_use_mouse()
{
	G_clear_screen() ;
	fprintf (stdout,"\n\n   Please use mouse to choose option\n") ;

	return 0;
}
