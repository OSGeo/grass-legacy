#include "graph.h"

R_screen_left()
{
	int anint ;
	_send_ident(SCREEN_LEFT) ;
	_get_int(&anint) ;
	return(anint) ;
}

R_screen_rite()
{
	int anint ;
	_send_ident(SCREEN_RITE) ;
	_get_int(&anint) ;
	return(anint) ;
}

R_screen_bot()
{
	int anint ;
	_send_ident(SCREEN_BOT) ;
	_get_int(&anint) ;
	return(anint) ;
}

R_screen_top()
{
	int anint ;
	_send_ident(SCREEN_TOP) ;
	_get_int(&anint) ;
	return(anint) ;
}
